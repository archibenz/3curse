import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import javax.imageio.ImageIO;

public class Main {
    private static final int PORT_START = 8080;
    private static final int PORT_END = 8095;

    public static void main(String[] args) throws Exception {
        HttpServer server = null;
        int port = -1;
        for (int candidate = PORT_START; candidate <= PORT_END; candidate++) {
            try {
                server = HttpServer.create(new InetSocketAddress(candidate), 0);
                port = candidate;
                break;
            } catch (IOException ignored) {}
        }
        if (server == null) {
            throw new IOException("Нет свободного порта в диапазоне " + PORT_START + "-" + PORT_END);
        }
        
        server.createContext("/api/maze/generate", new MazeGenerateHandler());
        server.createContext("/api/maze/path", new PathFindHandler());
        server.createContext("/api/maze/export", new ExportHandler());
        server.createContext("/api/benchmarks", new BenchmarkHandler());
        server.createContext("/", new StaticHandler());
        server.setExecutor(Executors.newFixedThreadPool(10));
        
        System.out.println("╔════════════════════════════════════════════════════════════╗");
        System.out.println("║         Maze Web Server - DAGA Project                     ║");
        System.out.println("╠════════════════════════════════════════════════════════════╣");
        System.out.println("║  Server started on: http://localhost:" + port + "                  ║");
        System.out.println("║  Open this URL in your browser                             ║");
        System.out.println("╚════════════════════════════════════════════════════════════╝");
        server.start();
    }

    // ==================== MAZE DATA STRUCTURES ====================
    
    static class Cell {
        int wallMask = 15; // Up=1, Down=2, Left=4, Right=8
        volatile int inUse = 0;
        int x, y;
        
        Cell(int x, int y) {
            this.x = x;
            this.y = y;
        }
    }
    
    static class Maze {
        Cell[][] cells;
        int height, width;
        int[] start = new int[2];
        int[] end = new int[2];
        List<int[]> exits = new ArrayList<>();
        
        static final int UP = 1, DOWN = 2, LEFT = 4, RIGHT = 8;
        
        Maze(int height, int width) {
            this.height = height;
            this.width = width;
            cells = new Cell[height][width];
            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    cells[i][j] = new Cell(i, j);
                }
            }
            setRandomStartEnd();
        }
        
        void setRandomStartEnd() {
            Random rand = new Random();
            int side = rand.nextInt(4);
            switch (side) {
                case 0: start = new int[]{0, rand.nextInt(width)}; break;
                case 1: start = new int[]{height - 1, rand.nextInt(width)}; break;
                case 2: start = new int[]{rand.nextInt(height), 0}; break;
                case 3: start = new int[]{rand.nextInt(height), width - 1}; break;
            }
            do {
                side = rand.nextInt(4);
                switch (side) {
                    case 0: end = new int[]{0, rand.nextInt(width)}; break;
                    case 1: end = new int[]{height - 1, rand.nextInt(width)}; break;
                    case 2: end = new int[]{rand.nextInt(height), 0}; break;
                    case 3: end = new int[]{rand.nextInt(height), width - 1}; break;
                }
            } while (Arrays.equals(start, end));
            exits.add(end.clone());
        }
        
        boolean isValid(int x, int y) {
            return x >= 0 && x < height && y >= 0 && y < width;
        }
        
        int getAvailableNeighbors(int x, int y) {
            int dirs = 0;
            if (isValid(x - 1, y) && cells[x - 1][y].inUse == 0) dirs |= UP;
            if (isValid(x + 1, y) && cells[x + 1][y].inUse == 0) dirs |= DOWN;
            if (isValid(x, y - 1) && cells[x][y - 1].inUse == 0) dirs |= LEFT;
            if (isValid(x, y + 1) && cells[x][y + 1].inUse == 0) dirs |= RIGHT;
            return dirs;
        }
        
        int chooseRandomDirection(int dirs) {
            if (dirs == 0) return 0;
            List<Integer> available = new ArrayList<>();
            if ((dirs & UP) != 0) available.add(UP);
            if ((dirs & DOWN) != 0) available.add(DOWN);
            if ((dirs & LEFT) != 0) available.add(LEFT);
            if ((dirs & RIGHT) != 0) available.add(RIGHT);
            return available.get(new Random().nextInt(available.size()));
        }
        
        int[] move(int x, int y, int dir) {
            switch (dir) {
                case UP: return new int[]{x - 1, y};
                case DOWN: return new int[]{x + 1, y};
                case LEFT: return new int[]{x, y - 1};
                case RIGHT: return new int[]{x, y + 1};
            }
            return new int[]{x, y};
        }
        
        int opposite(int dir) {
            switch (dir) {
                case UP: return DOWN;
                case DOWN: return UP;
                case LEFT: return RIGHT;
                case RIGHT: return LEFT;
            }
            return 0;
        }
        
        // Classic DFS Backtracking
        void generateClassic() {
            Deque<Cell> stack = new ArrayDeque<>();
            cells[start[0]][start[1]].inUse = 1;
            stack.push(cells[start[0]][start[1]]);
            
            while (!stack.isEmpty()) {
                Cell current = stack.peek();
                int dirs = getAvailableNeighbors(current.x, current.y);
                
                if (dirs == 0) {
                    stack.pop();
                    continue;
                }
                
                int dir = chooseRandomDirection(dirs);
                current.wallMask &= ~dir;
                
                int[] next = move(current.x, current.y, dir);
                Cell nextCell = cells[next[0]][next[1]];
                nextCell.wallMask &= ~opposite(dir);
                nextCell.inUse = 1;
                stack.push(nextCell);
            }
        }
        
        // Imperfect maze with extra loops
        void generateImperfect(int extraLoops) {
            Deque<Cell> stack = new ArrayDeque<>();
            cells[start[0]][start[1]].inUse = 1;
            stack.push(cells[start[0]][start[1]]);
            Random rand = new Random();
            
            while (!stack.isEmpty()) {
                Cell current = stack.peek();
                int x = current.x, y = current.y;
                int dirs = getAvailableNeighbors(x, y);
                
                if (dirs == 0) {
                    if (extraLoops > 0) {
                        int visited = 0;
                        if (isValid(x - 1, y) && cells[x - 1][y].inUse != 0 && (current.wallMask & UP) != 0) visited |= UP;
                        if (isValid(x + 1, y) && cells[x + 1][y].inUse != 0 && (current.wallMask & DOWN) != 0) visited |= DOWN;
                        if (isValid(x, y - 1) && cells[x][y - 1].inUse != 0 && (current.wallMask & LEFT) != 0) visited |= LEFT;
                        if (isValid(x, y + 1) && cells[x][y + 1].inUse != 0 && (current.wallMask & RIGHT) != 0) visited |= RIGHT;
                        
                        if (visited != 0 && rand.nextDouble() < 0.35) {
                            int dir = chooseRandomDirection(visited);
                            current.wallMask &= ~dir;
                            int[] next = move(x, y, dir);
                            cells[next[0]][next[1]].wallMask &= ~opposite(dir);
                            extraLoops--;
                        }
                    }
                    stack.pop();
                    continue;
                }
                
                int dir = chooseRandomDirection(dirs);
                current.wallMask &= ~dir;
                int[] next = move(x, y, dir);
                Cell nextCell = cells[next[0]][next[1]];
                nextCell.wallMask &= ~opposite(dir);
                nextCell.inUse = 1;
                stack.push(nextCell);
            }
        }
        
        // Multithreaded generation
        void generateMultithreaded(int numThreads) {
            List<int[]> startPoints = generateStartPoints(numThreads);
            ExecutorService executor = Executors.newFixedThreadPool(numThreads);
            CyclicBarrier barrier = new CyclicBarrier(numThreads);
            AtomicInteger activeThreads = new AtomicInteger(numThreads);
            
            for (int i = 0; i < numThreads; i++) {
                final int threadId = i + 1;
                final int[] sp = startPoints.get(i);
                executor.submit(() -> {
                    try {
                        barrier.await();
                        generateFromPoint(sp, threadId, activeThreads);
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                });
            }
            
            executor.shutdown();
            try {
                executor.awaitTermination(30, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
            
            connectRegions();
        }
        
        List<int[]> generateStartPoints(int count) {
            List<int[]> points = new ArrayList<>();
            List<int[]> corners = Arrays.asList(
                new int[]{0, 0},
                new int[]{0, width - 1},
                new int[]{height - 1, 0},
                new int[]{height - 1, width - 1}
            );
            
            points.add(start.clone());
            for (int[] corner : corners) {
                if (points.size() >= count) break;
                if (!Arrays.equals(corner, start)) {
                    points.add(corner);
                }
            }
            
            Random rand = new Random();
            while (points.size() < count) {
                int[] point = new int[]{rand.nextInt(height), rand.nextInt(width)};
                boolean tooClose = false;
                for (int[] p : points) {
                    if (Math.abs(p[0] - point[0]) + Math.abs(p[1] - point[1]) < 3) {
                        tooClose = true;
                        break;
                    }
                }
                if (!tooClose) points.add(point);
            }
            
            return points.subList(0, count);
        }
        
        synchronized void generateFromPoint(int[] sp, int threadId, AtomicInteger activeThreads) {
            Deque<Cell> stack = new ArrayDeque<>();
            synchronized (cells[sp[0]][sp[1]]) {
                cells[sp[0]][sp[1]].inUse = threadId;
            }
            stack.push(cells[sp[0]][sp[1]]);
            
            while (!stack.isEmpty()) {
                Cell current = stack.peek();
                int x = current.x, y = current.y;
                
                int dirs = 0;
                synchronized (this) {
                    if (isValid(x - 1, y) && cells[x - 1][y].inUse == 0) dirs |= UP;
                    if (isValid(x + 1, y) && cells[x + 1][y].inUse == 0) dirs |= DOWN;
                    if (isValid(x, y - 1) && cells[x][y - 1].inUse == 0) dirs |= LEFT;
                    if (isValid(x, y + 1) && cells[x][y + 1].inUse == 0) dirs |= RIGHT;
                }
                
                if (dirs == 0) {
                    stack.pop();
                    continue;
                }
                
                int dir = chooseRandomDirection(dirs);
                int[] next = move(x, y, dir);
                
                synchronized (this) {
                    if (cells[next[0]][next[1]].inUse == 0) {
                        current.wallMask &= ~dir;
                        cells[next[0]][next[1]].wallMask &= ~opposite(dir);
                        cells[next[0]][next[1]].inUse = threadId;
                        stack.push(cells[next[0]][next[1]]);
                    }
                }
            }
            activeThreads.decrementAndGet();
        }
        
        void connectRegions() {
            // Find all regions and connect them
            int[][] regionMap = new int[height][width];
            int regionCount = 0;
            
            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    if (regionMap[i][j] == 0 && cells[i][j].inUse != 0) {
                        regionCount++;
                        floodFill(regionMap, i, j, regionCount);
                    }
                }
            }
            
            // Connect adjacent regions by removing walls
            Random rand = new Random();
            for (int i = 0; i < height; i++) {
                for (int j = 0; j < width; j++) {
                    if (regionMap[i][j] > 0) {
                        if (isValid(i + 1, j) && regionMap[i + 1][j] > 0 && regionMap[i][j] != regionMap[i + 1][j]) {
                            if (rand.nextDouble() < 0.3) {
                                cells[i][j].wallMask &= ~DOWN;
                                cells[i + 1][j].wallMask &= ~UP;
                            }
                        }
                        if (isValid(i, j + 1) && regionMap[i][j + 1] > 0 && regionMap[i][j] != regionMap[i][j + 1]) {
                            if (rand.nextDouble() < 0.3) {
                                cells[i][j].wallMask &= ~RIGHT;
                                cells[i][j + 1].wallMask &= ~LEFT;
                            }
                        }
                    }
                }
            }
        }
        
        void floodFill(int[][] map, int x, int y, int region) {
            Deque<int[]> queue = new ArrayDeque<>();
            queue.add(new int[]{x, y});
            
            while (!queue.isEmpty()) {
                int[] pos = queue.poll();
                int px = pos[0], py = pos[1];
                
                if (!isValid(px, py) || map[px][py] != 0 || cells[px][py].inUse == 0) continue;
                
                map[px][py] = region;
                
                if ((cells[px][py].wallMask & UP) == 0) queue.add(new int[]{px - 1, py});
                if ((cells[px][py].wallMask & DOWN) == 0) queue.add(new int[]{px + 1, py});
                if ((cells[px][py].wallMask & LEFT) == 0) queue.add(new int[]{px, py - 1});
                if ((cells[px][py].wallMask & RIGHT) == 0) queue.add(new int[]{px, py + 1});
            }
        }
        
        // Add border exits
        List<int[]> addBorderExits(int count) {
            List<int[]> border = new ArrayList<>();
            for (int i = 0; i < height; i++) {
                border.add(new int[]{i, 0});
                border.add(new int[]{i, width - 1});
            }
            for (int j = 1; j < width - 1; j++) {
                border.add(new int[]{0, j});
                border.add(new int[]{height - 1, j});
            }
            border.removeIf(p -> Arrays.equals(p, start));
            Collections.shuffle(border);
            
            List<int[]> newExits = new ArrayList<>();
            int minDist = Math.max(width, height) / Math.max(count, 1);
            
            for (int[] cell : border) {
                if (newExits.size() >= count) break;
                boolean good = true;
                for (int[] ex : newExits) {
                    if (Math.abs(cell[0] - ex[0]) + Math.abs(cell[1] - ex[1]) < minDist) {
                        good = false;
                        break;
                    }
                }
                if (good) {
                    newExits.add(cell);
                    int x = cell[0], y = cell[1];
                    if (x == 0) cells[x][y].wallMask &= ~UP;
                    else if (x == height - 1) cells[x][y].wallMask &= ~DOWN;
                    else if (y == 0) cells[x][y].wallMask &= ~LEFT;
                    else cells[x][y].wallMask &= ~RIGHT;
                }
            }
            
            exits.clear();
            exits.add(end.clone());
            exits.addAll(newExits);
            return newExits;
        }
        
        // BFS pathfinding
        List<int[]> findPath(int[] from, int[] to) {
            if (!isValid(from[0], from[1]) || !isValid(to[0], to[1])) {
                return Collections.emptyList();
            }
            
            int[][] distance = new int[height][width];
            for (int[] row : distance) Arrays.fill(row, -1);
            int[][][] prev = new int[height][width][2];
            
            Deque<int[]> queue = new ArrayDeque<>();
            queue.add(from);
            distance[from[0]][from[1]] = 0;
            
            int[] dx = {-1, 1, 0, 0};
            int[] dy = {0, 0, -1, 1};
            int[] dirMask = {UP, DOWN, LEFT, RIGHT};
            
            while (!queue.isEmpty()) {
                int[] curr = queue.poll();
                int cx = curr[0], cy = curr[1];
                
                if (cx == to[0] && cy == to[1]) break;
                
                for (int i = 0; i < 4; i++) {
                    int nx = cx + dx[i], ny = cy + dy[i];
                    
                    if (isValid(nx, ny) && distance[nx][ny] == -1) {
                        if ((cells[cx][cy].wallMask & dirMask[i]) == 0) {
                            distance[nx][ny] = distance[cx][cy] + 1;
                            prev[nx][ny] = new int[]{cx, cy};
                            queue.add(new int[]{nx, ny});
                        }
                    }
                }
            }
            
            if (distance[to[0]][to[1]] == -1) return Collections.emptyList();
            
            List<int[]> path = new ArrayList<>();
            int[] curr = to;
            while (!(curr[0] == from[0] && curr[1] == from[1])) {
                path.add(curr);
                curr = prev[curr[0]][curr[1]];
            }
            path.add(from);
            Collections.reverse(path);
            return path;
        }
        
        // Convert to JSON
        String toJson() {
            StringBuilder sb = new StringBuilder();
            sb.append("{");
            sb.append("\"width\":").append(width).append(",");
            sb.append("\"height\":").append(height).append(",");
            sb.append("\"start\":[").append(start[0]).append(",").append(start[1]).append("],");
            sb.append("\"end\":[").append(end[0]).append(",").append(end[1]).append("],");
            sb.append("\"exits\":[");
            for (int i = 0; i < exits.size(); i++) {
                if (i > 0) sb.append(",");
                sb.append("[").append(exits.get(i)[0]).append(",").append(exits.get(i)[1]).append("]");
            }
            sb.append("],");
            sb.append("\"cells\":[");
            for (int i = 0; i < height; i++) {
                if (i > 0) sb.append(",");
                sb.append("[");
                for (int j = 0; j < width; j++) {
                    if (j > 0) sb.append(",");
                    sb.append(cells[i][j].wallMask);
                }
                sb.append("]");
            }
            sb.append("]}");
            return sb.toString();
        }
        
        // Convert path to JSON
        static String pathToJson(List<int[]> path) {
            StringBuilder sb = new StringBuilder();
            sb.append("[");
            for (int i = 0; i < path.size(); i++) {
                if (i > 0) sb.append(",");
                sb.append("[").append(path.get(i)[0]).append(",").append(path.get(i)[1]).append("]");
            }
            sb.append("]");
            return sb.toString();
        }
    }
    
    // ==================== HTTP HANDLERS ====================
    
    static class MazeGenerateHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, 0);
                exchange.close();
                return;
            }
            
            String body = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
            Map<String, String> params = parseJson(body);
            
            int width = Integer.parseInt(params.getOrDefault("width", "20"));
            int height = Integer.parseInt(params.getOrDefault("height", "20"));
            String algorithm = params.getOrDefault("algorithm", "classic");
            int extraLoops = Integer.parseInt(params.getOrDefault("extraLoops", "0"));
            int threads = Integer.parseInt(params.getOrDefault("threads", "4"));
            int exitCount = Integer.parseInt(params.getOrDefault("exits", "1"));
            
            width = Math.max(5, Math.min(100, width));
            height = Math.max(5, Math.min(100, height));
            
            long startTime = System.currentTimeMillis();
            Maze maze = new Maze(height, width);
            
            switch (algorithm) {
                case "imperfect":
                    maze.generateImperfect(extraLoops > 0 ? extraLoops : (width * height / 10));
                    break;
                case "multithread":
                    maze.generateMultithreaded(Math.max(2, Math.min(16, threads)));
                    break;
                default:
                    maze.generateClassic();
            }
            
            if (exitCount > 1) {
                maze.addBorderExits(exitCount - 1);
            }
            
            long genTime = System.currentTimeMillis() - startTime;
            
            List<int[]> path = maze.findPath(maze.start, maze.end);
            
            String json = "{\"maze\":" + maze.toJson() + 
                         ",\"path\":" + Maze.pathToJson(path) + 
                         ",\"generationTime\":" + genTime + "}";
            
            writeJson(exchange, json);
        }
    }
    
    static class PathFindHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, 0);
                exchange.close();
                return;
            }
            
            String body = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
            Map<String, String> params = parseJson(body);
            
            // This endpoint expects maze data in the request
            // For simplicity, we'll regenerate based on stored state
            // In production, you'd parse the full maze from the request
            
            String response = "{\"path\":[]}";
            writeJson(exchange, response);
        }
    }
    
    static class ExportHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, 0);
                exchange.close();
                return;
            }
            
            String body = new String(exchange.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
            Map<String, String> params = parseJson(body);
            
            String format = params.getOrDefault("format", "png").toLowerCase();
            int width = Integer.parseInt(params.getOrDefault("width", "20"));
            int height = Integer.parseInt(params.getOrDefault("height", "20"));
            int cellSize = Integer.parseInt(params.getOrDefault("cellSize", "20"));
            String mazeData = params.getOrDefault("cells", "");
            String pathData = params.getOrDefault("path", "");
            String startData = params.getOrDefault("start", "[0,0]");
            String endData = params.getOrDefault("end", "[0,0]");
            String exitsData = params.getOrDefault("exits", "[]");
            boolean showPath = "true".equals(params.getOrDefault("showPath", "true"));
            boolean showGrid = "true".equals(params.getOrDefault("showGrid", "true"));
            String wallColor = params.getOrDefault("wallColor", "#FFFFFF");
            String bgColor = params.getOrDefault("bgColor", "#1a1a2e");
            String pathColor = params.getOrDefault("pathColor", "#5dd4ff");
            
            if (format.equals("json")) {
                String json = "{\"width\":" + width + ",\"height\":" + height + 
                             ",\"cells\":" + mazeData + ",\"start\":" + startData + 
                             ",\"end\":" + endData + ",\"exits\":" + exitsData + 
                             ",\"path\":" + pathData + "}";
                writeJson(exchange, json);
                return;
            }
            
            if (format.equals("svg")) {
                String svg = generateSVG(width, height, cellSize, mazeData, pathData, 
                                        startData, endData, exitsData, showPath, showGrid,
                                        wallColor, bgColor, pathColor);
                byte[] data = svg.getBytes(StandardCharsets.UTF_8);
                exchange.getResponseHeaders().set("Content-Type", "image/svg+xml");
                exchange.getResponseHeaders().set("Content-Disposition", "attachment; filename=maze.svg");
                exchange.sendResponseHeaders(200, data.length);
                try (OutputStream os = exchange.getResponseBody()) {
                    os.write(data);
                }
                return;
            }
            
            // PNG or JPEG
            BufferedImage image = generateImage(width, height, cellSize, mazeData, pathData,
                                               startData, endData, exitsData, showPath, showGrid,
                                               wallColor, bgColor, pathColor);
            
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            String contentType;
            String filename;
            
            if (format.equals("jpeg") || format.equals("jpg")) {
                BufferedImage rgbImage = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_INT_RGB);
                Graphics2D g = rgbImage.createGraphics();
                g.setColor(Color.decode(bgColor));
                g.fillRect(0, 0, rgbImage.getWidth(), rgbImage.getHeight());
                g.drawImage(image, 0, 0, null);
                g.dispose();
                ImageIO.write(rgbImage, "JPEG", baos);
                contentType = "image/jpeg";
                filename = "maze.jpg";
            } else {
                ImageIO.write(image, "PNG", baos);
                contentType = "image/png";
                filename = "maze.png";
            }
            
            byte[] imageData = baos.toByteArray();
            exchange.getResponseHeaders().set("Content-Type", contentType);
            exchange.getResponseHeaders().set("Content-Disposition", "attachment; filename=" + filename);
            exchange.sendResponseHeaders(200, imageData.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(imageData);
            }
        }
        
        BufferedImage generateImage(int mazeWidth, int mazeHeight, int cellSize, 
                                   String cellsJson, String pathJson, String startJson, 
                                   String endJson, String exitsJson, boolean showPath, 
                                   boolean showGrid, String wallColor, String bgColor, String pathColor) {
            int imgWidth = mazeWidth * cellSize + 2;
            int imgHeight = mazeHeight * cellSize + 2;
            BufferedImage image = new BufferedImage(imgWidth, imgHeight, BufferedImage.TYPE_INT_ARGB);
            Graphics2D g = image.createGraphics();
            g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            
            // Background
            g.setColor(Color.decode(bgColor));
            g.fillRect(0, 0, imgWidth, imgHeight);
            
            // Parse cells
            int[][] cells = parseCells(cellsJson, mazeHeight, mazeWidth);
            int[] start = parsePoint(startJson);
            int[] end = parsePoint(endJson);
            List<int[]> exits = parsePointList(exitsJson);
            List<int[]> path = parsePointList(pathJson);
            
            // Draw grid
            if (showGrid) {
                g.setColor(new Color(50, 56, 70));
                for (int i = 0; i <= mazeHeight; i++) {
                    g.drawLine(1, 1 + i * cellSize, 1 + mazeWidth * cellSize, 1 + i * cellSize);
                }
                for (int j = 0; j <= mazeWidth; j++) {
                    g.drawLine(1 + j * cellSize, 1, 1 + j * cellSize, 1 + mazeHeight * cellSize);
                }
            }
            
            // Draw start cell
            g.setColor(new Color(70, 200, 120, 160));
            g.fillRect(1 + start[1] * cellSize, 1 + start[0] * cellSize, cellSize, cellSize);
            
            // Draw exit cells
            g.setColor(new Color(220, 80, 80, 160));
            for (int[] exit : exits) {
                g.fillRect(1 + exit[1] * cellSize, 1 + exit[0] * cellSize, cellSize, cellSize);
            }
            
            // Draw path
            if (showPath && path.size() > 1) {
                g.setColor(Color.decode(pathColor));
                g.setStroke(new BasicStroke(Math.max(2, cellSize / 4), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
                for (int i = 0; i < path.size() - 1; i++) {
                    int x1 = 1 + path.get(i)[1] * cellSize + cellSize / 2;
                    int y1 = 1 + path.get(i)[0] * cellSize + cellSize / 2;
                    int x2 = 1 + path.get(i + 1)[1] * cellSize + cellSize / 2;
                    int y2 = 1 + path.get(i + 1)[0] * cellSize + cellSize / 2;
                    g.drawLine(x1, y1, x2, y2);
                }
            }
            
            // Draw walls
            g.setColor(Color.decode(wallColor));
            g.setStroke(new BasicStroke(2));
            for (int i = 0; i < mazeHeight; i++) {
                for (int j = 0; j < mazeWidth; j++) {
                    int mask = cells[i][j];
                    int x = 1 + j * cellSize;
                    int y = 1 + i * cellSize;
                    if ((mask & 1) != 0) g.drawLine(x, y, x + cellSize, y); // UP
                    if ((mask & 2) != 0) g.drawLine(x, y + cellSize, x + cellSize, y + cellSize); // DOWN
                    if ((mask & 4) != 0) g.drawLine(x, y, x, y + cellSize); // LEFT
                    if ((mask & 8) != 0) g.drawLine(x + cellSize, y, x + cellSize, y + cellSize); // RIGHT
                }
            }
            
            g.dispose();
            return image;
        }
        
        String generateSVG(int mazeWidth, int mazeHeight, int cellSize,
                         String cellsJson, String pathJson, String startJson,
                         String endJson, String exitsJson, boolean showPath,
                         boolean showGrid, String wallColor, String bgColor, String pathColor) {
            int imgWidth = mazeWidth * cellSize + 2;
            int imgHeight = mazeHeight * cellSize + 2;
            
            int[][] cells = parseCells(cellsJson, mazeHeight, mazeWidth);
            int[] start = parsePoint(startJson);
            int[] end = parsePoint(endJson);
            List<int[]> exits = parsePointList(exitsJson);
            List<int[]> path = parsePointList(pathJson);
            
            StringBuilder svg = new StringBuilder();
            svg.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
            svg.append("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"").append(imgWidth)
               .append("\" height=\"").append(imgHeight).append("\">\n");
            
            // Background
            svg.append("<rect width=\"100%\" height=\"100%\" fill=\"").append(bgColor).append("\"/>\n");
            
            // Grid
            if (showGrid) {
                svg.append("<g stroke=\"#32384a\" stroke-width=\"1\">\n");
                for (int i = 0; i <= mazeHeight; i++) {
                    svg.append("<line x1=\"1\" y1=\"").append(1 + i * cellSize)
                       .append("\" x2=\"").append(1 + mazeWidth * cellSize)
                       .append("\" y2=\"").append(1 + i * cellSize).append("\"/>\n");
                }
                for (int j = 0; j <= mazeWidth; j++) {
                    svg.append("<line x1=\"").append(1 + j * cellSize)
                       .append("\" y1=\"1\" x2=\"").append(1 + j * cellSize)
                       .append("\" y2=\"").append(1 + mazeHeight * cellSize).append("\"/>\n");
                }
                svg.append("</g>\n");
            }
            
            // Start cell
            svg.append("<rect x=\"").append(1 + start[1] * cellSize)
               .append("\" y=\"").append(1 + start[0] * cellSize)
               .append("\" width=\"").append(cellSize)
               .append("\" height=\"").append(cellSize)
               .append("\" fill=\"rgba(70,200,120,0.6)\"/>\n");
            
            // Exit cells
            for (int[] exit : exits) {
                svg.append("<rect x=\"").append(1 + exit[1] * cellSize)
                   .append("\" y=\"").append(1 + exit[0] * cellSize)
                   .append("\" width=\"").append(cellSize)
                   .append("\" height=\"").append(cellSize)
                   .append("\" fill=\"rgba(220,80,80,0.6)\"/>\n");
            }
            
            // Path
            if (showPath && path.size() > 1) {
                svg.append("<path d=\"M");
                for (int i = 0; i < path.size(); i++) {
                    int x = 1 + path.get(i)[1] * cellSize + cellSize / 2;
                    int y = 1 + path.get(i)[0] * cellSize + cellSize / 2;
                    if (i > 0) svg.append(" L");
                    svg.append(x).append(" ").append(y);
                }
                svg.append("\" fill=\"none\" stroke=\"").append(pathColor)
                   .append("\" stroke-width=\"").append(Math.max(2, cellSize / 4))
                   .append("\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/>\n");
            }
            
            // Walls
            svg.append("<g stroke=\"").append(wallColor).append("\" stroke-width=\"2\" stroke-linecap=\"round\">\n");
            for (int i = 0; i < mazeHeight; i++) {
                for (int j = 0; j < mazeWidth; j++) {
                    int mask = cells[i][j];
                    int x = 1 + j * cellSize;
                    int y = 1 + i * cellSize;
                    if ((mask & 1) != 0) {
                        svg.append("<line x1=\"").append(x).append("\" y1=\"").append(y)
                           .append("\" x2=\"").append(x + cellSize).append("\" y2=\"").append(y).append("\"/>\n");
                    }
                    if ((mask & 2) != 0) {
                        svg.append("<line x1=\"").append(x).append("\" y1=\"").append(y + cellSize)
                           .append("\" x2=\"").append(x + cellSize).append("\" y2=\"").append(y + cellSize).append("\"/>\n");
                    }
                    if ((mask & 4) != 0) {
                        svg.append("<line x1=\"").append(x).append("\" y1=\"").append(y)
                           .append("\" x2=\"").append(x).append("\" y2=\"").append(y + cellSize).append("\"/>\n");
                    }
                    if ((mask & 8) != 0) {
                        svg.append("<line x1=\"").append(x + cellSize).append("\" y1=\"").append(y)
                           .append("\" x2=\"").append(x + cellSize).append("\" y2=\"").append(y + cellSize).append("\"/>\n");
                    }
                }
            }
            svg.append("</g>\n");
            svg.append("</svg>");
            
            return svg.toString();
        }
        
        int[][] parseCells(String json, int height, int width) {
            int[][] cells = new int[height][width];
            try {
                json = json.trim();
                if (json.startsWith("[") && json.endsWith("]")) {
                    json = json.substring(1, json.length() - 1);
                    String[] rows = json.split("\\],\\[");
                    for (int i = 0; i < Math.min(rows.length, height); i++) {
                        String row = rows[i].replaceAll("[\\[\\]]", "");
                        String[] vals = row.split(",");
                        for (int j = 0; j < Math.min(vals.length, width); j++) {
                            cells[i][j] = Integer.parseInt(vals[j].trim());
                        }
                    }
                }
            } catch (Exception e) {
                // Return default cells with all walls
                for (int[] row : cells) Arrays.fill(row, 15);
            }
            return cells;
        }
        
        int[] parsePoint(String json) {
            try {
                json = json.trim().replaceAll("[\\[\\]]", "");
                String[] parts = json.split(",");
                return new int[]{Integer.parseInt(parts[0].trim()), Integer.parseInt(parts[1].trim())};
            } catch (Exception e) {
                return new int[]{0, 0};
            }
        }
        
        List<int[]> parsePointList(String json) {
            List<int[]> points = new ArrayList<>();
            try {
                json = json.trim();
                if (json.equals("[]")) return points;
                if (json.startsWith("[[")) {
                    json = json.substring(1, json.length() - 1);
                    String[] pointStrs = json.split("\\],\\[");
                    for (String p : pointStrs) {
                        p = p.replaceAll("[\\[\\]]", "");
                        String[] parts = p.split(",");
                        points.add(new int[]{Integer.parseInt(parts[0].trim()), Integer.parseInt(parts[1].trim())});
                    }
                }
            } catch (Exception e) {
                // Return empty list
            }
            return points;
        }
    }
    
    static class BenchmarkHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            if (!"POST".equals(exchange.getRequestMethod()) && !"GET".equals(exchange.getRequestMethod())) {
                exchange.sendResponseHeaders(405, 0);
                exchange.close();
                return;
            }
            
            int[] threadCounts = {1, 2, 4, 6, 8};
            List<Double> syncTimes = new ArrayList<>();
            List<Double> noSyncTimes = new ArrayList<>();
            
            int width = 30, height = 30;
            int testsPerPoint = 3;
            
            for (int threads : threadCounts) {
                double syncTotal = 0, noSyncTotal = 0;
                
                for (int t = 0; t < testsPerPoint; t++) {
                    // Synchronized multithread
                    if (threads > 1) {
                        Maze maze = new Maze(height, width);
                        long start = System.nanoTime();
                        maze.generateMultithreaded(threads);
                        syncTotal += (System.nanoTime() - start) / 1_000_000.0;
                    } else {
                        Maze maze = new Maze(height, width);
                        long start = System.nanoTime();
                        maze.generateClassic();
                        syncTotal += (System.nanoTime() - start) / 1_000_000.0;
                    }
                    
                    // Independent (no sync) - simulate with classic
                    long start = System.nanoTime();
                    for (int i = 0; i < threads; i++) {
                        Maze maze = new Maze(height / threads + 1, width);
                        maze.generateClassic();
                    }
                    noSyncTotal += (System.nanoTime() - start) / 1_000_000.0;
                }
                
                syncTimes.add(syncTotal / testsPerPoint);
                noSyncTimes.add(noSyncTotal / testsPerPoint);
            }
            
            StringBuilder json = new StringBuilder();
            json.append("{\"threads\":[");
            for (int i = 0; i < threadCounts.length; i++) {
                if (i > 0) json.append(",");
                json.append(threadCounts[i]);
            }
            json.append("],\"sync\":[");
            for (int i = 0; i < syncTimes.size(); i++) {
                if (i > 0) json.append(",");
                json.append(String.format("%.2f", syncTimes.get(i)));
            }
            json.append("],\"nosync\":[");
            for (int i = 0; i < noSyncTimes.size(); i++) {
                if (i > 0) json.append(",");
                json.append(String.format("%.2f", noSyncTimes.get(i)));
            }
            json.append("]}");
            
            writeJson(exchange, json.toString());
        }
    }
    
    static class StaticHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            URI uri = exchange.getRequestURI();
            String requested = uri.getPath();
            if (requested.equals("/")) {
                requested = "/index.html";
            }

            Path base = resolveFrontendRoot();
            Path file = base.resolve(requested.substring(1)).normalize();
            if (!file.startsWith(base) || !Files.exists(file)) {
                exchange.sendResponseHeaders(404, 0);
                exchange.close();
                return;
            }

            String contentType = guessContentType(file);
            Headers headers = exchange.getResponseHeaders();
            headers.set("Content-Type", contentType);
            byte[] data = Files.readAllBytes(file);
            exchange.sendResponseHeaders(200, data.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(data);
            }
        }

        private String guessContentType(Path file) {
            String name = file.getFileName().toString().toLowerCase();
            if (name.endsWith(".html")) return "text/html; charset=utf-8";
            if (name.endsWith(".css")) return "text/css; charset=utf-8";
            if (name.endsWith(".js")) return "application/javascript; charset=utf-8";
            if (name.endsWith(".png")) return "image/png";
            if (name.endsWith(".jpg") || name.endsWith(".jpeg")) return "image/jpeg";
            if (name.endsWith(".svg")) return "image/svg+xml";
            if (name.endsWith(".ico")) return "image/x-icon";
            if (name.endsWith(".json")) return "application/json";
            return "application/octet-stream";
        }

        private Path resolveFrontendRoot() {
            Path[] candidates = new Path[] {
                Paths.get("web", "frontend").toAbsolutePath(),
                Paths.get("..", "frontend").toAbsolutePath(),
                Paths.get("frontend").toAbsolutePath()
            };
            for (Path candidate : candidates) {
                if (Files.exists(candidate.resolve("index.html"))) {
                    return candidate.normalize();
                }
            }
            return Paths.get("web", "frontend").toAbsolutePath();
        }
    }

    // ==================== UTILITY METHODS ====================
    
    private static void writeJson(HttpExchange exchange, String json) throws IOException {
        byte[] payload = json.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json; charset=utf-8");
        exchange.getResponseHeaders().set("Access-Control-Allow-Origin", "*");
        exchange.sendResponseHeaders(200, payload.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(payload);
        }
    }
    
    private static Map<String, String> parseJson(String json) {
        Map<String, String> params = new HashMap<>();
        try {
            json = json.trim();
            if (json.startsWith("{") && json.endsWith("}")) {
                json = json.substring(1, json.length() - 1);
                // Simple JSON parser for key-value pairs
                StringBuilder key = new StringBuilder();
                StringBuilder value = new StringBuilder();
                boolean inKey = true;
                boolean inString = false;
                boolean inArray = false;
                int arrayDepth = 0;
                
                for (int i = 0; i < json.length(); i++) {
                    char c = json.charAt(i);
                    
                    if (c == '"' && (i == 0 || json.charAt(i - 1) != '\\')) {
                        inString = !inString;
                        continue;
                    }
                    
                    if (!inString) {
                        if (c == '[') {
                            arrayDepth++;
                            inArray = true;
                        } else if (c == ']') {
                            arrayDepth--;
                            if (arrayDepth == 0) inArray = false;
                        }
                    }
                    
                    if (c == ':' && !inString && !inArray) {
                        inKey = false;
                        continue;
                    }
                    
                    if (c == ',' && !inString && !inArray) {
                        params.put(key.toString().trim(), value.toString().trim());
                        key = new StringBuilder();
                        value = new StringBuilder();
                        inKey = true;
                        continue;
                    }
                    
                    if (inKey) {
                        key.append(c);
                    } else {
                        value.append(c);
                    }
                }
                
                if (key.length() > 0) {
                    params.put(key.toString().trim(), value.toString().trim());
                }
            }
        } catch (Exception e) {
            // Return empty map on parse error
        }
        return params;
    }
}
