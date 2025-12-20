import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Random;

public class Main {
    private static final int PORT = 8080;
    private static final Random RNG = new Random();

    public static void main(String[] args) throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress(PORT), 0);
        server.createContext("/api/maze", new MazeHandler());
        server.createContext("/api/benchmarks", new BenchmarkHandler());
        server.createContext("/", new StaticHandler());
        server.setExecutor(null);
        System.out.println("Web server started on http://localhost:" + PORT);
        server.start();
    }

    private static class MazeHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            int width = 20;
            int height = 20;
            StringBuilder path = new StringBuilder();
            path.append("[");
            for (int i = 0; i < Math.min(width, height); i++) {
                path.append("[").append(i).append(",").append(i).append("]");
                if (i < Math.min(width, height) - 1) {
                    path.append(",");
                }
            }
            path.append("]");

            String json = "{" +
                    "\"width\":" + width + "," +
                    "\"height\":" + height + "," +
                    "\"path\":" + path + "}";
            writeJson(exchange, json);
        }
    }

    private static class BenchmarkHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            String json = "{" +
                    "\"threads\":[1,2,4,6,8]," +
                    "\"sync\":[" + randomList(5, 120, 220) + "]," +
                    "\"nosync\":[" + randomList(5, 150, 280) + "]" +
                    "}";
            writeJson(exchange, json);
        }

        private String randomList(int count, int min, int max) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < count; i++) {
                int val = min + RNG.nextInt(max - min + 1);
                sb.append(val);
                if (i < count - 1) {
                    sb.append(",");
                }
            }
            return sb.toString();
        }
    }

    private static class StaticHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange exchange) throws IOException {
            URI uri = exchange.getRequestURI();
            String requested = uri.getPath();
            if (requested.equals("/")) {
                requested = "/index.html";
            }

            Path base = Paths.get("web", "frontend").toAbsolutePath();
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
            return "application/octet-stream";
        }
    }

    private static void writeJson(HttpExchange exchange, String json) throws IOException {
        byte[] payload = json.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json; charset=utf-8");
        exchange.sendResponseHeaders(200, payload.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(payload);
        }
    }
}
