/**
 * Maze Generator — DAGA Project
 * Complete Web Application
 */

// ========================================
// STATE
// ========================================

const state = {
    maze: null,
    path: [],
    generationTime: 0,
    zoom: 1,
    selectedFormat: 'png'
};

// ========================================
// DOM ELEMENTS
// ========================================

const $ = id => document.getElementById(id);
const $$ = sel => document.querySelectorAll(sel);

const elements = {
    // Tabs
    navBtns: $$('.nav-btn'),
    tabs: $$('.tab'),
    
    // Controls
    algorithmRadios: $$('input[name="algorithm"]'),
    mazeWidth: $('mazeWidth'),
    mazeHeight: $('mazeHeight'),
    threads: $('threads'),
    threadsValue: $('threadsValue'),
    threadsOption: $('threadsOption'),
    extraLoops: $('extraLoops'),
    loopsValue: $('loopsValue'),
    loopsOption: $('loopsOption'),
    exits: $('exits'),
    exitsValue: $('exitsValue'),
    showPath: $('showPath'),
    animatePath: $('animatePath'),
    
    // Buttons
    btnGenerate: $('btnGenerate'),
    btnZoomIn: $('btnZoomIn'),
    btnZoomOut: $('btnZoomOut'),
    btnReset: $('btnReset'),
    btnRunBenchmark: $('btnRunBenchmark'),
    
    // Maze
    mazeCanvas: $('mazeCanvas'),
    mazeContainer: $('mazeContainer'),
    mazePlaceholder: $('mazePlaceholder'),
    
    // Stats
    genTime: $('genTime'),
    mazeSize: $('mazeSize'),
    pathLength: $('pathLength'),
    
    // Export
    formatCards: $$('.format-card'),
    cellSize: $('cellSize'),
    cellSizeValue: $('cellSizeValue'),
    exportShowPath: $('exportShowPath'),
    exportShowGrid: $('exportShowGrid'),
    wallColor: $('wallColor'),
    wallColorText: $('wallColorText'),
    bgColor: $('bgColor'),
    bgColorText: $('bgColorText'),
    pathColorExport: $('pathColorExport'),
    pathColorText: $('pathColorText'),
    exportPreview: $('exportPreview'),
    
    // Benchmark
    benchmarkCanvas: $('benchmarkCanvas'),
    
    // Loading
    loadingOverlay: $('loadingOverlay'),
    toastContainer: $('toastContainer')
};

// ========================================
// INITIALIZATION
// ========================================

function init() {
    setupTabs();
    setupAlgorithmOptions();
    setupRangeSliders();
    setupColorPickers();
    setupExportFormats();
    setupMazeControls();
    setupBenchmark();
    setupResizeHandler();
    
    // Generate initial maze
    generateMaze();
}

// ========================================
// TABS
// ========================================

function setupTabs() {
    elements.navBtns.forEach(btn => {
        btn.addEventListener('click', () => {
            const tabId = btn.dataset.tab;
            
            elements.navBtns.forEach(b => b.classList.remove('active'));
            elements.tabs.forEach(t => t.classList.remove('active'));
            
            btn.classList.add('active');
            document.getElementById(tabId).classList.add('active');
        });
    });
}

// ========================================
// ALGORITHM OPTIONS
// ========================================

function setupAlgorithmOptions() {
    elements.algorithmRadios.forEach(radio => {
        radio.addEventListener('change', () => {
            const algo = radio.value;
            elements.threadsOption.style.display = algo === 'multithread' ? 'block' : 'none';
            elements.loopsOption.style.display = algo === 'imperfect' ? 'block' : 'none';
        });
    });
}

// ========================================
// RANGE SLIDERS
// ========================================

function setupRangeSliders() {
    const sliders = [
        { slider: elements.threads, display: elements.threadsValue },
        { slider: elements.extraLoops, display: elements.loopsValue },
        { slider: elements.exits, display: elements.exitsValue },
        { slider: elements.cellSize, display: elements.cellSizeValue }
    ];
    
    sliders.forEach(({ slider, display }) => {
        if (slider && display) {
            slider.addEventListener('input', () => {
                display.textContent = slider.value;
            });
        }
    });
}

// ========================================
// COLOR PICKERS
// ========================================

function setupColorPickers() {
    const colorPairs = [
        { picker: elements.wallColor, text: elements.wallColorText },
        { picker: elements.bgColor, text: elements.bgColorText },
        { picker: elements.pathColorExport, text: elements.pathColorText }
    ];
    
    colorPairs.forEach(({ picker, text }) => {
        if (picker && text) {
            picker.addEventListener('input', () => {
                text.value = picker.value.toUpperCase();
            });
            text.addEventListener('input', () => {
                if (/^#[0-9A-Fa-f]{6}$/.test(text.value)) {
                    picker.value = text.value;
                }
            });
        }
    });
}

// ========================================
// EXPORT FORMATS
// ========================================

function setupExportFormats() {
    elements.formatCards.forEach(card => {
        card.addEventListener('click', () => {
            elements.formatCards.forEach(c => c.classList.remove('selected'));
            card.classList.add('selected');
            state.selectedFormat = card.dataset.format;
            exportMaze(state.selectedFormat);
        });
    });
}

// ========================================
// MAZE CONTROLS
// ========================================

function setupMazeControls() {
    elements.btnGenerate.addEventListener('click', generateMaze);
    
    elements.btnZoomIn.addEventListener('click', () => {
        state.zoom = Math.min(state.zoom * 1.2, 3);
        drawMaze();
    });
    
    elements.btnZoomOut.addEventListener('click', () => {
        state.zoom = Math.max(state.zoom / 1.2, 0.5);
        drawMaze();
    });
    
    elements.btnReset.addEventListener('click', () => {
        state.zoom = 1;
        drawMaze();
    });
}

// ========================================
// BENCHMARK
// ========================================

function setupBenchmark() {
    elements.btnRunBenchmark.addEventListener('click', runBenchmark);
}

// ========================================
// RESIZE HANDLER
// ========================================

function setupResizeHandler() {
    let resizeTimeout;
    window.addEventListener('resize', () => {
        clearTimeout(resizeTimeout);
        resizeTimeout = setTimeout(() => {
            if (state.maze) {
                drawMaze();
            }
        }, 100);
    });
}

// ========================================
// MAZE GENERATION
// ========================================

async function generateMaze() {
    showLoading(true);
    
    const algorithm = document.querySelector('input[name="algorithm"]:checked').value;
    const width = parseInt(elements.mazeWidth.value) || 20;
    const height = parseInt(elements.mazeHeight.value) || 20;
    const threads = parseInt(elements.threads.value) || 4;
    const extraLoops = parseInt(elements.extraLoops.value) || 10;
    const exitCount = parseInt(elements.exits.value) || 1;
    
    try {
        const response = await fetch('/api/maze/generate', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                algorithm,
                width,
                height,
                threads,
                extraLoops,
                exits: exitCount
            })
        });
        
        if (!response.ok) throw new Error('Generation failed');
        
        const data = await response.json();
        state.maze = data.maze;
        state.path = data.path;
        state.generationTime = data.generationTime;
        
        elements.mazePlaceholder.classList.add('hidden');
        updateStats();
        drawMaze();
        updateExportPreview();
        
        showToast('Лабиринт сгенерирован!', 'success');
    } catch (error) {
        console.error('Error generating maze:', error);
        showToast('Ошибка генерации. Проверьте, запущен ли сервер.', 'error');
        
        // Fallback: generate locally
        generateMazeLocally(width, height);
    } finally {
        showLoading(false);
    }
}

// ========================================
// LOCAL MAZE GENERATION (FALLBACK)
// ========================================

function generateMazeLocally(width, height) {
    const UP = 1, DOWN = 2, LEFT = 4, RIGHT = 8;
    
    // Initialize cells with all walls
    const cells = [];
    for (let i = 0; i < height; i++) {
        cells[i] = [];
        for (let j = 0; j < width; j++) {
            cells[i][j] = 15; // All walls
        }
    }
    
    // Random start point on border
    const start = [Math.floor(Math.random() * height), 0];
    const end = [Math.floor(Math.random() * height), width - 1];
    
    // DFS generation
    const visited = new Set();
    const stack = [[start[0], start[1]]];
    visited.add(`${start[0]},${start[1]}`);
    
    const directions = [
        [-1, 0, UP, DOWN],
        [1, 0, DOWN, UP],
        [0, -1, LEFT, RIGHT],
        [0, 1, RIGHT, LEFT]
    ];
    
    while (stack.length > 0) {
        const [cx, cy] = stack[stack.length - 1];
        
        // Find unvisited neighbors
        const neighbors = [];
        for (const [dx, dy, dir, oppDir] of directions) {
            const nx = cx + dx, ny = cy + dy;
            if (nx >= 0 && nx < height && ny >= 0 && ny < width && !visited.has(`${nx},${ny}`)) {
                neighbors.push([nx, ny, dir, oppDir]);
            }
        }
        
        if (neighbors.length === 0) {
            stack.pop();
            continue;
        }
        
        // Choose random neighbor
        const [nx, ny, dir, oppDir] = neighbors[Math.floor(Math.random() * neighbors.length)];
        
        // Remove walls
        cells[cx][cy] &= ~dir;
        cells[nx][ny] &= ~oppDir;
        
        visited.add(`${nx},${ny}`);
        stack.push([nx, ny]);
    }
    
    // BFS for path
    const path = findPathBFS(cells, start, end, width, height);
    
    state.maze = {
        width,
        height,
        cells,
        start,
        end,
        exits: [end]
    };
    state.path = path;
    state.generationTime = 0;
    
    elements.mazePlaceholder.classList.add('hidden');
    updateStats();
    drawMaze();
    updateExportPreview();
}

function findPathBFS(cells, start, end, width, height) {
    const UP = 1, DOWN = 2, LEFT = 4, RIGHT = 8;
    const directions = [
        [-1, 0, UP],
        [1, 0, DOWN],
        [0, -1, LEFT],
        [0, 1, RIGHT]
    ];
    
    const queue = [[start[0], start[1]]];
    const visited = new Set([`${start[0]},${start[1]}`]);
    const parent = new Map();
    
    while (queue.length > 0) {
        const [cx, cy] = queue.shift();
        
        if (cx === end[0] && cy === end[1]) {
            // Reconstruct path
            const path = [];
            let curr = `${end[0]},${end[1]}`;
            while (curr) {
                const [x, y] = curr.split(',').map(Number);
                path.unshift([x, y]);
                curr = parent.get(curr);
            }
            return path;
        }
        
        for (const [dx, dy, dir] of directions) {
            const nx = cx + dx, ny = cy + dy;
            const key = `${nx},${ny}`;
            
            if (nx >= 0 && nx < height && ny >= 0 && ny < width &&
                !visited.has(key) && (cells[cx][cy] & dir) === 0) {
                visited.add(key);
                parent.set(key, `${cx},${cy}`);
                queue.push([nx, ny]);
            }
        }
    }
    
    return [];
}

// ========================================
// MAZE DRAWING
// ========================================

function drawMaze() {
    if (!state.maze) return;
    
    const canvas = elements.mazeCanvas;
    const ctx = canvas.getContext('2d');
    const container = elements.mazeContainer;
    
    const { width, height, cells, start, exits } = state.maze;
    const containerWidth = container.clientWidth - 40;
    const containerHeight = container.clientHeight - 40;
    
    // Calculate cell size
    const baseCellSize = Math.min(containerWidth / width, containerHeight / height);
    const cellSize = Math.max(8, Math.floor(baseCellSize * state.zoom));
    
    canvas.width = width * cellSize;
    canvas.height = height * cellSize;
    
    // Clear and fill background
    ctx.fillStyle = '#14161c';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    
    // Draw grid
    ctx.strokeStyle = 'rgba(50, 56, 70, 0.5)';
    ctx.lineWidth = 1;
    for (let i = 0; i <= height; i++) {
        ctx.beginPath();
        ctx.moveTo(0, i * cellSize);
        ctx.lineTo(width * cellSize, i * cellSize);
        ctx.stroke();
    }
    for (let j = 0; j <= width; j++) {
        ctx.beginPath();
        ctx.moveTo(j * cellSize, 0);
        ctx.lineTo(j * cellSize, height * cellSize);
        ctx.stroke();
    }
    
    // Draw start cell
    ctx.fillStyle = 'rgba(70, 200, 120, 0.6)';
    ctx.fillRect(start[1] * cellSize, start[0] * cellSize, cellSize, cellSize);
    
    // Draw exit cells
    ctx.fillStyle = 'rgba(220, 80, 80, 0.6)';
    for (const exit of exits) {
        ctx.fillRect(exit[1] * cellSize, exit[0] * cellSize, cellSize, cellSize);
    }
    
    // Draw path
    if (elements.showPath.checked && state.path.length > 1) {
        if (elements.animatePath.checked) {
            animateMazePath(ctx, cellSize);
        } else {
            drawFullPath(ctx, cellSize);
        }
    }
    
    // Draw walls
    ctx.strokeStyle = '#dce2eb';
    ctx.lineWidth = 2;
    ctx.lineCap = 'round';
    
    const UP = 1, DOWN = 2, LEFT = 4, RIGHT = 8;
    
    for (let i = 0; i < height; i++) {
        for (let j = 0; j < width; j++) {
            const mask = cells[i][j];
            const x = j * cellSize;
            const y = i * cellSize;
            
            if (mask & UP) {
                ctx.beginPath();
                ctx.moveTo(x, y);
                ctx.lineTo(x + cellSize, y);
                ctx.stroke();
            }
            if (mask & DOWN) {
                ctx.beginPath();
                ctx.moveTo(x, y + cellSize);
                ctx.lineTo(x + cellSize, y + cellSize);
                ctx.stroke();
            }
            if (mask & LEFT) {
                ctx.beginPath();
                ctx.moveTo(x, y);
                ctx.lineTo(x, y + cellSize);
                ctx.stroke();
            }
            if (mask & RIGHT) {
                ctx.beginPath();
                ctx.moveTo(x + cellSize, y);
                ctx.lineTo(x + cellSize, y + cellSize);
                ctx.stroke();
            }
        }
    }
}

function drawFullPath(ctx, cellSize) {
    if (state.path.length < 2) return;
    
    // Glow effect
    ctx.strokeStyle = 'rgba(93, 212, 255, 0.3)';
    ctx.lineWidth = Math.max(4, cellSize / 3);
    ctx.lineCap = 'round';
    ctx.lineJoin = 'round';
    
    ctx.beginPath();
    ctx.moveTo(state.path[0][1] * cellSize + cellSize / 2, state.path[0][0] * cellSize + cellSize / 2);
    for (let i = 1; i < state.path.length; i++) {
        ctx.lineTo(state.path[i][1] * cellSize + cellSize / 2, state.path[i][0] * cellSize + cellSize / 2);
    }
    ctx.stroke();
    
    // Main line
    ctx.strokeStyle = '#5dd4ff';
    ctx.lineWidth = Math.max(2, cellSize / 5);
    ctx.stroke();
    
    // End marker
    const end = state.path[state.path.length - 1];
    ctx.fillStyle = '#f7d56f';
    ctx.beginPath();
    ctx.arc(end[1] * cellSize + cellSize / 2, end[0] * cellSize + cellSize / 2, 
            Math.max(4, cellSize / 3), 0, Math.PI * 2);
    ctx.fill();
}

let animationFrame = 0;
let animationTimer = null;

function animateMazePath(ctx, cellSize) {
    if (animationTimer) {
        cancelAnimationFrame(animationTimer);
    }
    
    animationFrame = 0;
    
    function animate() {
        animationFrame = Math.min(animationFrame + 1, state.path.length);
        
        // Redraw maze without path
        drawMazeWithoutPath();
        
        // Draw animated path
        if (animationFrame > 1) {
            const canvas = elements.mazeCanvas;
            const ctx = canvas.getContext('2d');
            
            ctx.strokeStyle = 'rgba(93, 212, 255, 0.3)';
            ctx.lineWidth = Math.max(4, cellSize / 3);
            ctx.lineCap = 'round';
            ctx.lineJoin = 'round';
            
            ctx.beginPath();
            ctx.moveTo(state.path[0][1] * cellSize + cellSize / 2, state.path[0][0] * cellSize + cellSize / 2);
            for (let i = 1; i < animationFrame; i++) {
                ctx.lineTo(state.path[i][1] * cellSize + cellSize / 2, state.path[i][0] * cellSize + cellSize / 2);
            }
            ctx.stroke();
            
            ctx.strokeStyle = '#5dd4ff';
            ctx.lineWidth = Math.max(2, cellSize / 5);
            ctx.stroke();
            
            // Head marker
            const head = state.path[animationFrame - 1];
            ctx.fillStyle = '#f7d56f';
            ctx.beginPath();
            ctx.arc(head[1] * cellSize + cellSize / 2, head[0] * cellSize + cellSize / 2,
                    Math.max(4, cellSize / 3), 0, Math.PI * 2);
            ctx.fill();
        }
        
        if (animationFrame < state.path.length) {
            animationTimer = requestAnimationFrame(animate);
        }
    }
    
    setTimeout(animate, 100);
}

function drawMazeWithoutPath() {
    if (!state.maze) return;
    
    const canvas = elements.mazeCanvas;
    const ctx = canvas.getContext('2d');
    const { width, height, cells, start, exits } = state.maze;
    const cellSize = canvas.width / width;
    
    // Clear and fill background
    ctx.fillStyle = '#14161c';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    
    // Draw grid
    ctx.strokeStyle = 'rgba(50, 56, 70, 0.5)';
    ctx.lineWidth = 1;
    for (let i = 0; i <= height; i++) {
        ctx.beginPath();
        ctx.moveTo(0, i * cellSize);
        ctx.lineTo(width * cellSize, i * cellSize);
        ctx.stroke();
    }
    for (let j = 0; j <= width; j++) {
        ctx.beginPath();
        ctx.moveTo(j * cellSize, 0);
        ctx.lineTo(j * cellSize, height * cellSize);
        ctx.stroke();
    }
    
    // Draw start cell
    ctx.fillStyle = 'rgba(70, 200, 120, 0.6)';
    ctx.fillRect(start[1] * cellSize, start[0] * cellSize, cellSize, cellSize);
    
    // Draw exit cells
    ctx.fillStyle = 'rgba(220, 80, 80, 0.6)';
    for (const exit of exits) {
        ctx.fillRect(exit[1] * cellSize, exit[0] * cellSize, cellSize, cellSize);
    }
    
    // Draw walls
    ctx.strokeStyle = '#dce2eb';
    ctx.lineWidth = 2;
    ctx.lineCap = 'round';
    
    const UP = 1, DOWN = 2, LEFT = 4, RIGHT = 8;
    
    for (let i = 0; i < height; i++) {
        for (let j = 0; j < width; j++) {
            const mask = cells[i][j];
            const x = j * cellSize;
            const y = i * cellSize;
            
            if (mask & UP) {
                ctx.beginPath();
                ctx.moveTo(x, y);
                ctx.lineTo(x + cellSize, y);
                ctx.stroke();
            }
            if (mask & DOWN) {
                ctx.beginPath();
                ctx.moveTo(x, y + cellSize);
                ctx.lineTo(x + cellSize, y + cellSize);
                ctx.stroke();
            }
            if (mask & LEFT) {
                ctx.beginPath();
                ctx.moveTo(x, y);
                ctx.lineTo(x, y + cellSize);
                ctx.stroke();
            }
            if (mask & RIGHT) {
                ctx.beginPath();
                ctx.moveTo(x + cellSize, y);
                ctx.lineTo(x + cellSize, y + cellSize);
                ctx.stroke();
            }
        }
    }
}

// ========================================
// STATS
// ========================================

function updateStats() {
    if (!state.maze) return;
    
    elements.genTime.textContent = state.generationTime > 0 ? `${state.generationTime} мс` : '—';
    elements.mazeSize.textContent = `${state.maze.width}×${state.maze.height}`;
    elements.pathLength.textContent = state.path.length > 0 ? `${state.path.length} клеток` : 'Нет';
}

// ========================================
// EXPORT
// ========================================

function updateExportPreview() {
    if (!state.maze) {
        elements.exportPreview.innerHTML = '<p class="preview-placeholder">Сначала сгенерируйте лабиринт</p>';
        return;
    }
    
    elements.exportPreview.innerHTML = `
        <button class="btn btn-primary" onclick="exportMaze('${state.selectedFormat}')">
            <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                <path d="M21 15v4a2 2 0 01-2 2H5a2 2 0 01-2-2v-4M7 10l5 5 5-5M12 15V3"/>
            </svg>
            Скачать ${state.selectedFormat.toUpperCase()}
        </button>
    `;
}

async function exportMaze(format) {
    if (!state.maze) {
        showToast('Сначала сгенерируйте лабиринт', 'error');
        return;
    }
    
    showLoading(true);
    
    try {
        const response = await fetch('/api/maze/export', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                format,
                width: state.maze.width,
                height: state.maze.height,
                cellSize: parseInt(elements.cellSize.value) || 20,
                cells: JSON.stringify(state.maze.cells),
                path: JSON.stringify(state.path),
                start: JSON.stringify(state.maze.start),
                end: JSON.stringify(state.maze.end),
                exits: JSON.stringify(state.maze.exits),
                showPath: elements.exportShowPath.checked.toString(),
                showGrid: elements.exportShowGrid.checked.toString(),
                wallColor: elements.wallColor.value,
                bgColor: elements.bgColor.value,
                pathColor: elements.pathColorExport.value
            })
        });
        
        if (!response.ok) throw new Error('Export failed');
        
        const blob = await response.blob();
        const url = URL.createObjectURL(blob);
        
        const a = document.createElement('a');
        a.href = url;
        a.download = `maze.${format === 'jpeg' ? 'jpg' : format}`;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
        
        showToast(`Экспортировано в ${format.toUpperCase()}!`, 'success');
    } catch (error) {
        console.error('Export error:', error);
        
        // Fallback: export from canvas
        exportFromCanvas(format);
    } finally {
        showLoading(false);
    }
}

function exportFromCanvas(format) {
    if (!state.maze) return;
    
    const canvas = elements.mazeCanvas;
    let dataUrl;
    let filename;
    
    switch (format) {
        case 'png':
            dataUrl = canvas.toDataURL('image/png');
            filename = 'maze.png';
            break;
        case 'jpeg':
        case 'jpg':
            dataUrl = canvas.toDataURL('image/jpeg', 0.9);
            filename = 'maze.jpg';
            break;
        case 'svg':
            const svg = generateSVG();
            const blob = new Blob([svg], { type: 'image/svg+xml' });
            const url = URL.createObjectURL(blob);
            const a = document.createElement('a');
            a.href = url;
            a.download = 'maze.svg';
            document.body.appendChild(a);
            a.click();
            document.body.removeChild(a);
            URL.revokeObjectURL(url);
            showToast('Экспортировано в SVG!', 'success');
            return;
        case 'json':
            const json = JSON.stringify({
                width: state.maze.width,
                height: state.maze.height,
                cells: state.maze.cells,
                start: state.maze.start,
                end: state.maze.end,
                exits: state.maze.exits,
                path: state.path
            }, null, 2);
            const jsonBlob = new Blob([json], { type: 'application/json' });
            const jsonUrl = URL.createObjectURL(jsonBlob);
            const jsonA = document.createElement('a');
            jsonA.href = jsonUrl;
            jsonA.download = 'maze.json';
            document.body.appendChild(jsonA);
            jsonA.click();
            document.body.removeChild(jsonA);
            URL.revokeObjectURL(jsonUrl);
            showToast('Экспортировано в JSON!', 'success');
            return;
        default:
            return;
    }
    
    const a = document.createElement('a');
    a.href = dataUrl;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    
    showToast(`Экспортировано в ${format.toUpperCase()}!`, 'success');
}

function generateSVG() {
    const { width, height, cells, start, exits } = state.maze;
    const cellSize = parseInt(elements.cellSize.value) || 20;
    const imgWidth = width * cellSize + 2;
    const imgHeight = height * cellSize + 2;
    const UP = 1, DOWN = 2, LEFT = 4, RIGHT = 8;
    
    let svg = `<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" width="${imgWidth}" height="${imgHeight}">
<rect width="100%" height="100%" fill="${elements.bgColor.value}"/>`;
    
    // Grid
    if (elements.exportShowGrid.checked) {
        svg += `<g stroke="#32384a" stroke-width="1">`;
        for (let i = 0; i <= height; i++) {
            svg += `<line x1="1" y1="${1 + i * cellSize}" x2="${1 + width * cellSize}" y2="${1 + i * cellSize}"/>`;
        }
        for (let j = 0; j <= width; j++) {
            svg += `<line x1="${1 + j * cellSize}" y1="1" x2="${1 + j * cellSize}" y2="${1 + height * cellSize}"/>`;
        }
        svg += `</g>`;
    }
    
    // Start
    svg += `<rect x="${1 + start[1] * cellSize}" y="${1 + start[0] * cellSize}" width="${cellSize}" height="${cellSize}" fill="rgba(70,200,120,0.6)"/>`;
    
    // Exits
    for (const exit of exits) {
        svg += `<rect x="${1 + exit[1] * cellSize}" y="${1 + exit[0] * cellSize}" width="${cellSize}" height="${cellSize}" fill="rgba(220,80,80,0.6)"/>`;
    }
    
    // Path
    if (elements.exportShowPath.checked && state.path.length > 1) {
        svg += `<path d="M`;
        for (let i = 0; i < state.path.length; i++) {
            const x = 1 + state.path[i][1] * cellSize + cellSize / 2;
            const y = 1 + state.path[i][0] * cellSize + cellSize / 2;
            svg += i > 0 ? ` L${x} ${y}` : `${x} ${y}`;
        }
        svg += `" fill="none" stroke="${elements.pathColorExport.value}" stroke-width="${Math.max(2, cellSize / 4)}" stroke-linecap="round" stroke-linejoin="round"/>`;
    }
    
    // Walls
    svg += `<g stroke="${elements.wallColor.value}" stroke-width="2" stroke-linecap="round">`;
    for (let i = 0; i < height; i++) {
        for (let j = 0; j < width; j++) {
            const mask = cells[i][j];
            const x = 1 + j * cellSize;
            const y = 1 + i * cellSize;
            if (mask & UP) svg += `<line x1="${x}" y1="${y}" x2="${x + cellSize}" y2="${y}"/>`;
            if (mask & DOWN) svg += `<line x1="${x}" y1="${y + cellSize}" x2="${x + cellSize}" y2="${y + cellSize}"/>`;
            if (mask & LEFT) svg += `<line x1="${x}" y1="${y}" x2="${x}" y2="${y + cellSize}"/>`;
            if (mask & RIGHT) svg += `<line x1="${x + cellSize}" y1="${y}" x2="${x + cellSize}" y2="${y + cellSize}"/>`;
        }
    }
    svg += `</g>`;
    svg += `</svg>`;
    
    return svg;
}

// ========================================
// BENCHMARKS
// ========================================

async function runBenchmark() {
    showLoading(true);
    elements.btnRunBenchmark.disabled = true;
    
    try {
        const response = await fetch('/api/benchmarks', { method: 'POST' });
        if (!response.ok) throw new Error('Benchmark failed');
        
        const data = await response.json();
        drawBenchmarkGraph(data);
        showToast('Тест завершён!', 'success');
    } catch (error) {
        console.error('Benchmark error:', error);
        
        // Fallback: run local benchmark
        runLocalBenchmark();
    } finally {
        showLoading(false);
        elements.btnRunBenchmark.disabled = false;
    }
}

function runLocalBenchmark() {
    const threadCounts = [1, 2, 4, 6, 8];
    const syncTimes = [];
    const nosyncTimes = [];
    
    for (const threads of threadCounts) {
        const start = performance.now();
        for (let i = 0; i < 3; i++) {
            generateMazeSync(30, 30);
        }
        syncTimes.push((performance.now() - start) / 3);
        
        const start2 = performance.now();
        for (let i = 0; i < 3; i++) {
            for (let t = 0; t < threads; t++) {
                generateMazeSync(Math.floor(30 / threads) + 1, 30);
            }
        }
        nosyncTimes.push((performance.now() - start2) / 3);
    }
    
    drawBenchmarkGraph({ threads: threadCounts, sync: syncTimes, nosync: nosyncTimes });
    showToast('Локальный тест завершён!', 'success');
}

function generateMazeSync(height, width) {
    const cells = [];
    for (let i = 0; i < height; i++) {
        cells[i] = [];
        for (let j = 0; j < width; j++) {
            cells[i][j] = 15;
        }
    }
    
    const visited = new Set();
    const stack = [[0, 0]];
    visited.add('0,0');
    
    const directions = [[-1, 0, 1, 2], [1, 0, 2, 1], [0, -1, 4, 8], [0, 1, 8, 4]];
    
    while (stack.length > 0) {
        const [cx, cy] = stack[stack.length - 1];
        const neighbors = [];
        
        for (const [dx, dy, dir, oppDir] of directions) {
            const nx = cx + dx, ny = cy + dy;
            if (nx >= 0 && nx < height && ny >= 0 && ny < width && !visited.has(`${nx},${ny}`)) {
                neighbors.push([nx, ny, dir, oppDir]);
            }
        }
        
        if (neighbors.length === 0) {
            stack.pop();
            continue;
        }
        
        const [nx, ny, dir, oppDir] = neighbors[Math.floor(Math.random() * neighbors.length)];
        cells[cx][cy] &= ~dir;
        cells[nx][ny] &= ~oppDir;
        visited.add(`${nx},${ny}`);
        stack.push([nx, ny]);
    }
    
    return cells;
}

function drawBenchmarkGraph(data) {
    const canvas = elements.benchmarkCanvas;
    const ctx = canvas.getContext('2d');
    
    const padding = 50;
    const width = canvas.width - padding * 2;
    const height = canvas.height - padding * 2;
    
    // Clear
    ctx.fillStyle = '#14161c';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    
    const maxVal = Math.max(...data.sync, ...data.nosync) * 1.1;
    const points = data.threads.length;
    
    // Grid
    ctx.strokeStyle = 'rgba(93, 212, 255, 0.1)';
    ctx.lineWidth = 1;
    
    for (let i = 0; i <= 5; i++) {
        const y = padding + (height / 5) * i;
        ctx.beginPath();
        ctx.moveTo(padding, y);
        ctx.lineTo(canvas.width - padding, y);
        ctx.stroke();
        
        // Y-axis labels
        ctx.fillStyle = '#6b7280';
        ctx.font = '11px JetBrains Mono';
        ctx.textAlign = 'right';
        const val = maxVal - (maxVal / 5) * i;
        ctx.fillText(val.toFixed(1) + ' мс', padding - 10, y + 4);
    }
    
    // X-axis labels
    ctx.textAlign = 'center';
    for (let i = 0; i < points; i++) {
        const x = padding + (width / (points - 1)) * i;
        ctx.fillText(data.threads[i] + ' потоков', x, canvas.height - 15);
    }
    
    // Draw lines
    function drawLine(series, color) {
        ctx.strokeStyle = color;
        ctx.lineWidth = 3;
        ctx.lineCap = 'round';
        ctx.lineJoin = 'round';
        
        ctx.beginPath();
        for (let i = 0; i < series.length; i++) {
            const x = padding + (width / (points - 1)) * i;
            const y = padding + height - (series[i] / maxVal) * height;
            if (i === 0) ctx.moveTo(x, y);
            else ctx.lineTo(x, y);
        }
        ctx.stroke();
        
        // Points
        for (let i = 0; i < series.length; i++) {
            const x = padding + (width / (points - 1)) * i;
            const y = padding + height - (series[i] / maxVal) * height;
            
            ctx.fillStyle = '#14161c';
            ctx.beginPath();
            ctx.arc(x, y, 6, 0, Math.PI * 2);
            ctx.fill();
            
            ctx.fillStyle = color;
            ctx.beginPath();
            ctx.arc(x, y, 4, 0, Math.PI * 2);
            ctx.fill();
        }
    }
    
    drawLine(data.sync, '#5dd4ff');
    drawLine(data.nosync, '#f5b041');
    
    // Title
    ctx.fillStyle = '#e7eaf1';
    ctx.font = '14px Outfit';
    ctx.textAlign = 'center';
    ctx.fillText('Время генерации лабиринта (мс)', canvas.width / 2, 25);
}

// ========================================
// LOADING & TOAST
// ========================================

function showLoading(show) {
    elements.loadingOverlay.classList.toggle('visible', show);
}

function showToast(message, type = 'success') {
    const toast = document.createElement('div');
    toast.className = `toast ${type}`;
    toast.innerHTML = `
        <svg class="toast-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
            ${type === 'success' 
                ? '<path d="M22 11.08V12a10 10 0 1 1-5.93-9.14"/><polyline points="22 4 12 14.01 9 11.01"/>'
                : '<circle cx="12" cy="12" r="10"/><line x1="15" y1="9" x2="9" y2="15"/><line x1="9" y1="9" x2="15" y2="15"/>'}
        </svg>
        <span class="toast-message">${message}</span>
    `;
    
    elements.toastContainer.appendChild(toast);
    
    setTimeout(() => {
        toast.style.animation = 'slideIn 0.3s ease reverse';
        setTimeout(() => toast.remove(), 300);
    }, 3000);
}

// ========================================
// GLOBAL EXPORT FUNCTION
// ========================================

window.exportMaze = exportMaze;

// ========================================
// INITIALIZE
// ========================================

document.addEventListener('DOMContentLoaded', init);
