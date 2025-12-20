const tabs = document.querySelectorAll('.nav button');
const sections = document.querySelectorAll('.tab');

function activate(tabId) {
  tabs.forEach(btn => btn.classList.toggle('active', btn.dataset.tab === tabId));
  sections.forEach(sec => sec.classList.toggle('active', sec.id === tabId));
}

tabs.forEach(btn => btn.addEventListener('click', () => activate(btn.dataset.tab)));

const mazeCanvas = document.getElementById('mazeCanvas');
const mazeCtx = mazeCanvas.getContext('2d');
let mazeData = null;
let animIndex = 0;
let animTimer = null;

function drawGrid(width, height, path = []) {
  const cw = mazeCanvas.width / width;
  const ch = mazeCanvas.height / height;
  mazeCtx.clearRect(0, 0, mazeCanvas.width, mazeCanvas.height);
  mazeCtx.strokeStyle = '#283040';
  mazeCtx.lineWidth = 1;
  for (let i = 0; i <= height; i++) {
    mazeCtx.beginPath();
    mazeCtx.moveTo(0, i * ch);
    mazeCtx.lineTo(mazeCanvas.width, i * ch);
    mazeCtx.stroke();
  }
  for (let j = 0; j <= width; j++) {
    mazeCtx.beginPath();
    mazeCtx.moveTo(j * cw, 0);
    mazeCtx.lineTo(j * cw, mazeCanvas.height);
    mazeCtx.stroke();
  }

  if (path.length > 1) {
    mazeCtx.strokeStyle = '#5dd4ff';
    mazeCtx.lineWidth = Math.max(2, Math.min(cw, ch) / 3);
    mazeCtx.beginPath();
    const [r0, c0] = path[0];
    mazeCtx.moveTo(c0 * cw + cw / 2, r0 * ch + ch / 2);
    for (let i = 1; i < path.length; i++) {
      const [r, c] = path[i];
      mazeCtx.lineTo(c * cw + cw / 2, r * ch + ch / 2);
    }
    mazeCtx.stroke();
  }

  if (path.length) {
    const [r, c] = path[path.length - 1];
    mazeCtx.fillStyle = '#f7d36f';
    mazeCtx.beginPath();
    mazeCtx.arc(c * cw + cw / 2, r * ch + ch / 2, Math.max(4, Math.min(cw, ch) / 3), 0, Math.PI * 2);
    mazeCtx.fill();
  }
}

async function loadMaze() {
  const res = await fetch('/api/maze');
  mazeData = await res.json();
  animIndex = 0;
  drawGrid(mazeData.width, mazeData.height, []);
}

function animatePath() {
  if (!mazeData) return;
  if (animTimer) clearInterval(animTimer);
  animIndex = 0;
  animTimer = setInterval(() => {
    animIndex = Math.min(animIndex + 1, mazeData.path.length);
    drawGrid(mazeData.width, mazeData.height, mazeData.path.slice(0, animIndex));
    if (animIndex >= mazeData.path.length) {
      clearInterval(animTimer);
    }
  }, 60);
}

const btnGenerate = document.getElementById('btnGenerate');
const btnAnimate = document.getElementById('btnAnimate');

btnGenerate.addEventListener('click', () => loadMaze());
btnAnimate.addEventListener('click', () => animatePath());

const graphCanvas = document.getElementById('graphCanvas');
const graphCtx = graphCanvas.getContext('2d');

function drawGraph(data) {
  graphCtx.clearRect(0, 0, graphCanvas.width, graphCanvas.height);
  graphCtx.fillStyle = '#14161c';
  graphCtx.fillRect(0, 0, graphCanvas.width, graphCanvas.height);

  const maxVal = Math.max(...data.sync, ...data.nosync);
  const padding = 40;
  const width = graphCanvas.width - padding * 2;
  const height = graphCanvas.height - padding * 2;

  function plot(series, color) {
    graphCtx.strokeStyle = color;
    graphCtx.lineWidth = 3;
    graphCtx.beginPath();
    series.forEach((val, idx) => {
      const x = padding + (idx / (series.length - 1)) * width;
      const y = padding + height - (val / maxVal) * height;
      if (idx === 0) graphCtx.moveTo(x, y);
      else graphCtx.lineTo(x, y);
    });
    graphCtx.stroke();
  }

  plot(data.sync, '#5dd4ff');
  plot(data.nosync, '#f5b041');
}

const btnBench = document.getElementById('btnBench');
btnBench.addEventListener('click', async () => {
  const res = await fetch('/api/benchmarks');
  const data = await res.json();
  drawGraph(data);
});

loadMaze();
