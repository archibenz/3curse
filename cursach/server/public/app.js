const state = {
  apiBase: '/api',
  items: [],
  editingId: null,
};

async function loadConfig() {
  try {
    const resp = await fetch('config.json');
    const json = await resp.json();
    state.apiBase = json.apiBase || '/api';
  } catch (e) {
    console.warn('Не удалось загрузить конфиг клиента, используется /api', e);
  }
}

function toast(message) {
  const toastEl = document.getElementById('toast');
  toastEl.textContent = message;
  toastEl.classList.remove('hidden');
  setTimeout(() => toastEl.classList.add('hidden'), 2500);
}

async function request(path, options = {}) {
  const resp = await fetch(`${state.apiBase}${path}`, {
    headers: { 'Content-Type': 'application/json' },
    ...options,
  });
  const contentType = resp.headers.get('Content-Type') || '';
  const data = contentType.includes('application/json') ? await resp.json() : {};
  if (!resp.ok) {
    throw new Error(data.error || data.message || 'Ошибка запроса');
  }
  return data;
}

function formatDate(value) {
  if (!value) return '—';
  return new Date(value + 'Z').toLocaleString('ru-RU');
}

function render() {
  const list = document.getElementById('list');
  const empty = document.getElementById('empty');
  list.innerHTML = '';

  const search = document.getElementById('search').value.toLowerCase();
  const status = document.getElementById('statusFilter').value;
  const filtered = state.items.filter((item) => {
    const matchesSearch = item.title.toLowerCase().includes(search) || (item.description || '').toLowerCase().includes(search);
    const matchesStatus = status === 'all' || item.status === status;
    return matchesSearch && matchesStatus;
  });

  if (!filtered.length) {
    empty.classList.remove('hidden');
    return;
  }

  empty.classList.add('hidden');
  const template = document.getElementById('cardTemplate');

  filtered.forEach((item) => {
    const node = template.content.cloneNode(true);
    node.querySelector('.status').textContent = item.status;
    node.querySelector('.status').dataset.status = item.status;
    node.querySelector('.id').textContent = item.id;
    node.querySelector('.created').textContent = formatDate(item.created_at);
    node.querySelector('.title').textContent = item.title;
    node.querySelector('.description').textContent = item.description || '—';

    node.querySelector('.edit').addEventListener('click', () => populateForm(item));
    node.querySelector('.delete').addEventListener('click', () => deleteEvent(item.id));

    list.appendChild(node);
  });
}

async function loadEvents() {
  const data = await request('/events');
  state.items = Array.isArray(data) ? data : data.items || [];
  render();
  document.getElementById('envInfo').textContent = `Клиентов: ∞ | Всего записей: ${state.items.length}`;
}

function resetForm() {
  const form = document.getElementById('eventForm');
  form.reset();
  state.editingId = null;
  document.getElementById('formTitle').textContent = 'Новое событие';
}

function populateForm(item) {
  document.getElementById('title').value = item.title;
  document.getElementById('description').value = item.description;
  document.getElementById('status').value = item.status;
  document.getElementById('formTitle').textContent = `Редактирование #${item.id}`;
  state.editingId = item.id;
}

async function deleteEvent(id) {
  if (!confirm('Удалить событие?')) return;
  await request(`/events/${id}`, { method: 'DELETE' });
  toast('Событие удалено');
  await loadEvents();
}

async function handleSubmit(event) {
  event.preventDefault();
  const payload = {
    title: document.getElementById('title').value,
    description: document.getElementById('description').value,
    status: document.getElementById('status').value,
  };

  if (state.editingId) {
    await request(`/events/${state.editingId}`, { method: 'PUT', body: JSON.stringify(payload) });
    toast('Изменения сохранены');
  } else {
    await request('/events', { method: 'POST', body: JSON.stringify(payload) });
    toast('Событие создано');
  }
  resetForm();
  await loadEvents();
}

async function init() {
  await loadConfig();
  document.getElementById('eventForm').addEventListener('submit', handleSubmit);
  document.getElementById('reset').addEventListener('click', resetForm);
  document.getElementById('openCreate').addEventListener('click', resetForm);
  document.getElementById('createInline').addEventListener('click', resetForm);
  document.getElementById('reload').addEventListener('click', loadEvents);
  document.getElementById('search').addEventListener('input', render);
  document.getElementById('statusFilter').addEventListener('change', render);

  await loadEvents();
}

init().catch((err) => {
  console.error(err);
  toast('Не удалось загрузить данные. Проверьте сервер.');
});
