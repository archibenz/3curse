const state = {
  apiBase: '/api',
  items: [],
  editingId: null,
  refreshTimer: null,
  pendingDelete: null,
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
  if (!message) return;
}

function localizeError(err, fallback = 'Не удалось выполнить запрос') {
  if (!err) return fallback;
  const message = (err.message || '').trim();
  if (!message) return fallback;
  const mappings = {
    'Failed to fetch': 'Нет ответа от сервера. Проверьте, что он запущен',
    'The string did not match the expected pattern.': 'Некорректный адрес запроса',
  };
  if (mappings[message]) return mappings[message];
  if (/[А-Яа-яЁё]/.test(message)) return message;
  return fallback;
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
    node.querySelector('.delete').addEventListener('click', () => openDeleteMenu(item));

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
  form.querySelectorAll('input, textarea').forEach((field) => {
    field.value = '';
  });
  document.getElementById('status').value = 'planned';
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

function openDeleteMenu(item) {
  state.pendingDelete = item;
  document.getElementById('deleteTitle').textContent = item.title;
  document.getElementById('deleteMeta').textContent = `ID ${item.id} • ${formatDate(item.created_at)}`;
  document.getElementById('deleteMenu').classList.remove('hidden');
}

function closeDeleteMenu() {
  state.pendingDelete = null;
  document.getElementById('deleteMenu').classList.add('hidden');
}

async function confirmDelete() {
  if (!state.pendingDelete) return;
  const deleteId = state.pendingDelete.id;
  closeDeleteMenu();
  try {
    await request(`/events/${deleteId}`, { method: 'DELETE' });
    toast('Событие удалено');
  } catch (err) {
    console.error(err);
    toast(localizeError(err, 'Не удалось удалить событие'));
    return;
  }
  try {
    await loadEvents();
  } catch (err) {
    console.warn('Не удалось обновить список после удаления', err);
    toast(localizeError(err, 'Список обновится автоматически через несколько секунд.'));
  }
}

async function handleSubmit(event) {
  event.preventDefault();
  const payload = {
    title: document.getElementById('title').value,
    description: document.getElementById('description').value,
    status: document.getElementById('status').value,
  };

  try {
    if (state.editingId) {
      await request(`/events/${state.editingId}`, { method: 'PUT', body: JSON.stringify(payload) });
      toast('Изменения сохранены');
    } else {
      await request('/events', { method: 'POST', body: JSON.stringify(payload) });
      toast('Событие создано');
    }
    resetForm();
    try {
      await loadEvents();
    } catch (err) {
      console.warn('Не удалось обновить список после сохранения', err);
      toast(localizeError(err, 'Список обновится автоматически через несколько секунд'));
    }
  } catch (err) {
    console.error(err);
    toast(localizeError(err, 'Не удалось сохранить событие'));
  }
}

function scheduleRefresh() {
  if (state.refreshTimer) {
    clearInterval(state.refreshTimer);
  }
  state.refreshTimer = setInterval(() => {
    loadEvents().catch((err) => console.warn('Не удалось обновить список', err));
  }, 12000);
}

async function init() {
  await loadConfig();
  document.getElementById('eventForm').addEventListener('submit', handleSubmit);
  document.getElementById('reset').addEventListener('click', (event) => {
    event.preventDefault();
    resetForm();
  });
  document.getElementById('openCreate').addEventListener('click', (event) => {
    event.preventDefault();
    resetForm();
  });
  document.getElementById('createInline').addEventListener('click', (event) => {
    event.preventDefault();
    resetForm();
  });
  document.getElementById('reload').addEventListener('click', loadEvents);
  document.getElementById('search').addEventListener('input', render);
  document.getElementById('statusFilter').addEventListener('change', render);
  document.getElementById('deleteCancel').addEventListener('click', closeDeleteMenu);
  document.getElementById('deleteCancelSecondary').addEventListener('click', closeDeleteMenu);
  document.getElementById('deleteConfirm').addEventListener('click', confirmDelete);
  document.getElementById('deleteMenu').addEventListener('click', (e) => {
    if (e.target.id === 'deleteMenu') closeDeleteMenu();
  });

  await loadEvents();
  scheduleRefresh();
}

init().catch((err) => {
  console.error(err);
  toast(localizeError(err, 'Не удалось загрузить данные. Проверьте сервер'));
});
