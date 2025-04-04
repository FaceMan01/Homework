document.addEventListener('DOMContentLoaded', () => {
    const apiUrl = 'http://127.0.0.1:8000/todos/';

    const todoList = document.getElementById('todo-list');
    const addTodoForm = document.getElementById('add-todo-form');
    const titleInput = document.getElementById('title');
    const descriptionInput = document.getElementById('description');
    const filterAllBtn = document.getElementById('filter-all');
    const filterActiveBtn = document.getElementById('filter-active');
    const filterDoneBtn = document.getElementById('filter-done');

    let currentFilter = null; // null = all, false = active, true = done

    // --- Функции для работы с API ---

    const fetchTodos = async (isDoneFilter = null) => {
        // Перед загрузкой отменим редактирование, если оно активно
        cancelAnyActiveEdit();
        showLoading();
        let url = apiUrl;
        const params = new URLSearchParams();
        if (isDoneFilter !== null) {
            params.append('is_done', isDoneFilter);
        }
        params.append('limit', 200); // Загружаем больше задач

        url += `?${params.toString()}`;

        try {
            const response = await fetch(url);
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            const todos = await response.json();
            renderTodos(todos);
        } catch (error) {
            console.error('Ошибка при загрузке задач:', error);
            showError('Не удалось загрузить задачи.');
        }
    };

    const addTodo = async (title, description) => {
        cancelAnyActiveEdit(); // Отменяем редактирование перед добавлением
        try {
            const response = await fetch(apiUrl, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    title: title,
                    description: description || null,
                    is_done: false
                }),
            });
            if (!response.ok) {
                let errorDetail = `HTTP error! status: ${response.status}`;
                try {
                    const errorData = await response.json();
                    errorDetail = errorData.detail || JSON.stringify(errorData);
                } catch (e) { /* ignore */ }
                throw new Error(errorDetail);
            }
            fetchTodos(currentFilter);
            addTodoForm.reset();
        } catch (error) {
            console.error('Ошибка при добавлении задачи:', error);
            alert(`Не удалось добавить задачу: ${error.message}`);
        }
    };

    const updateTodo = async (id, data) => {
         // Отправляем только те поля, которые нужно обновить
         // data может быть {is_done: boolean} или {title: string, description: string}
        try {
            const response = await fetch(`${apiUrl}${id}`, {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(data),
            });
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
            return await response.json(); // Возвращаем обновленный todo
        } catch (error) {
            console.error(`Ошибка при обновлении задачи ${id}:`, error);
            alert('Не удалось обновить задачу.');
            return null; // Возвращаем null в случае ошибки
        }
    };

    const deleteTodo = async (id) => {
        cancelAnyActiveEdit(); // Отменяем редактирование перед удалением
        if (!confirm('Вы уверены, что хотите удалить эту задачу?')) return;

        try {
            const response = await fetch(`${apiUrl}${id}`, { method: 'DELETE' });
            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }
             fetchTodos(currentFilter); // Обновляем список после успешного удаления
        } catch (error) {
            console.error('Ошибка при удалении задачи:', error);
            alert('Не удалось удалить задачу.');
        }
    };

    // --- Функции для отрисовки UI ---

    const renderTodos = (todos) => {
        todoList.innerHTML = '';

        if (todos.length === 0) {
            todoList.innerHTML = '<li class="loading">Нет задач для отображения.</li>';
            return;
        }

        todos.forEach(todo => {
            const li = createTodoElement(todo);
            todoList.appendChild(li);
        });
    };

    const createTodoElement = (todo) => {
        const li = document.createElement('li');
        li.className = todo.is_done ? 'done' : '';
        li.dataset.id = todo.id;

        // Чекбокс
        const checkbox = document.createElement('input');
        checkbox.type = 'checkbox';
        checkbox.checked = todo.is_done;
        checkbox.addEventListener('change', async (e) => {
             // Отменяем редактирование, если оно активно в ЭТОМ элементе
            if (li.classList.contains('editing')) {
                cancelEdit(li);
            }
            const updatedTodo = await updateTodo(todo.id, { is_done: e.target.checked });
            if (updatedTodo) {
                 // Обновляем стиль элемента LI вместо перезагрузки всего списка
                li.classList.toggle('done', updatedTodo.is_done);
                 // Если активен фильтр, и статус задачи больше ему не соответствует,
                 // можно либо перезагрузить список, либо удалить элемент
                if (currentFilter !== null && updatedTodo.is_done !== currentFilter) {
                    fetchTodos(currentFilter); // Проще перезагрузить
                }
            } else {
                 // Вернуть чекбокс в исходное состояние при ошибке
                e.target.checked = !e.target.checked;
            }
        });

        // Контент задачи (текст)
        const contentDiv = document.createElement('div');
        contentDiv.className = 'todo-content';
        const titleSpan = document.createElement('span');
        titleSpan.className = 'title';
        titleSpan.textContent = todo.title;
        contentDiv.appendChild(titleSpan);
        const descriptionSpan = document.createElement('span');
        descriptionSpan.className = 'description';
        descriptionSpan.textContent = todo.description || ''; // Показываем пустую строку, если нет описания
        contentDiv.appendChild(descriptionSpan);

        // Контейнер для полей ввода (скрыт по умолчанию)
        const editInputContainer = document.createElement('div');
        editInputContainer.className = 'edit-input-container';
        const editTitleInput = document.createElement('input');
        editTitleInput.type = 'text';
        editTitleInput.className = 'edit-title-input';
        editTitleInput.required = true;
        editTitleInput.maxLength = 100;
        const editDescriptionTextarea = document.createElement('textarea');
        editDescriptionTextarea.className = 'edit-desc-textarea';
        editDescriptionTextarea.maxLength = 500;
        editInputContainer.appendChild(editTitleInput);
        editInputContainer.appendChild(editDescriptionTextarea);

        // Контейнер для кнопок действий
        const actionButtonsDiv = document.createElement('div');
        actionButtonsDiv.className = 'action-buttons';

        // Кнопка Редактировать
        const editBtn = document.createElement('button');
        editBtn.className = 'edit-btn';
        editBtn.textContent = 'Редактировать';
        editBtn.addEventListener('click', () => {
            startEditing(li, todo.title, todo.description || '');
        });

        // Кнопка Удалить
        const deleteBtn = document.createElement('button');
        deleteBtn.className = 'delete-btn';
        deleteBtn.textContent = 'Удалить';
        deleteBtn.addEventListener('click', () => deleteTodo(todo.id));

        // Кнопка Сохранить (скрыта по умолчанию)
        const saveBtn = document.createElement('button');
        saveBtn.className = 'save-btn';
        saveBtn.textContent = 'Сохранить';
        saveBtn.addEventListener('click', async () => {
            const newTitle = editTitleInput.value.trim();
            const newDescription = editDescriptionTextarea.value.trim();
            if (!newTitle) {
                alert('Название задачи не может быть пустым!');
                editTitleInput.focus();
                return;
            }
            const updatedTodo = await updateTodo(todo.id, { title: newTitle, description: newDescription || null });
            if (updatedTodo) {
                // Обновляем текст в оригинальных спанах
                titleSpan.textContent = updatedTodo.title;
                descriptionSpan.textContent = updatedTodo.description || '';
                cancelEdit(li); // Выходим из режима редактирования
            }
            // Если updateTodo вернул null, ошибка уже была показана
        });

        // Кнопка Отмена (скрыта по умолчанию)
        const cancelBtn = document.createElement('button');
        cancelBtn.className = 'cancel-btn';
        cancelBtn.textContent = 'Отмена';
        cancelBtn.addEventListener('click', () => cancelEdit(li));

        // Добавляем кнопки в контейнер
        actionButtonsDiv.appendChild(editBtn);
        actionButtonsDiv.appendChild(deleteBtn);
        actionButtonsDiv.appendChild(saveBtn);
        actionButtonsDiv.appendChild(cancelBtn);

        // Собираем элемент списка
        li.appendChild(checkbox);
        li.appendChild(contentDiv);
        li.appendChild(editInputContainer); // Добавляем скрытый контейнер
        li.appendChild(actionButtonsDiv);

        return li;
    }

    const showLoading = () => {
        todoList.innerHTML = '<li class="loading">Загрузка...</li>';
    };

    const showError = (message) => {
        todoList.innerHTML = `<li class="loading" style="color: red;">${message}</li>`;
    };

    const updateFilterButtons = () => {
        filterAllBtn.classList.toggle('active', currentFilter === null);
        filterActiveBtn.classList.toggle('active', currentFilter === false);
        filterDoneBtn.classList.toggle('active', currentFilter === true);
    };

    // --- Функции для управления режимом редактирования ---

    const startEditing = (liElement, currentTitle, currentDescription) => {
        // Сначала отменим редактирование для других элементов
        cancelAnyActiveEdit(liElement);

        liElement.classList.add('editing');

        // Находим поля ввода внутри этого li
        const titleInput = liElement.querySelector('.edit-title-input');
        const descriptionTextarea = liElement.querySelector('.edit-desc-textarea');

        // Заполняем поля текущими значениями
        titleInput.value = currentTitle;
        descriptionTextarea.value = currentDescription;

        titleInput.focus(); // Ставим фокус на поле заголовка
    };

    const cancelEdit = (liElement) => {
        liElement.classList.remove('editing');
        // Очистка полей не обязательна, т.к. они скроются,
        // но можно сделать для порядка
        // const titleInput = liElement.querySelector('.edit-title-input');
        // const descriptionTextarea = liElement.querySelector('.edit-desc-textarea');
        // titleInput.value = '';
        // descriptionTextarea.value = '';
    };

    // Функция для отмены редактирования любого активного элемента
    const cancelAnyActiveEdit = (excludeLiElement = null) => {
        const currentlyEditing = todoList.querySelector('li.editing');
        if (currentlyEditing && currentlyEditing !== excludeLiElement) {
            cancelEdit(currentlyEditing);
        }
    }

    // --- Обработчики событий ---

    addTodoForm.addEventListener('submit', (e) => {
        e.preventDefault();
        const title = titleInput.value.trim();
        const description = descriptionInput.value.trim();
        if (title) {
            addTodo(title, description);
        } else {
            alert('Название задачи не может быть пустым!');
        }
    });

    filterAllBtn.addEventListener('click', () => {
        currentFilter = null;
        updateFilterButtons();
        fetchTodos(currentFilter);
    });

    filterActiveBtn.addEventListener('click', () => {
        currentFilter = false;
        updateFilterButtons();
        fetchTodos(currentFilter);
    });

    filterDoneBtn.addEventListener('click', () => {
        currentFilter = true;
        updateFilterButtons();
        fetchTodos(currentFilter);
    });

    // --- Начальная загрузка ---
    updateFilterButtons();
    fetchTodos(currentFilter);
});