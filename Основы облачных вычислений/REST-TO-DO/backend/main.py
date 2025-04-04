from fastapi import FastAPI, Depends, HTTPException, status, Query
from fastapi.middleware.cors import CORSMiddleware # <--- Импортируйте
from sqlalchemy.orm import Session
from typing import List, Optional

import crud, models, schemas
from database import SessionLocal, engine, get_db # Импортируем get_db

# Создаем таблицы в БД (если их еще нет)
# В реальном приложении лучше использовать Alembic для миграций
# models.Base.metadata.create_all(bind=engine) # Можно раскомментировать для простоты, но init_db.py - лучший подход

app = FastAPI(
    title="Todo API",
    description="Простой API для управления списком дел",
    version="0.1.0",
)

# --- НАСТРОЙКА CORS ---
# Список источников (origins), которым разрешен доступ
origins = [
    "http://localhost", # Если вы будете запускать фронтенд через локальный сервер
    "http://localhost:8080", # Пример другого порта
    "http://127.0.0.1",
    "http://127.0.0.1:8080", # Пример другого порта
    "null",  # <--- ВАЖНО: Для запросов от локальных файлов (file://)
    # Можно добавить "*" для разрешения всех источников, но это менее безопасно для продакшена
    "*"
]

app.add_middleware(
    CORSMiddleware,
    allow_origins=origins, # Разрешенные источники
    allow_credentials=True, # Разрешить куки (если они используются)
    allow_methods=["*"],    # Разрешить все методы (GET, POST, PUT, DELETE и т.д.)
    allow_headers=["*"],    # Разрешить все заголовки
)

# --- Эндпоинт для создания задачи ---
@app.post("/todos/", response_model=schemas.Todo, status_code=status.HTTP_201_CREATED, tags=["Todos"])
def create_todo_endpoint(
    todo: schemas.TodoCreate, # Данные из тела запроса, валидированные по схеме TodoCreate
    db: Session = Depends(get_db) # Инъекция зависимости - сессия БД
):
    """
    Создать новую задачу.
    - **title**: Название задачи (обязательно)
    - **description**: Описание (опционально)
    - **is_done**: Статус выполнения (по умолчанию false)
    """
    return crud.create_todo(db=db, todo=todo)

# --- Эндпоинт для получения списка задач ---
@app.get("/todos/", response_model=List[schemas.Todo], tags=["Todos"])
def read_todos_endpoint(
    skip: int = Query(0, ge=0, description="Сколько записей пропустить (для пагинации)"),
    limit: int = Query(100, ge=1, le=200, description="Максимальное количество записей для возврата"),
    is_done: Optional[bool] = Query(None, description="Фильтровать по статусу выполнения"),
    db: Session = Depends(get_db)
):
    """
    Получить список задач с возможностью пагинации и фильтрации по статусу.
    """
    todos = crud.get_todos(db, skip=skip, limit=limit, is_done=is_done)
    return todos

# --- Эндпоинт для получения одной задачи по ID ---
@app.get("/todos/{todo_id}", response_model=schemas.Todo, tags=["Todos"])
def read_todo_endpoint(
    todo_id: int, # Параметр пути
    db: Session = Depends(get_db)
):
    """
    Получить одну задачу по её уникальному идентификатору (ID).
    """
    db_todo = crud.get_todo(db, todo_id=todo_id)
    if db_todo is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Todo not found")
    return db_todo # FastAPI автоматически преобразует models.Todo в schemas.Todo благодаря orm_mode

# --- Эндпоинт для обновления задачи ---
@app.put("/todos/{todo_id}", response_model=schemas.Todo, tags=["Todos"])
def update_todo_endpoint(
    todo_id: int,
    todo_update: schemas.TodoUpdate, # Данные из тела запроса для обновления
    db: Session = Depends(get_db)
):
    """
    Обновить существующую задачу по её ID.
    Можно обновлять только нужные поля (title, description, is_done).
    """
    db_todo = crud.get_todo(db, todo_id=todo_id)
    if db_todo is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Todo not found")
    updated_todo = crud.update_todo(db=db, db_todo=db_todo, todo_update=todo_update)
    return updated_todo

# --- Эндпоинт для удаления задачи ---
@app.delete("/todos/{todo_id}", status_code=status.HTTP_204_NO_CONTENT, tags=["Todos"])
def delete_todo_endpoint(
    todo_id: int,
    db: Session = Depends(get_db)
):
    """
    Удалить задачу по её ID.
    Возвращает статус 204 No Content в случае успеха.
    """
    db_todo = crud.get_todo(db, todo_id=todo_id)
    if db_todo is None:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND, detail="Todo not found")
    crud.delete_todo(db=db, db_todo=db_todo)
    # При статусе 204 тело ответа не отправляется, поэтому можно ничего не возвращать (return None или просто закончить функцию)
    return None

# Добавим простой корневой эндпоинт для проверки работы
@app.get("/", tags=["Root"])
def read_root():
    return {"message": "Welcome to the Todo API!"}