from sqlalchemy.orm import Session
import models, schemas
from typing import List, Optional

# --- Получение одной задачи по ID ---
def get_todo(db: Session, todo_id: int) -> Optional[models.Todo]:
    return db.query(models.Todo).filter(models.Todo.id == todo_id).first()

# --- Получение списка задач с пагинацией и фильтрацией ---
def get_todos(
    db: Session,
    skip: int = 0,
    limit: int = 100,
    is_done: Optional[bool] = None
) -> List[models.Todo]:
    query = db.query(models.Todo)
    if is_done is not None:
        query = query.filter(models.Todo.is_done == is_done)
    return query.offset(skip).limit(limit).all()

# --- Создание новой задачи ---
def create_todo(db: Session, todo: schemas.TodoCreate) -> models.Todo:
    # Создаем объект SQLAlchemy модели из данных Pydantic DTO
    db_todo = models.Todo(
        title=todo.title,
        description=todo.description,
        is_done=todo.is_done
    )
    db.add(db_todo) # Добавляем в сессию
    db.commit()    # Сохраняем изменения в БД
    db.refresh(db_todo) # Обновляем объект db_todo данными из БД (например, чтобы получить id)
    return db_todo

# --- Обновление существующей задачи ---
def update_todo(db: Session, db_todo: models.Todo, todo_update: schemas.TodoUpdate) -> Optional[models.Todo]:
    # Получаем данные из DTO как словарь, исключая неустановленные значения
    update_data = todo_update.dict(exclude_unset=True)

    # Обновляем поля объекта SQLAlchemy
    for key, value in update_data.items():
        setattr(db_todo, key, value)

    db.add(db_todo) # Добавляем в сессию (хотя объект уже там, это помечает его как измененный)
    db.commit()    # Сохраняем изменения
    db.refresh(db_todo) # Обновляем объект
    return db_todo

# --- Удаление задачи ---
def delete_todo(db: Session, db_todo: models.Todo) -> models.Todo:
    db.delete(db_todo) # Помечаем на удаление
    db.commit()      # Сохраняем изменения
    return db_todo   # Возвращаем удаленный объект (он еще доступен до конца сессии)