from pydantic import BaseModel, Field
from typing import Optional
from datetime import datetime

# --- Базовая схема DTO ---
# Общие поля, которые есть во всех DTO, связанных с Todo
class TodoBase(BaseModel):
    title: str = Field(..., min_length=1, max_length=100, description="Название задачи")
    description: Optional[str] = Field(None, max_length=500, description="Описание задачи")
    is_done: bool = Field(default=False, description="Статус выполнения задачи")

    # Пример конфигурации для Swagger UI
    class Config:
        schema_extra = {
            "example": {
                "title": "Купить молоко",
                "description": "Не забыть проверить срок годности",
                "is_done": False
            }
        }

# --- DTO для создания ---
# Используется для валидации данных в POST запросе
class TodoCreate(TodoBase):
    pass # Наследует все поля от TodoBase

# --- DTO для обновления ---
# Используется для валидации данных в PUT/PATCH запросе
# Все поля опциональны, т.к. мы можем обновлять только часть данных
class TodoUpdate(BaseModel):
    title: Optional[str] = Field(None, min_length=1, max_length=100, description="Новое название задачи")
    description: Optional[str] = Field(None, max_length=500, description="Новое описание задачи")
    is_done: Optional[bool] = Field(None, description="Новый статус выполнения задачи")

    class Config:
        schema_extra = {
            "example": {
                "title": "Купить свежее молоко",
                "is_done": True
            }
        }

# --- DTO для чтения (ответа API) ---
# Включает поля, которые мы хотим вернуть клиенту (включая id и таймстемпы)
class Todo(TodoBase):
    id: int
    created_at: datetime
    updated_at: datetime

    # Эта конфигурация позволяет Pydantic работать с объектами SQLAlchemy
    class Config:
        orm_mode = True # Позволяет читать данные прямо из ORM модели (models.Todo)