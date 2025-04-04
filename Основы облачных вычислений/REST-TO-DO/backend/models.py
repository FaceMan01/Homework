from sqlalchemy import Column, Integer, String, Boolean, DateTime, func
from database import Base

class Todo(Base):
    __tablename__ = "todos" # Имя таблицы в БД

    id = Column(Integer, primary_key=True, index=True)
    title = Column(String, index=True, nullable=False)
    description = Column(String, nullable=True)
    is_done = Column(Boolean, default=False, nullable=False)
    created_at = Column(DateTime(timezone=True), server_default=func.now(), nullable=False)
    updated_at = Column(DateTime(timezone=True), onupdate=func.now(), server_default=func.now(), nullable=False)

    def __repr__(self):
        return f"<Todo(id={self.id}, title='{self.title}', is_done={self.is_done})>"