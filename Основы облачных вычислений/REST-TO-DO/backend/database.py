from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
import os

SQLALCHEMY_DATABASE_URL = "sqlite:///./todos.db"

engine = create_engine(
    SQLALCHEMY_DATABASE_URL, connect_args={"check_same_thread": False}
)

# Создаем фабрику сессий
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

# Базовый класс для наших моделей SQLAlchemy
Base = declarative_base()

# Функция-зависимость для получения сессии БД в запросах FastAPI
def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()