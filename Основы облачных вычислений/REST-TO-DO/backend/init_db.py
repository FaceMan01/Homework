from database import engine, Base
from models import Todo # Важно импортировать модель, чтобы она была зарегистрирована в Base.metadata

print("Создание таблиц в базе данных...")
# Создает все таблицы, определенные в Base.metadata (в нашем случае только 'todos')
Base.metadata.create_all(bind=engine)
print("Таблицы успешно созданы.")

if __name__ == "__main__":
    # Этот блок не обязателен, просто чтобы показать, что скрипт отработал
    print("Скрипт инициализации БД завершен.")