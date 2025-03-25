import socket
import threading

class WordSorter:
    """Класс для сортировки слов в алфавитном порядке без повторений"""
    @staticmethod
    def process_text(text):
        words = set(text.lower().split())  # Приводим к нижнему регистру и убираем дубликаты
        sorted_words = sorted(words)  # Сортируем
        return "\n".join(sorted_words)  # Возвращаем результат строкой

def handle_client(client_socket):
    """Обрабатывает подключение клиента"""
    try:
        data = client_socket.recv(4096).decode("utf-8")  # Получаем текст
        if data:
            sorted_words = WordSorter.process_text(data)  # Обрабатываем текст
            client_socket.send(sorted_words.encode("utf-8"))  # Отправляем результат
    finally:
        client_socket.close()  # Закрываем соединение

def start_server(host="127.0.0.1", port=12345):
    """Запускает сервер"""
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.bind((host, port))
    server_socket.listen(5)  # Максимум 5 одновременных подключений
    print(f"Сервер запущен на {host}:{port}")

    while True:
        client_sock, addr = server_socket.accept()
        print(f"Подключение от {addr}")
        client_handler = threading.Thread(target=handle_client, args=(client_sock,))
        client_handler.start()  # Запускаем обработку клиента в отдельном потоке

if __name__ == "__main__":
    start_server()
