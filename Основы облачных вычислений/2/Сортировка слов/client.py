import socket

def start_client(host="127.0.0.1", port=12345):
    """Запускает клиентскую часть"""
    while True:
        text = input("Введите текст: ").strip()
        if text == "":
            break

        client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            client_socket.connect((host, port))
            client_socket.send(text.encode("utf-8"))  # Отправляем текст
            sorted_words = client_socket.recv(4096).decode("utf-8")  # Получаем ответ
            print("\nОтсортированные слова:\n" + sorted_words)
        except ConnectionError:
            print("Ошибка соединения с сервером")
        finally:
            client_socket.close()

if __name__ == "__main__":
    start_client()
