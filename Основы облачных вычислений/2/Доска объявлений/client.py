import socket

def start_client(host="127.0.0.1", port=12345):
    """Запускает клиент"""
    while True:
        message = input("Введите сообщение (\"LIST\" для просмотра, пустая строка для выхода): ").strip()
        if message == "":  # Выход
            break

        client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        try:
            client_socket.connect((host, port))
            client_socket.send(message.encode("utf-8"))
            response = client_socket.recv(4096).decode("utf-8")
            print("\nОтвет сервера:\n" + response)
        except ConnectionError:
            print("Ошибка соединения с сервером")
        finally:
            client_socket.close()

if __name__ == "__main__":
    start_client()
