import socket
import threading

ADS_FILE = "ads.txt"

def load_ads():
    """Загружает объявления из файла"""
    try:
        with open(ADS_FILE, "r", encoding="utf-8") as file:
            return file.readlines()
    except FileNotFoundError:
        return []

def save_ad(ad_text):
    """Сохраняет объявление в файл"""
    with open(ADS_FILE, "a", encoding="utf-8") as file:
        file.write(ad_text + "\n")

def handle_client(client_socket):
    """Обрабатывает запрос клиента"""
    try:
        while True:
            data = client_socket.recv(1024).decode("utf-8").strip()
            if not data:  # Если пустая строка - разрываем соединение
                break

            if data.upper() == "LIST":
                ads = load_ads()
                response = "".join(ads) if ads else "No ads available."
            else:
                save_ad(data)
                response = f'Message added: "{data}"'

            client_socket.send(response.encode("utf-8"))
    finally:
        client_socket.close()

def start_server(host="127.0.0.1", port=12345):
    """Запускает сервер"""
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.bind((host, port))
    server_socket.listen(5)
    print(f"Server started on {host}:{port}")

    while True:
        client_sock, addr = server_socket.accept()
        print(f"Connected: {addr}")
        threading.Thread(target=handle_client, args=(client_sock,)).start()

if __name__ == "__main__":
    start_server()
