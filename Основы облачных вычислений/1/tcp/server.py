import socket

def recv_line(sock):
    """
    Читает данные из сокета до символа новой строки.
    """
    line = b""
    while True:
        char = sock.recv(1)  # читаем по одному байту
        if not char:
            break  # соединение закрыто
        # Если получен символ новой строки, завершаем чтение
        if char == b'\n':
            break
        # Игнорируем символ возврата каретки
        if char == b'\r':
            continue
        line += char
    return line

def start_server():
    host = '0.0.0.0'  # прослушивание на всех интерфейсах
    port = 12345      # можно выбрать любой свободный порт

    # Создаём TCP-сокет
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server_socket.bind((host, port))
    server_socket.listen(5)
    print(f"Server running on {host}:{port}")

    while True:
        client_socket, addr = server_socket.accept()
        print(f"Connected:{addr}")
        
        # Отправляем сообщение с запросом имени
        client_socket.send("Enter your name: ".encode('utf-8'))
        
        # Читаем строку до символа новой строки
        data = recv_line(client_socket)
        if not data:
            client_socket.close()
            continue
        
        name = data.strip().decode('utf-8')
        welcome_message = f"Hello, {name}!\n".encode('utf-8')
        
        # Отправляем приветственное сообщение
        client_socket.send(welcome_message)
        client_socket.close()

if __name__ == '__main__':
    start_server()
