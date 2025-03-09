import socket

def recv_line(sock):
    """
    Читает данные из сокета до символа новой строки.
    """
    line = b""
    while True:
        char = sock.recv(1)
        if not char:
            break  # соединение закрыто
        if char == b'\n':
            break  # достигнут конец строки
        if char == b'\r':
            continue  # пропускаем возврат каретки
        line += char
    return line.decode('utf-8')

def start_client():
    host = '127.0.0.1'  # адрес сервера (локальный для примера)
    port = 12345        # порт, на котором запущен сервер

    # Создаем TCP-сокет
    client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_socket.connect((host, port))
    
    # Получаем запрос имени от сервера
    prompt = client_socket.recv(1024).decode('utf-8')
    print(prompt, end="")

    # Читаем имя с клавиатуры
    name = input()

    # Отправляем имя серверу (добавляем символ новой строки для завершения ввода)
    client_socket.sendall((name + "\n").encode('utf-8'))
    
    # Получаем приветственное сообщение от сервера
    welcome_message = recv_line(client_socket)
    print(welcome_message)

    client_socket.close()

if __name__ == '__main__':
    start_client()
