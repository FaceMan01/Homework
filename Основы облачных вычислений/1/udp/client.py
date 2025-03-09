import socket

def start_udp_client():
    host = '127.0.0.1'  # адрес сервера
    port = 12345        # порт сервера
    server_address = (host, port)
    
    client_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    client_socket.settimeout(5)  # установка таймаута ожидания ответа
    
    try:
        # Отправляем команду для начала сессии
        client_socket.sendto(b"start", server_address)
        
        # Ожидаем получения приглашения от сервера
        data, _ = client_socket.recvfrom(1024)
        prompt = data.decode('utf-8')
        print(prompt, end="")
        
        # Читаем имя пользователя с клавиатуры
        name = input()
        
        # Отправляем имя серверу (с завершающим символом новой строки)
        client_socket.sendto((name + "\n").encode('utf-8'), server_address)
        
        # Получаем приветственное сообщение от сервера
        data, _ = client_socket.recvfrom(1024)
        welcome_message = data.decode('utf-8')
        print(welcome_message)
    except socket.timeout:
        print("No response from server")
    finally:
        client_socket.close()

if __name__ == '__main__':
    start_udp_client()
