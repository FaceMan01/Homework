import socket

def start_udp_server():
    host = '0.0.0.0'
    port = 12345
    server_socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    server_socket.bind((host, port))
    print(f"UDP server is running on {host}:{port}")
    
    # Словарь для хранения состояний клиентов.
    # По ключу (адрес клиента) будем отслеживать, ожидаем ли мы от него имя.
    client_states = {} 
    
    while True:
        data, client_address = server_socket.recvfrom(1024)
        message = data.decode('utf-8').strip()
        
        # Если клиента нет в словаре, считаем, что это начало сессии
        if client_address not in client_states:
            # Если клиент отправил "start", посылаем приглашение ввести имя
            if message.lower() == "start":
                client_states[client_address] = "awaiting_name"
                server_socket.sendto("Enter your name: ".encode('utf-8'), client_address)
            else:
                # Если сразу отправлено имя, приветствуем клиента
                welcome_message = f"Hello, {message}!\n".encode('utf-8')
                server_socket.sendto(welcome_message, client_address)
        else:
            # Клиент уже получил приглашение – ожидаем его имя
            if client_states[client_address] == "awaiting_name":
                welcome_message = f"Hello, {message}!\n".encode('utf-8')
                server_socket.sendto(welcome_message, client_address)
                # Завершаем сессию для данного клиента
                del client_states[client_address]

if __name__ == '__main__':
    start_udp_server()
