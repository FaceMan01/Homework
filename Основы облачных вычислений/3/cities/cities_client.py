import grpc

import cities_pb2
import cities_pb2_grpc

def run_game():
    channel = grpc.insecure_channel('localhost:50052') # Подключаемся к серверу на порту 50052
    stub = cities_pb2_grpc.CitiesGameStub(channel)

    print("Добро пожаловать в клиент-серверную игру 'Города'!")

    start_response = stub.StartGame(cities_pb2.StartGameRequest())
    print(start_response.message)

    last_letter = ''
    while True:
        city = input(f"\nВаш ход. Введите город, начинающийся на букву '{last_letter.upper() if last_letter else 'любую'}': ").strip()

        if city.lower() == "/q":
            print("Вы вышли из игры.")
            break

        move_request = cities_pb2.MakeMoveRequest(cityName=city)
        move_response = stub.MakeMove(move_request)

        if move_response.isValidMove:
            print("Ход принят!")
            last_letter = move_response.nextLetter.lower()
            if move_response.gameOver:
                print(move_response.errorMessage)
                break
        else:
            print(f"Неверный ход: {move_response.errorMessage}")
            if last_letter:
                print(f"Город должен начинаться на букву '{last_letter.upper()}'")


if __name__ == '__main__':
    run_game()