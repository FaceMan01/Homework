import grpc
from concurrent import futures
import time

import cities_pb2
import cities_pb2_grpc

class CitiesGameServicer(cities_pb2_grpc.CitiesGameServicer):

    def __init__(self):
        self.cities_list = {  # Список городов (множество для быстрого поиска)
            "москва", "санкт-петербург", "новосибирск", "екатеринбург", "новгород",
            "казань", "челябинск", "омск", "самара", "ростов-на-дону",
            "уфа", "красноярск", "пермь", "воронеж", "волгоград",
            "краснодар", "саратов", "тюмень", "тольятти", "ижевск",
            "барнаул", "ульяновск", "владивосток", "ярославль", "хабаровск",
            "иркутск", "кемерово", "липецк", "пенза", "рязань",
            "томск", "астрахань", "курск", "орёл", "тверь",
            "тула", "чебоксары", "чита", "элиста", "якутск"
        }
        self.used_cities = set()
        self.last_letter = ''
        self.game_over = False

    def StartGame(self, request, context):
        print("Новая игра запущена сервером.")
        self.used_cities = set()  # Сброс использованных городов для новой игры
        self.last_letter = ''
        self.game_over = False
        return cities_pb2.StartGameResponse(message="Игра 'Города' началась! Первый ход за вами.")

    def MakeMove(self, request, context):
        city = request.cityName.lower()
        print(f"Получен ход от клиента: {city}")

        if self.game_over:
            return cities_pb2.MakeMoveResponse(
                isValidMove=False,
                errorMessage="Игра уже окончена. Начните новую игру.",
                gameOver=True
            )

        if not city.isalpha():
            return cities_pb2.MakeMoveResponse(
                isValidMove=False,
                errorMessage="Ошибка: В названии города должны быть только буквы.",
                nextLetter=self.last_letter.upper() if self.last_letter else '',
                gameOver=False
            )

        if city not in self.cities_list:
            return cities_pb2.MakeMoveResponse(
                isValidMove=False,
                errorMessage="Ошибка: Такого города нет в списке доступных городов.",
                nextLetter=self.last_letter.upper() if self.last_letter else '',
                gameOver=False
            )

        if city in self.used_cities:
            return cities_pb2.MakeMoveResponse(
                isValidMove=False,
                errorMessage="Ошибка: Этот город уже был назван.",
                nextLetter=self.last_letter.upper() if self.last_letter else '',
                gameOver=False
            )

        if self.last_letter and city[0] != self.last_letter:
            return cities_pb2.MakeMoveResponse(
                isValidMove=False,
                errorMessage=f"Ошибка: Город должен начинаться на букву '{self.last_letter.upper()}'",
                nextLetter=self.last_letter.upper() if self.last_letter else '',
                gameOver=False
            )

        self.cities_list.remove(city)
        self.used_cities.add(city)

        valid_last_letter = ''
        for char in reversed(city):
            if char not in ['ь', 'ъ', 'ы']:
                valid_last_letter = char
                break

        if not valid_last_letter:
            return cities_pb2.MakeMoveResponse(
                isValidMove=False,
                errorMessage="Ошибка: Город не подходит для определения следующей буквы (заканчивается на ь, ъ, ы). Попробуйте другой город.",
                nextLetter=self.last_letter.upper() if self.last_letter else '',
                gameOver=False
            )

        self.last_letter = valid_last_letter

        # Проверка, остались ли города на следующую букву (простая проверка, можно улучшить)
        possible_next_cities = False
        for c in self.cities_list:
            if c.startswith(self.last_letter):
                possible_next_cities = True
                break
        if not possible_next_cities:
            self.game_over = True
            return cities_pb2.MakeMoveResponse(
                isValidMove=True,
                nextLetter='',
                gameOver=True,
                errorMessage="Игра окончена! Вы победили, назвав все возможные города на букву '{}'.".format(self.last_letter.upper())
            )


        return cities_pb2.MakeMoveResponse(
            isValidMove=True,
            nextLetter=self.last_letter.upper(),
            gameOver=False
        )


def serve():
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    cities_pb2_grpc.add_CitiesGameServicer_to_server(CitiesGameServicer(), server)
    server.add_insecure_port('[::]:50052') # Используем другой порт, например 50052
    server.start()
    print("Сервер игры 'Города' запущен на порту 50052...")
    try:
        while True:
            time.sleep(86400)
    except KeyboardInterrupt:
        server.stop(0)

if __name__ == '__main__':
    serve()