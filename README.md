# Эндпойнты

- /test - метод: GET, для проверки работоспособности приложения

- /feedback - метод: POST, в теле ожидает объект с полями name, phone, email(необязательно), message, channel - тг канал, куда отправлять(значение для channel можно получить в свойстве телеграм канала "ссылка", например: { channel: "@chatdeni2" })


# Подготовка

1. Токен телеграм бота должен быть указан в переменной FEEDBACK_BOT.

2. Создать нужное кол-во каналов в телеграм. Добавить бота в качестве администратора канала.

Первоначальное задание по боту - в файле task.txt

Server.hs - API сервер. Запускается на порту 8085 и прослушивает эндпойнт (метод POST)
http://localhost:8085/feedback, в теле которого ожидает поля email и message

# Использование
```
const object = {
    email, 
    message, 
    name, 
    phone, 
    channel: "@chatdeni2" // идентификатор ТГ канала
    // channel: "@chatdeni @chatdeni2" // несколько ТГ каналов
}

fetch("http://localhost:8085/feedback", {
    method: "POST",
    headers: {
        "Content-Type": "application/json",
    },
    body: JSON.stringify(object),
}).then(r => r.json()).then(console.log)
```

Для проверки работы можно использовать:

https://github.com/DeniLark/form-feedback-bot
