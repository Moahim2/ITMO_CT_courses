# 1ch

## Описание

### Что надо

- Окно с сообщениями. В качестве примера посмотрите на чат в Telegram.
  - Все сообщения имеют
    - Отправителя <br/>
      Для ваших сообщений возьмите, например, ник на github
    - Получателя <br/> 
      На данный момент единственно возможное значение — `1@channel`. 
      Можно не отображать
    - Время отправки в UNIX Time
    - Содержимое
      - Текст <br/> Просто показываем
      - Картинку <br/> Показываем, по клику подгружаем в нормальном разрешении
        в отдельную Activity
- Поле для ввода текста с кнопкой отправки.

### Бонус

- +1 очко тому, кто догадается, как прикрутить отправку картинки с диска

### Критерии провала

- Не работает с сетью
- Теряет данные при повороте (другой смене конфигурации)
- Падает
- Использует __любую__ стороннюю библиотеку для работы с сетью и управления потоками
  - В том числе `kotlinx.coroutines`
- Data race
- Утечка ресурсов
  - Внимательно следите за лямбдами
- `package com.example`

### Что предполагается использовать

- `Service`
  - Можно `IntentService`, даже несмотря на то, что он немного `@Deprecated`
- `Handler`
- `Looper`
- `Intent` для передачи данных между компонентами
  - Да, `startActivity` __необязательно__ создаёт новую activivty, см. `onNewIntent`
  - То же верно и для `startService`
- `org.json` для разбора и создания JSON
- `java.net.HttpURLConnection` для работы с сетью

### Штрафы

- Все warning, кроме
  - IntentService deprecation
  - Accessability issues
  - Устаревшие механизмы работы с файлами при условии выполнения бонуса
- [Gson](https://github.com/google/gson)

### За что?..

Проблемы утечек и синхронизации нагляднее всего видны на практически голых
механизмах без магии. Умение их находить и обходить будет полезно и при
работе с чуть более магическими инструментами типа
[Retrofit](https://square.github.io/retrofit/),
[RxJava](https://github.com/ReactiveX/RxJava),
[kotlinx.coroutines](https://kotlinlang.org/docs/coroutines-overview.html)
и [Room](https://developer.android.com/jetpack/androidx/releases/room).

## API

Хост лежит по адресу http://213.189.221.170:8008

- GET `/1ch` <br/>
  Список сообщений
  - `limit` <br/>
    Сколько сообщений отдавать, по умолчанию 20
  - `lastKnownId` <br/>
    Начиная с какого сообщения отдавать сообщения, по умолчанию 0
- GET `/img/<path>` <br/>
  Картиночка по указанному пути в хорошем разрешении
- GET `/thumb/<path>` <br/>
  Картиночка по указанному пути в шакальном разрешении
- POST `/1ch` с сообщением в body и `Content-Type=application/json`
    - `200 OK` и `id` поста
    - Ошибку, смотри коды состояния HTTP
- POST `/1ch` с сообщением в body и `Content-Type=multipart/form-data`
    - `200 OK` и `id` поста
    - Ошибку, смотри коды состояния HTTP
    - Пример смотри в `MultipartTool` и `MultipartExampleClient`
      - Да, их можно скопипастить в проект и использовать

Сообщение:
```json lines
{
  "id": "123", // optional for request
  "from": "my name", // mandatory
  "to": "1@ch", // optional for request, defaults to "1@channel"
  "data": {
    "Text": {
      "text": "my message" // mandatory
    },
    // OR (moreover, XOR)
    "Image": {
      "link": "path/to/pic.jpg" // optional for request
    }
  },
  "time": "1297490" // UNIX-time, optional for requests
}
```
