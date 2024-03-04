- Команды:
   - Команда пишется в запущенном cmd клиенте следующим образом:
        COMMAND_TYPE arguments
   - LOGIN 'login' 'password' (обязательно для успешного выполнения других команд)
   - UPLOAD 'absolute file name' 'full name directory on server' (доступ у пользователя как к корню на сервере - только к своей личной директории)
   - DOWNLOAD 'full file name on server' 'absolute directory name for file saving'
   - COPY 'old full file name' 'new full directory name'
   - MOVE 'old full file name' 'new full directory name'
   - Пример1 : LOGIN fff fff - авторизует вас на сервере
   - Пример2 : UPLOAD C:\Users\Ivanl\testFile hellodir - загружает ваш файл на сервер по пути userdata\fff\hellodir\testFile
   - В fxml версии по сути все аналогично, но имя команды писать не нужно - она выбирается в меню, и поля для ввода разделены под аргументы.


- Сервер:
    - Запускать из корня проекта в cmd командой
        - ./gradlew server:run --args='arg1 arg2 arg3'
        - где:
            - arg1 - hostname (string)
            - arg2 - port (int)
            - arg3 - on/off logs (0 or 1)
        - ! можно не указывать флаг --args 
            - по дефолту
              - arg1 = "localhost"
              - arg2 = "8080"
              - arg3 = "1" (on)

- Клиент (по собственному желанию реализованы две версии UI : CMD и FXML):
    1) Для запуска cmd версии введите
        - ./gradlew client:runCMDClient --console=plain --args='arg1 arg2'
        - ! --console=plain обязательный флаг (для инпута с консоли)
    2) Для запуска fxml версии введите
        - ./gradlew client:run --args='arg1 arg2'
            - где: 
                - arg1 - hostname (string)
                - arg2 - port (int)
            - ! можно не указывать флаг --args (дефолт тот же, что у сервера)
- P.S. :
  - Пока что есть только 3 корректных пары логин-пароль:
    - "IvanLevitskiy", "Moahim2003",
    - "fff", "fff",
    - "OldUser", "123",
  
