# Local Environment
A localized replica of the Github CI environment used for ITMO Year1 C++ course, which you can use to test your lab submissions locally (so that you don't use up precious Github CI AND your own time).  
[По-русски](#я-русский)

## Usage
Since this environment is mostly based around Docker containers, you should first set it up on your PC. Download Docker for your OS here - [Get Docker](https://docs.docker.com/get-docker/). As well as the download itself, there's also a sidebar on the left with a small tutorial, so that you know how to get started. After the installation, there shouldn't be any need to actually control Docker yourself (other than actually launching it), everything is done through two scripts present in this repository - **build.py** and **run.py**.

### 1. Launch build.py
```bash
python3 build.py # Or python build.py, or ./build.py
```
This script will build the **cpp-env** container, the environment of which matches that of Github CI. This is where the compilation and testing of your program will take place.

### Don't forget to launch build.py after updating this repo (git pull)!!!

### 2. Launch run.py with your finished project (repository)
```bash
python3 run.py test /local/path/to/repo # naturally, this should be replaced with the local path to your project
```
As a concrete example:
```bash
python3 run.py test ~/itmo/cpp/monte-carlo
```

## Functionality
Any usage of the **run.py** script after a successful Docker-container build with **build.py** looks something like this:
```bash
python3 run.py {command} {path to repository}
```
Where {command} is one of:
1. **test** - compiles the project with ASAN ([address sanitizer](https://clang.llvm.org/docs/AddressSanitizer.html)) and USAN ([undefined behaviour sanitizer](https://clang.llvm.org/docs/UndefinedBehaviorSanitizer.html)) sanitizers, launches tests
2. **build** - simply builds the projects without any sanitizers or further testing
3. **fmt** - formats the project with existing .clang-format in the root directory of your repo (normally this should be the one specified in the base repo of the homework, and pushing code without this formatting will cause an error during Github CI execution)  
   You can also specify the path to a different .clang-format:
   ```bash
   python3 run.py fmt ../.my-clang-format ../lfru-buddy
   ```
5. **checkfmt** - verifies that the project meets the requirements imposed by .clang-format of the repo
6. **send** - run tests, format code and automatically push all changes into the currently open branch of the repository

## Purpose
Organisations on github have a limited number of minutes available for use by Github Actions, which are used each time you push your changes into a repository. You should **always** first test your code locally using this environment, and push into the actual github repository **after** all tests and checks pass.

# Я русский
## Использование
Так как это окружение основано в частности на использовании Docker-контейнера, для начала необходимо установить его себе на компьютер.
Скачиваем Docker для своей ОС здесь - [Get Docker](https://docs.docker.com/get-docker/), тут же слева есть небольшой туториал, чтобы было, с чего начать.
После установки Docker особо никаких дальнейших взаимодействий с ним вручную производить не нужно (кроме как запустить его),
всё управление делается с помощью двух файлов в этом репозитории - **build.py** и **run.py**.

### 1. Запускаем build.py:
```bash
python3 build.py # Или же python build.py, или же ./build.py
```
Этот скрипт соберёт контейнер **cpp-env**, окружение которого совпадает тем, что у Github CI. В нём же и будет проходить компиляция и тесты.

### Не забываем запустить build.py при обновлении (git pull)!!!

### 2. Натравляем run.py на наш выполненный проект (репозиторий):
```bash
python3 run.py test /локальный/путь/к/репозиторию # естественно это заменяем на локальный путь до того репозитория, где выполнили проект
```
Конкретнее, например:
```bash
python3 run.py test ~/itmo/cpp/monte-carlo
```
Это запустит Docker контейнер cpp-env, который скомпилирует код в вашем репозитории и запустит тесты. 

## Функционал
Любое использование скрипта **run.py** после сборки Docker-контейнера с помощью **build.py** выглядит как:
```bash
python3 run.py {команда} {путь к репозиторию}
```
Где команда это одно из:
1. **test** - компилирует проект с ASAN ([address sanitizer](https://clang.llvm.org/docs/AddressSanitizer.html)), USAN ([undefined behaviour sanitizer](https://clang.llvm.org/docs/UndefinedBehaviorSanitizer.html)) санитайзерами, прогоняет тесты
2. **build** - просто проверит компиляцию проекта без санитайзеров
3. **fmt** - отформатирует проект под .clang-format, указанный в репозитории (обычно это общий формат кода для конкретного задания, часто несоблюдение формата вызовет ошибку при пуше кода на гитхаб)  
   Дополнительно указав путь до другого файла .clang-format можно отформатировать в свой формат:
   ```bash
   python3 run.py fmt ../.my-clang-format ../lfru-buddy
   ```
5. **checkfmt** - проверит, соответствует ли код формату, указанному в .clang-format репозитория
6. **send** - прогонит тесты, отформатирует код, после чего запушит его на гитхаб в ту ветку, в которой на данной момент находится локальный репозиторий

## Зачем
У организаций на github (например, itiviti-cpp-2021) есть ограниченное кол-во минут, которые израсходуются каждый раз,
когда вы загружаете свой код в данный репозиторий и запускается долгий процесс компиляции и тестирования. Для этого **обязательно** всегда тестируйте сначала локально, с помощью этого окружения.

Только после успешного прохождения **всех тестов и проверки форматирования** стоит push'ить свой локальный репозиторий в github.
