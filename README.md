# checkers


[![Build Status](https://travis-ci.org/cmc-haskell-2016/checkers.svg?branch=master)](https://travis-ci.org/cmc-haskell-2016/checkers)

## Скриншоты

### Игра
![Игра](screenshots/game.png)

### Стартовая страница
![Стартовая страница](screenshots/startpage.png)

### Меню статистики
![Меню статистики](screenshots/statistics.png)



## Установка и запуск

Для установки клонируйте репозиторий и соберите проект с помощью `stack`:

```
git clone https://github.com/cmc-haskell-2016/checkers.git
cd checkers
stack setup
stack build
```

После установки запуск осуществляется командой `stack exec`:

```
stack exec checkers
```

Во время разработки инициировать повторную сборку проекта с последующим запуском рекомендуется
следующей командой:

```
stack build && stack exec checkers
```
