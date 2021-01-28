# Сервис формирования текстовых документов на основе mustache-шаблонов

## Запуск сервиса

```
mkdir -p must-render/conf/templates
cd must-render
```

- В директории `must-render/conf/templates` размещаем файлы шаблонов.

- В директории `must-render/conf` создаем файл `config.yaml` следующего содержания:

```yaml
templates:
  - template_1.mustache
  - template_2.mustache
```

где `template_1.mustache` и `template_2.mustache` - имена главных файлов шаблонов (файлы, импортируемые в главные перечислять не нужно).

В директории `must-render` создаем файл `docker-compose.yml` следующего содержания (при необходимости можно добавить конфигурацию сети и т.п.):

```yaml
version: "3"

services:
  must-render:
    image: <image>
    container_name: must-render
    volumes:
      - ./conf:/opt/app/conf
    ports:
      - 7777:7777 # Если нужно, то пробрасываем порт 7777 на хост
```

Далее выполняем:

```
docker-compose up -d
```

## Формирование документа

Для формирования документа необходимо выполнить запрос следующего вида:

```http
POST http://service:port?t=template_1.mustache
Content-Type: application/json

{
    "param1": "value1",
    "param2": "value2"
}
```

Готовый документ будет в теле ответа.
