# GBIF Viewer. Module: Web Viewer

This repository provides several use cases for the application.
1. Use the application to work on your local computer;
2. Publish the application using Shiny Server Docker image;
3. Publish the application using Shiny Docker image to run using Shiny proxy.

## Use the application to work on your local computer

Application files are located in the [`app`](app) folder.


## Shiny Server Docker image

Build Web GBIF Viewer shiny server Docker image

```bash

docker build --file ./shiny_server.dockerfile -t web_gbif_viewer_shiny_server .

```

Run webgbifviewer docker image

```bash

docker run --rm -d --name webgbifviewershinyserver -p 3838:3838 -v "$(pwd)/shiny_server_log:/var/log/shiny-server" web_gbif_viewer_shiny_server

# TODO добавить монтирование докер тома с данными

```


## Shiny Docker image to run using Shiny proxy

```bash

docker build --file ./shiny_app.dockerfile -t web_gbif_viewer_shiny_app .

```

Run shiny app

```bash

docker run --rm -d --name webgbifviewershinyapp -p 3838:3838 web_gbif_viewer_shiny_app

```



