# Biodiversity Viewer. Module: Web Viewer

This repository provides several use cases for the application.
1. Use the application to work on your local computer;
2. Publish the application using Shiny Server Docker image;
3. Publish the application using Shiny Docker image to run using Shiny proxy.

## Use the application to work on your local computer

Application files are located in the [`app`](app) folder.


## Shiny Server

### Shiny Server Docker image

Build Web Biodiversity Viewer shiny server Docker image

```bash

docker build --file ./shiny_server.dockerfile -t biodiversity_viewer_shiny_server .

```

Run biodiversityviewer docker image

```bash

docker run --rm -d --name shinyserver -p 3838:3838 -v "$(pwd)/shiny_server_log:/var/log/shiny-server" -v "gbif_data:/srv/shiny-server/gbif_data" biodiversity_viewer_shiny_server

# TODO добавить монтирование докер тома с данными

```


## ShinyProxy

![Shinyproxy workflow](Biodiversity_viewer_shinyproxy_scheme.png)

### Build Shiny Docker image to run in ShinyProxy

```bash

docker build --file ./shiny_app.dockerfile -t biodiversity_viewer_shiny_app .

```

Test run shiny app

```bash

docker run --rm -d --name biodiversityviewershinyapp -p 3838:3838 biodiversity_viewer_shiny_app

```

### Get ShinyProxy

```bash

docker build --file ./shinyproxy.dockerfile -t biodiversity_viewer_shinyproxy .

```

Test run shinyproxy

```bash

docker run --rm -it --name shinyproxy -p 8080:8080 biodiversity_viewer_shinyproxy

```

Run SinyProxy 

```bash

docker run -d -v /var/run/docker.sock:/var/run/docker.sock --net sp-example-net --name shinyproxy -p 8080:8080 biodiversity_viewer_shinyproxy

```

