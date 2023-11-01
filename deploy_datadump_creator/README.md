Build base Docker image:

```bash

docker build -f Dockerfile_base -t antonbiatov/base_gbifwiever_datadamp_creator .

```

Test run image

```bash

docker run -it --rm --name gbif_datadump_creator -v "$(pwd)/01_data:/home/input_data" -v "$(pwd)/03_output:/home/output_data" -v "$(pwd)/scripts:/home/app" antonbiatov/base_gbifwiever_datadamp_creator

```

Run inside conteiner

```bash

${APP_DIR}/execute.sh

```




Build product Docker image:

```bash

docker build -t antonbiatov/gbifwiever_datadamp_creator .

```

Run container based on our image


Need to write absolute pathes.

```bash

docker run -it --rm -v C:/Users/admin/Documents/GitHub/gbif_shiny_onlineviewer/container_datadump/01_data:/home/input_data/ -v C:/Users/admin/Documents/GitHub/gbif_shiny_onlineviewer/container_datadump/03_output:/home/output_data/ antonbiatov/gbifwiever_datadamp_creator

```

Run it in terminal

```r

source("02_code/myScript.R")

```


