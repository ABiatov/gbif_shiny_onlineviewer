Build base Docker image:

```bash

docker build -f Dockerfile_base -t antonbiatov/base_gbifwiever_datadamp_creator .

```

Test run image

```bash

docker run -it --rm --name gbif_datadump_creator -v "$(pwd)/data:/home/data" -v "$(pwd)/outputs:/home/outputs" -v "$(pwd)/scripts:/home/scripts" -v "$(pwd)/temp:/home/temp" antonbiatov/base_gbifwiever_datadamp_creator

```

Run inside conteiner

```bash

${APP_DIR}/execute.sh

```

# Note

The script [**1_data_preparation.R**](scripts/1_data_preparation.R) for manual running. Don't use it on server.

> scripts/1_data_preparation.R 


Build product Docker image:


Before build product Docker image, you need to change file [`gbif_ini.R`](scripts/gbif_ini.R) You need to add your GBIF credentials into this file. 

> gbif_user = "your_GBIF_username"
> gbif_pwd = "your_GBIF_password"
> gbif_email = "your_email"


Run this command for build docker image:

```bash

docker build -t antonbiatov/gbifwiever_datadamp_creator .

```

Run container based on our image


Need to write absolute pathes.

```bash

docker run -it --rm --name gbif_datadump_creator -v "$(pwd)/data:/home/data" -v "$(pwd)/outputs:/home/outputs" -v "$(pwd)/temp:/home/temp" antonbiatov/gbifwiever_datadamp_creator

```

Run it in terminal

```r

source("02_code/myScript.R")

```


