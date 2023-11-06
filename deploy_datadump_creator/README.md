






## Note

The script [**1_data_preparation.R**](scripts/1_data_preparation.R) for manual running. Don't use it on server.

> scripts/1_data_preparation.R 


## Build Docker image:


Before build product Docker image, you **need to change file** [`gbif_ini.R`](scripts/gbif_ini.R). You need to add your GBIF credentials into this file. 

> gbif_user = "your_GBIF_username"
>
> gbif_pwd = "your_GBIF_password"
>
> gbif_email = "your_email"

The next, run this command for build docker image:

```bash

docker build -t antonbiatov/gbifwiever_datadamp_creator .

```

Run container based on our image.


For test you can run it with mounted local folders with temporary data.

```bash

docker run -it --rm --name gbif_datadump_creator -v "$(pwd)/data:/app/data" -v "$(pwd)/outputs:/app/outputs" -v "$(pwd)/temp:/app/temp" antonbiatov/gbifwiever_datadamp_creator

```



## Use Crontab for run conteiner by schedule

test run

```bash

docker run --rm --name gbif_datadump_creator -v "gbif_data:/app/outputs" antonbiatov/gbifwiever_datadamp_creator

```

Edit Crontab 

```bash

crontab -e

```

Add line to Crontab

```bash

0 2 * * 7 docker run -it --rm --name gbif_datadump_creator -v "gbif_data:/app/outputs" antonbiatov/gbifwiever_datadamp_creator

```

