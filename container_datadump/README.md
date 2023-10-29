

Build Docker image:

```bash

docker build -t antonbiatov/datadamp_gbifwiever .

```

Run container based on our image

Need to write absolute pathes.

```bash

docker run -it --rm -v C:/Users/admin/Documents/GitHub/gbif_shiny_onlineviewer/container_datadump/01_data:/usr/src/app/01_data -v C:/Users/admin/Documents/GitHub/gbif_shiny_onlineviewer/container_datadump/03_output:/usr/src/app/03_output antonbiatov/datadamp_gbifwiever

```

Run it in terminal

```r

source("02_code/myScript.R")

```


