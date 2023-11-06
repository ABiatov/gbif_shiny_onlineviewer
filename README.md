# gbif_shiny_onlineviewer

===

**GBIF Viewer**: an open web-based biodiversity conservation decision-making tool for policy and governance. Спільний проєкт The [Habitat Foundation](https://thehabitatfoundation.org/) та [Української Природоохоронної Групи](https://uncg.org.ua/), за підтримки [NLBIF: The Netherlands Biodiversity Information Facility](https://www.nlbif.nl/), nlbif2022.014


The GBIF Viewer consists of 2 components: a data dump creator and a web-viewer. There must be some kind of file storage through which data must be transferred between these components.
The storage role for the data dump can be a Docker volume or a folder on the host server or cloud storage. This storage must be mounted to the container.


## Folders in this repository

** [gbif_app_v1](gbif_app_v1) ** - The first version application. Get GBIF data by list of species. Geting data by occ_search()

** [gbif_app_v2](gbif_app_v2) ** - The second version application. Get GBIF data from data damp.

** [name_lookup](name_lookup) ** - The code for data damp creation.


** [container_webapp](container_webapp) ** - The docker conteiner for build web application wich work with data damp.

** [container_datadump](container_datadump) ** - The Docker container for generating data dump. 



# Installing 

## Plan

1. Create a docker volume to exchange data between components
2. Get GBIF Viewer source code
3. Deploy datadump creator & run it 
4. Deploy web application 


##  Create a docker volume to exchange data between components

You need to create any storage 

```bash

docker volume create --name gbif_data

```


## Get GBIF Viewer source code

```bash

git clone https://github.com/ABiatov/gbif_shiny_onlineviewer.git

cd gbif_shiny_onlineviewer

```


## Deploy datadump creator

Go to folder [`deploy_datadump_creator`](deploy_datadump_creator) and read manual ['deploy_datadump_creator/READMY.md'](deploy_datadump_creator/READMY.md).

```bash

cd deploy_datadump_creator

```


## Deploy web application

Go to folder [`deploy_webapp`](deploy_webapp) and read manual ['deploy_webapp/READMY.md'](deploy_webapp/READMY.md).

```bash

cd deploy_webapp

```



