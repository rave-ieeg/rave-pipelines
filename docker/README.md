---
author: Zhengjia Wang
license: CC-BY
---

## Docker container for RAVE

`rave-shiny-server.Dockerfile` builds a docker image readily to be deployed 
for in-lab use or to the cloud. 

To compile the docker image, please download docker app and make the command
`docker` available. Then download this repository, open your terminal and run:

```sh
chmod a+x ./docker/docker-build.sh
./docker/docker-build.sh
```

It will take 10~20 minutes to compile the image. You can then test with

```sh
docker run --rm -ti --name test -e APPLICATION_LOGS_TO_STDOUT=true -p 3838:3838 dipterix/rave-shiny-server
```

A RAVE server will be deployed to the localhost and you can access the service
by opening a browser and go to 

  http://127.0.0.1:3838

### Use RAVE in docker

RAVE data is stored at `/data/rave_data` inside of the containers. You can mount
your own data via `-v` parameter.

By default, RAVE stores data under the home directory, you can use 

```sh
docker run -e APPLICATION_LOGS_TO_STDOUT=true -p 3838:3838 -v "$HOME/rave_data":/data/rave_data dipterix/rave-shiny-server
```

to mount the local RAVE directory located at `$HOME/rave_data` on the container
at `/data/rave_data`. If your RAVE data folder is in other places, please
change `$HOME/rave_data` accordingly. 

Here are critical mounting points:

* `/data/rave_data/raw_dir` raw data directory
* `/data/rave_data/data_dir` processed data directory
* `/data/rave_data/cache_dir` and `/tmp` temporary files (they grow quickly and we recommend SSD for fast access)
* `/data/rave_data/others` templates, external data files

