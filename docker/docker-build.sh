#!/bin/bash

# Get the script directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

# set docker build context to the script directory
cd "$SCRIPT_DIR"


docker build --progress=plain -t dipterix/rave-shiny-server -f "./rave-shiny-server.Dockerfile" "."

# docker run --rm -ti --name test -e APPLICATION_LOGS_TO_STDOUT=true -p 3838:3838 dipterix/rave-shiny-server
# docker exec -ti test /bin/bash
# docker run -it -p 17283:17283 --name test rave-ieeg/shiny-verse bash

# docker run -ti -d -v /Volumes/RAVE_SSD/packages:/data/packages -e APPLICATION_LOGS_TO_STDOUT=true rave-ieeg/shiny-verse bash
# docker run -ti --name rd --security-opt seccomp=unconfined -v /Volumes/RAVE_SSD/packages:/mydir wch1/r-debug
# docker exec -ti rd /bin/bash
# docker run -ti --name rchk -v /Volumes/RAVE_SSD/packages:/mydir kalibera/rchk:latest bash
# docker exec -ti rchk /bin/bash
