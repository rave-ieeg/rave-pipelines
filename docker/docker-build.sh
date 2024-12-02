#!/bin/bash

# Get the script directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

# set docker build context to the script directory
cd "$SCRIPT_DIR"


docker build --platform linux/amd64 --progress=plain -t dipterix/rave-server -f "./rave-shiny-server.Dockerfile" "."

# docker run --rm -ti --name test -e APPLICATION_LOGS_TO_STDOUT=true -p 3838:3838 dipterix/rave-shiny-server
