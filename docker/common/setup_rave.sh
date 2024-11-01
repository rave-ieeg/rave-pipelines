#!/bin/bash
set -e

export DEBIAN_FRONTEND=noninteractive

R -q -e 'raveio::raveio_setopt("data_dir", "/data/rave_data/data_dir")'
R -q -e 'raveio::raveio_setopt("raw_data_dir", "/data/rave_data/raw_dir")'
R -q -e 'raveio::raveio_setopt("tensor_temp_path", "/data/rave_data/cache_dir")'
R -q -e 'raveio::raveio_setopt("threeBrain_template_dir", "/data/rave_data/others/three_brain")'

R -q -e 'ravemanager::configure_python()'
R -q -e 'rpymat::run_command("conda remove -n r-reticulate --all -y")'
R -q -e 'ravemanager::finalize_installation()'

rm -rf /data/rave_data/raw_dir/DemoSubject
rm -rf /data/rave_data/raw_dir/yael_demo_001
rm -rf /data/rave_data/data_dir/demo

# Clean cache
R -q -e 'rpymat::run_command("pip cache purge")'
R -q -e 'rpymat::run_command("conda clean -a -y")'

