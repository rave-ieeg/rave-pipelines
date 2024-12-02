#!/bin/bash
set -e

## build ARGs
NCPUS=${NCPUS:--1}

# shellcheck source=/dev/null
source /etc/os-release

export DEBIAN_FRONTEND=noninteractive

# a function to install apt packages only if they are not installed
function apt_install() {
    if ! dpkg -s "$@" >/dev/null 2>&1; then
        if [ "$(find /var/lib/apt/lists/* | wc -l)" = "0" ]; then
            apt-get update
        fi
        apt-get install -y --no-install-recommends "$@"
    fi
}

apt_install locales patch pkg-config libhdf5-dev libssl-dev libxml2-dev libcurl4-openssl-dev libgit2-dev libfftw3-dev

## Configure default locale
LANG=${LANG:-"en_US.UTF-8"}
/usr/sbin/locale-gen --lang "${LANG}"
/usr/sbin/update-locale --reset LANG="${LANG}"

R_HOME=${R_HOME:-"/usr/local/lib/R"}

mkdir -p /data/rave_data/raw_dir
mkdir -p /data/rave_data/data_dir
mkdir -p /data/rave_data/cache_dir
mkdir -p /data/rave_data/others/three_brain

# Add repos
cat <<EOT >> "${R_HOME}/etc/Rprofile.site"
local({
    r <- getOption("repos")
    r["RAVE"] <- "https://rave-ieeg.r-universe.dev"
    options(repos = r)
})
EOT

install2.r --error --skipinstalled -n "$NCPUS" pak

R -q -e 'pak::pak_install_extra()'
R -q -e 'pak::pak("ravemanager")'
R -q -e 'ravemanager::install(python = FALSE, finalize = FALSE)'

# Make sure RAVE pipelines and their dependences are installed
R -q -e 'raveio::pipeline_install_github(repo = "rave-ieeg/rave-pipelines", to = "default", upgrade = FALSE)'

# R -q -e 'raveio::raveio_setopt("data_dir", "/data/rave_data/data_dir")'
# R -q -e 'raveio::raveio_setopt("raw_data_dir", "/data/rave_data/raw_dir")'
# R -q -e 'raveio::raveio_setopt("tensor_temp_path", "/data/rave_data/cache_dir")'
# R -q -e 'raveio::raveio_setopt("threeBrain_template_dir", "/data/rave_data/others/three_brain")'
# R -q -e 'ravemanager::finalize_installation()'

chmod -R 0777 /data
chmod -R 0777 /usr/local/lib/R/etc

apt-get autoremove -y
apt-get autoclean -y
rm -rf /var/lib/apt/lists/*
rm -rf /tmp/*
rm -rf /root/.local/share/R/*
rm -rf /root/.config/R/*
rm -rf /root/.cache/R/*

# Check the R info
echo -e "Check the R info...\n"

R -q -e "sessionInfo(); library(rave)"

echo -e "\nInstalled RAVE from source, done!"

