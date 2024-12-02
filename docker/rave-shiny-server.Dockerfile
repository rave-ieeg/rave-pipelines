# Use an official R runtime as the base image
FROM rocker/shiny-verse:latest

USER root

# EXPOSE 3838
ENV RAVE_PORT=3838
ENV RAVE_LIB_PATH="/usr/local/lib/R/site-library"
ENV RAVE_SERVER_ROOT="/apps/shiny/server-files/shiny-server/apps"
EXPOSE 3838

# RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers && usermod -aG sudo shiny

COPY ./common/install_rave.sh /apps/shiny/install_rave.sh
RUN bash /apps/shiny/install_rave.sh


COPY ./common/setup_rave.sh /apps/shiny/setup_rave.sh
RUN chmod 0777 /apps/shiny/setup_rave.sh

# && \
#     /bin/su -c "bash /apps/shiny/setup_rave.sh" - shiny


COPY ./common/server-files /apps/shiny/server-files
RUN cp -f /apps/shiny/server-files/shiny-server.conf /etc/shiny-server/shiny-server.conf && \
    chmod -R 0777 /apps/shiny/server-files && \
    rm -rf /tmp/*

# chown -R shiny /apps/shiny/server-files && \

# USER shiny

# rm -rf /root/.local/share/R/
