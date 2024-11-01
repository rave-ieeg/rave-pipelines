# Use an official R runtime as the base image
FROM rocker/shiny-verse:latest

# EXPOSE 3838
ENV RAVE_PORT=3838
ENV RAVE_LIB_PATH="/usr/local/lib/R/site-library"
ENV RAVE_SERVER_ROOT="/home/shiny/server-files/shiny-server/apps"
EXPOSE 3838

# RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers && usermod -aG sudo shiny

COPY ./common/install_rave.sh /home/shiny/install_rave.sh
RUN bash /home/shiny/install_rave.sh


COPY ./common/setup_rave.sh /home/shiny/setup_rave.sh
RUN chmod 0777 /home/shiny/setup_rave.sh && \
    /bin/su -c "bash /home/shiny/setup_rave.sh" - shiny


COPY ./common/server-files /home/shiny/server-files
RUN cp -f /home/shiny/server-files/shiny-server.conf /etc/shiny-server/shiny-server.conf && \
    chown -R shiny /home/shiny/server-files && \
    chmod -R 0755 /home/shiny/server-files && \
    rm -rf /tmp/*

USER shiny

# rm -rf /root/.local/share/R/
