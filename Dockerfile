FROM landscapedatacommons/r-base:4.2.2
LABEL maintainer='Ken Ramsey <kramsey@jornada-vmail.nmsu.edu>'
# create Rprofile.site file in container
RUN echo "local({options(shiny.port = 3838, shiny.host = '0.0.0.0')})" > /usr/lib/R/etc/Rprofile.site
# change user
USER docker
# make app folder
RUN mkdir /home/docker/terradactyl-tool
# copy app to image
COPY . /home/docker/terradactyl-tool
# set working directory (location of app.R)
RUN R -e "setwd('/opt/docker/landscapedatacommons/tools/benchmark-exploration-tool')"
# select port
EXPOSE 3838
#CMD R -e "shiny::runApp('/terradactyl', host = '0.0.0.0', port = 3838)"
#CMD R -e "shiny::runApp('/home/docker/terradactyl-tool')"
CMD ["tail","-f", "/dev/null"]