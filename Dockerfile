FROM landscapedatacommons/r-base:4.2.2
LABEL maintainer='Ken Ramsey <kramsey@jornada-vmail.nmsu.edu>'
# create Rprofile.site file in container
RUN echo "local({options(shiny.port = 3838, shiny.host = '0.0.0.0')})" > /usr/lib/R/etc/Rprofile.site
# give docker user permissions to /tmp
RUN chown -R root:docker /tmp
# change user
USER docker
# make app folder
RUN mkdir /home/docker/rangeland-indicator-calculator
# copy app to image
COPY . /home/docker/rangeland-indicator-calculator
# set working directory
RUN R -e "setwd('/home/docker/rangeland-indicator-calculator')"
# select port
EXPOSE 3838
CMD R -e "shiny::runApp('/home/docker/rangeland-indicator-calculator')"