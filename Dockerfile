FROM rocker/binder:latest

USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}

## Become normal user again
USER ${NB_USER}
RUN wget https://github.com/danielpearson90/vmacfeaturesearch && \
R -e "devtools::install_deps()"
R -e "devtools::install_github("thomasp85/patchwork")""
