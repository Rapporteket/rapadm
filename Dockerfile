FROM rapporteket/base-r:main

LABEL maintainer "Arnfinn Hykkerud Steindal <arnfinn.hykkerud.steindal@helse-nord.no>"
LABEL no.rapporteket.cd.enable="true"

ARG GH_PAT
ENV GITHUB_PAT=${GH_PAT}

WORKDIR /app/R

COPY *.tar.gz .

RUN install2.r --error --skipinstalled --ncpus -1 \
    bslib \
    dplyr \
    ggplot2 \
    magrittr \
    readr \
    rlang \
    rpivotTable \
    shiny \
    yaml \
    && rm -rf /tmp/downloaded_packages \
    && R -e "remotes::install_github(\"Rapporteket/rapbase\", ref = \"falktest\")" \
    && R -e "remotes::install_local(list.files(pattern = \"*.tar.gz\"))" \
    && rm ./*.tar.gz \
    && R -e "remotes::install_github(\"Rapporteket/ablanor\", ref = \"poc\")"

EXPOSE 3838

RUN adduser --uid "1000" --disabled-password rapporteket && \
    chown -R 1000:1000 /app/R && \
    chmod -R 755 /app/R
USER rapporteket

CMD ["R", "-e", "options(shiny.port = 3838, shiny.host = \"0.0.0.0\"); rapadm::run_app()"]
