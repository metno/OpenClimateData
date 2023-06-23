# syntax=docker/dockerfile:1
ARG UBUNTU_LTS=jammy

# First a tempoary container to convert from asc to gpg
FROM ubuntu:${UBUNTU_LTS} AS apt-key
ARG DEBIAN_FRONTEND=noninteractive
RUN mkdir -p /work
WORKDIR /work
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    rm -f /etc/apt/apt.conf.d/docker-clean && \
    echo 'Binary::apt::APT::Keep-Downloaded-Packages "true";' > /etc/apt/apt.conf.d/keep-cache && \
    apt-get update -qq && \
    apt-get --no-install-recommends install -yq gnupg
COPY *.asc .
RUN cat cran_ubuntu_key.asc | gpg --dearmour > cran_ubuntu_key.gpg
RUN cat cran2deb4ubuntu.asc | gpg --dearmour > cran2deb4ubuntu.gpg

# Common base for compile and runtime environment (limit number of container layers, only include necessary files)
FROM ubuntu:${UBUNTU_LTS} AS base
ARG DEBIAN_FRONTEND=noninteractive
ARG SHINY_URL=https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.20.1002-amd64.deb
ARG SHINY_SHA256=36667a9fbe59a103dcc147766cbf06027034887c3efb82a3c13bba32e255633b
ARG UBUNTU_LTS=jammy
COPY --from=apt-key /work/*.gpg /etc/apt
COPY dependencies.ubuntu /
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    rm -f /etc/apt/apt.conf.d/docker-clean && \
    echo 'Binary::apt::APT::Keep-Downloaded-Packages "true";' > /etc/apt/apt.conf.d/keep-cache && \
    apt-get update -qq && \
    apt-get dist-upgrade -yq && \
    apt-get --no-install-recommends install -yq ca-certificates && \
    echo "deb [arch=amd64 signed-by=/etc/apt/cran_ubuntu_key.gpg] https://cloud.r-project.org/bin/linux/ubuntu ${UBUNTU_LTS}-cran40/" | tee /etc/apt/sources.list.d/cran.list && \
    echo "deb [arch=amd64 signed-by=/etc/apt/cran2deb4ubuntu.gpg] https://ppa.launchpadcontent.net/c2d4u.team/c2d4u4.0+/ubuntu ${UBUNTU_LTS} main" | tee /etc/apt/sources.list.d/cran2deb4ubuntu.list && \
    eval $(/dependencies.ubuntu) && \
    apt-get update -qq && \
    apt-get --no-install-recommends install -yq \
        curl \
        $RUN_DEPS \
    && echo ok
#    && \
#    curl -o /tmp/shiny-server.deb -fsSL "${SHINY_URL}" && \
#    echo "${SHINY_SHA256}  /tmp/shiny-server.deb" | sha256sum -c && \
#    apt-get install --no-install-recommends -yq /tmp/shiny-server.deb && \
#    apt-get install --no-install-recommends -yqf && \
#    rm -f /tmp/shiny-server.deb && \
#    apt-get autoremove -yq

# Temporary container for compilation
# Use many statements for easier caching during build - this doesn't matter in the end as the build time environment is discarded after compilation
FROM base AS build
ARG DEBIAN_FRONTEND=noninteractive
RUN --mount=type=cache,target=/var/cache/apt,sharing=locked \
    --mount=type=cache,target=/var/lib/apt,sharing=locked \
    eval $(/dependencies.ubuntu) && \
    apt-get update -qq && \
    apt-get install -yq --no-install-recommends $COMPILE_DEPS
COPY dependencies.R .
RUN R --slave --no-restore -e 'source("dependencies.R")'
RUN strip /usr/local/lib/R/site-library/*/libs/*.so
RUN mkdir -p /dist/usr/local/lib/R /dist/etc/shiny-server /dist/srv/shiny-server /dist/usr/local/bin
RUN cp -av /usr/local/lib/R/site-library /dist/usr/local/lib/R/
COPY shiny-server.conf /dist/etc/shiny-server/shiny-server.conf
COPY app/* /dist/srv/shiny-server
COPY scripts/* /dist/usr/local/bin

# Add supercronic, which is used to schedule the data updates with cron syntax
# see ../podman.yaml for example of sidecar doing the periodic updates
FROM base as supercronic
# Latest releases available at https://github.com/aptible/supercronic/releases
ENV SUPERCRONIC_URL=https://github.com/aptible/supercronic/releases/download/v0.2.25/supercronic-linux-amd64 \
    SUPERCRONIC=supercronic-linux-amd64 \
    SUPERCRONIC_SHA1SUM=642f4f5a2b67f3400b5ea71ff24f18c0a7d77d49

RUN curl -fsSLO "$SUPERCRONIC_URL" \
 && echo "${SUPERCRONIC_SHA1SUM}  ${SUPERCRONIC}" | sha1sum -c - \
 && chmod +x "$SUPERCRONIC" \
 && mv "$SUPERCRONIC" "/usr/local/bin/${SUPERCRONIC}" \
 && ln -s "/usr/local/bin/${SUPERCRONIC}" /usr/local/bin/supercronic

# Build application runtime container (limit number of container layers, only include necessary files)
FROM supercronic AS runtime
LABEL org.opencontainers.image.licenses="GPL-2.0-or-later" \
      org.opencontainers.image.source="https://github.com/metno/OpenClimateData" \
      org.opencontainers.image.vendor="MET Norway" \
      org.opencontainers.image.authors="rasmus.benestad@met.no"
ENV HOME=/home/unpriv \
    SHINY_LOG_STDERR=1
COPY --from=build /dist/ /
RUN useradd -m unpriv -s /bin/bash && \
    for WRITE_DIR in \
      /var/log/shiny-server \
      /var/lib/shiny-server \
    ; do mkdir -p "${WRITE_DIR}" && chmod -R a+rwX "${WRITE_DIR}"; done && \
    for READ_DIR in \
      /srv/shiny-server \
      /home/unpriv \
    ; do mkdir -p "${READ_DIR}" && chmod -R a-w,a+rX "${READ_DIR}"; done && \
    echo Filesystem privileges configured for running as any unprivileged user
USER unpriv:unpriv
WORKDIR /home/unpriv
CMD R --slave --no-restore -e 'shiny::runApp("/srv/shiny-server", host="0.0.0.0", port=3838)'
EXPOSE 3838
