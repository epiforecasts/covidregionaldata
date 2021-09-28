# R version: latest, ... ,4.0.1 , 4.0.0
ARG VARIANT="latest"
FROM rocker/verse:${VARIANT}

# Use the [Option] comment to specify true/false arguments that should appear in VS Code UX
#
# [Option] Install zsh
ARG INSTALL_ZSH="true"
# [Option] Upgrade OS packages to their latest versions
ARG UPGRADE_PACKAGES="false"

# Install needed packages and setup non-root user. Use a separate RUN statement to add your own dependencies.
ARG USERNAME=vscode
ARG USER_UID=1100
ARG USER_GID=$USER_UID
COPY .devcontainer/library-scripts/*.sh /tmp/library-scripts/
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && /bin/bash /tmp/library-scripts/common-debian.sh "${INSTALL_ZSH}" "${USERNAME}" "${USER_UID}" "${USER_GID}" "${UPGRADE_PACKAGES}" "true" "true" \
    && usermod -a -G staff ${USERNAME} \
    && apt-get -y install \
        python3-pip \
        libgit2-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        libxt-dev \
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/* /tmp/library-scripts

RUN python3 -m pip --no-cache-dir install radian pre-commit
RUN install2.r --error --skipinstalled --repos ${CRAN} --ncpus -1 \
        devtools \
        languageserver \
        precommit \
        git2r \
    && rm -rf /tmp/downloaded_packages

# add development dependencies
RUN Rscript -e 'devtools::install_github("mdlincoln/docthis")'

# add linux libraries for dependencies
RUN apt-get update \
     && export DEBIAN_FRONTEND=noninteractive \
     && apt-get -y install --no-install-recommends libgdal-dev \
            libudunits2-dev  libharfbuzz-dev libfribidi-dev \
            libjq-dev libprotobuf-dev protobuf-compiler

# add dependencies for logo making
RUN install2.r --error --skipinstalled --repos ${CRAN} --ncpus -1 \
        hexSticker \
        showtext \
        maps \
        sf \
        rnaturalearth \
        rmapshaper \
    && rm -rf /tmp/downloaded_packages
RUN Rscript -e 'devtools::install_github("ropensci/rnaturalearthdata")'
RUN Rscript -e 'devtools::install_github("ropensci/rnaturalearthhires")'

# install package dependencies
COPY DESCRIPTION /tmp/package/DESCRIPTION
RUN Rscript -e "devtools::install_dev_deps('/tmp/package')"
