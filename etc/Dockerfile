## start with the Docker "base R" Debian-based image
FROM r-base

## keep the packages current
RUN apt-get update -qq \
	&& apt-get dist-upgrade -y
	
## install requirements for R packages
## best practice to run update each time in case the build restarts later
RUN DEBIAN_FRONTEND=noninteractive apt-get update -qq \
	&& DEBIAN_FRONTEND=noninteractive apt-get install -t unstable -y \
	libgmp-dev \
	curl \
	libcurl4-openssl-dev \
	python-dev \
	libssl-dev \
	default-jdk \
	libxml2-dev \
	libcgal-dev \
	libfreetype6-dev \
	xorg \
	libx11-dev \
	libftgl2 \
	texinfo \
	texlive-base \
	texlive-extra-utils \
	texlive-fonts-extra \
	texlive-fonts-recommended \
	texlive-generic-recommended \
	texlive-latex-base \
	texlive-latex-extra \
	texlive-latex-recommended \
	git

RUN DEBIAN_FRONTEND=noninteractive apt-get update -qq \
	&& apt-get install -t unstable -y \
	r-recommended \
	r-cran-evd \ 
	r-cran-plotly \ 
	r-cran-knitr \ 
	r-cran-rmarkdown \ 
	r-cran-mvtnorm \ 
	r-cran-truncnorm \ 
	r-cran-reshape2 \ 
	r-cran-data.table \ 
	r-cran-pracma \ 
	r-cran-plyr \ 
	r-cran-ggplot2 \ 
	r-cran-gridextra \ 
	r-cran-reshape \ 
	r-cran-magrittr \ 
	r-cran-testthat \ 
	r-cran-pbapply \ 
	r-cran-rgenoud \ 
	r-cran-latticeextra \ 
	r-cran-robustbase \ 
	r-cran-ellipse \ 
	r-cran-leaps \ 
	r-cran-openxlsx \ 
	r-cran-png \ 
	r-cran-colorspace \ 
	r-cran-viridislite \ 
	r-cran-rjson \ 
	r-cran-rcurl \ 
	r-cran-digest \ 
	r-cran-xtable \ 
	r-cran-sp \ 
	r-cran-igraph \ 
	r-cran-dplyr \ 
	r-cran-tidyr \ 
	r-cran-zoo \ 
	r-cran-nleqslv \ 
	r-cran-vgam \ 
	r-cran-psych \ 
	r-cran-desolve \ 
	r-cran-Rcpp \ 
	r-cran-DT \ 
	r-cran-shiny \ 
	r-cran-maxlik \ 
	r-cran-r.utils \ 
	r-cran-biasedurn \ 
	r-cran-fBasics \ 
	r-cran-fExtremes \ 
	r-cran-msm \ 
	r-cran-sn \ 
	r-cran-stabledist \ 
	r-cran-statmod \ 
	r-cran-lazyeval \ 
	r-cran-memoise \ 
	r-cran-tibble \ 
	r-cran-hmisc \ 
	r-cran-lpSolve \ 
	r-cran-readxl \ 
	r-cran-stringr \ 
	r-cran-gtools \ 
	r-cran-rgl

COPY [rev_dep_docker_packages.R", "/home/rocker/rev_dep_docker_packages.R"]

RUN Rscript /home/docker/rev_dep_docker_packages.R triangle

RUN wget https://install-github.me/r-lib/revdepcheck \
	&& Rscript -e 'source("revdepcheck")'
