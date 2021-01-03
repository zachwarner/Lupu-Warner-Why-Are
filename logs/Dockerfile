#!/usr/bin/env bash

# Set the base environment
FROM ubuntu:18.04
ENV DEBIAN_FRONTEND=noninteractive

# Add the CRAN repository for newer versions of R
RUN apt-get update \
	&& apt-get install -y software-properties-common=0.96.24.32.14 \
	&& apt-get update \
	&& apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
	&& add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran40/' 

# Install R
RUN apt-get update \
	&& apt-get install -y r-base-dev=4.0.2-1.1804.0 \
	&& apt-get install -y wget=1.19.4-1ubuntu2.2

# Get R packages - NB the edarf version is a specific git commit
RUN wget http://cran.r-project.org/src/contrib/Archive/remotes/remotes_2.1.0.tar.gz \
	&& R CMD INSTALL remotes_2.1.0.tar.gz \
	&& Rscript -e 'remotes::install_github("zmjones/edarf/pkg@debfaf36e93cc30bf4dff8f75f14fed9d33c4b42")' \
	&& Rscript -e 'remotes::install_version("caTools", "1.18.0")' \
	&& Rscript -e 'remotes::install_version("RSNNS", "0.4-12")' \
	&& Rscript -e 'remotes::install_version("Rcpp", "1.0.5")' \
	&& Rscript -e 'remotes::install_version("scoring", "0.6")' \
	&& Rscript -e 'remotes::install_version("caret", "6.0-86")' \
	&& Rscript -e 'remotes::install_version("MASS", "7.3-53")' \
	&& Rscript -e 'remotes::install_version("boot", "1.3-25")' \
	&& Rscript -e 'remotes::install_version("randomForest", "4.6-14")' \
	&& Rscript -e 'remotes::install_version("mice", "3.11.0")' \
	&& Rscript -e 'remotes::install_version("lattice", "0.20-41")' \
	&& Rscript -e 'remotes::install_version("reshape", "0.8.8")' \
	&& Rscript -e 'remotes::install_version("ggplot2", "3.3.2")' \
	&& Rscript -e 'remotes::install_version("earth", "5.2.0")' \
	&& Rscript -e 'remotes::install_version("party", "1.3-5")' \
	&& Rscript -e 'remotes::install_version("deepnet", "0.2")' \
	&& Rscript -e 'remotes::install_version("mboost", "2.9-3")' \
	&& Rscript -e 'remotes::install_version("glmnet", "4.0-2")' \
	&& Rscript -e 'remotes::install_version("kernlab", "0.9-29")' \
	&& Rscript -e 'remotes::install_version("e1071", "1.7-3")'

# # Add the CRAN repository for newer versions of R
# RUN apt-get update \
# 	&& apt-get install -y software-properties-common=0.96.24.32.14 \
# 	&& apt-get update \
# 	&& apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9 \
# 	&& add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/' 

# # Install R
# RUN apt-get update \
# 	&& apt-get install -y r-base-dev=3.6.2-1bionic \
# 	&& apt-get install -y wget=1.19.4-1ubuntu2.2

# # Get R packages
# RUN wget http://cran.r-project.org/src/contrib/Archive/remotes/remotes_2.1.0.tar.gz \
# 	&& R CMD INSTALL remotes_2.1.0.tar.gz \
# 	&& Rscript -e 'remotes::install_version("edarf", "1.1.1")' \
# 	&& Rscript -e 'remotes::install_version("caTools", "1.18.0")' \
# 	&& Rscript -e 'remotes::install_version("RSNNS", "0.4-12")' \
# 	&& Rscript -e 'remotes::install_version("Rcpp", "1.0.3")' \
# 	&& Rscript -e 'remotes::install_version("scoring", "0.6")' \
# 	&& Rscript -e 'remotes::install_version("caret", "6.0-85")' \
# 	&& Rscript -e 'remotes::install_version("MASS", "7.3-51.5")' \
# 	&& Rscript -e 'remotes::install_version("boot", "1.3-24")' \
# 	&& Rscript -e 'remotes::install_version("randomForest", "4.6-14")' \
# 	&& Rscript -e 'remotes::install_version("mice", "3.7.0")' \
# 	&& Rscript -e 'remotes::install_version("lattice", "0.20-38")' \
# 	&& Rscript -e 'remotes::install_version("reshape", "0.8.8")' \
# 	&& Rscript -e 'remotes::install_version("ggplot2", "3.2.1")' \
# 	&& Rscript -e 'remotes::install_version("earth", "5.1.2")' \
# 	&& Rscript -e 'remotes::install_version("party", "1.3-3")' \
# 	&& Rscript -e 'remotes::install_version("deepnet", "0.2")' \
# 	&& Rscript -e 'remotes::install_version("mboost", "2.9-1")' \
# 	&& Rscript -e 'remotes::install_version("glmnet", "3.0-2")' \
# 	&& Rscript -e 'remotes::install_version("kernlab", "0.9-29")' \
# 	&& Rscript -e 'remotes::install_version("e1071", "1.7-3")'