#!/bin/bash
# af-cong-mechanisms-replication-master.sh
# Creates a docker container and runs all code to reproduce all results
# Zach Warner
# 2 January 2021

# Set the working directory
cd /path/to/this/replication

# Pull the image
docker pull zachwarner/aff-cong-mechanisms:latest

# Deploy the container
docker run --rm -it -v $(pwd)/data:/data -v $(pwd)/results:/results -v $(pwd)/scripts:/scripts -v $(pwd)/logs:/logs zachwarner/aff-cong-mechanisms:latest

# Run the script
R CMD Rscript /scripts/Lupu-Warner-Why-Are.R

# Fix permissions
chmod -R a+rwX /results
chmod -R a+rwX /logs

# Exit
exit