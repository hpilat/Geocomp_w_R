# create a folder for data and scripts
dir.create("data/")
dir.create("scripts/")

# install required packages 
remotes::install_github("geocompr/geocompkg", dependencies = TRUE)

# load required packages
library(sf)
library(terra)
library(spData)
install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")

library(spDataLarge)


