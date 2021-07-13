# Install pacman if it does not already exist
if(!require(pacman)){
  install.packages(pacman)
}

# Use pacman to load/install packaged
pacman::p_load(conflicted, 
               dotenv, 
               targets, 
               tarchetypes, 
               tidyverse, 
               lme4, 
               emmeans, 
               agridat, 
               here, 
               magrittr, 
               broom, 
               broom.mixed, 
               openxlsx)
library(rmarkdown)
