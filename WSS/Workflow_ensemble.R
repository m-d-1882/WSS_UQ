setwd('C:/Users/md624/OneDrive - University of Exeter/Microsoft/WSS/WSS-main/')

# Remove existing variables if in an interactive session.
if(interactive()){
  rm(list = ls())
}
source("./getParms.R")
#source("./covid_trimmed.r")
source('./covid_trimmed_oneregion.r')
#source("./ScottishData.R")
#source("./Regional.R")
source('./Regional_oneregion.R')