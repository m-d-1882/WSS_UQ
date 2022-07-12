# this script generates design points which are then evaluated by the WSS model
# and stored in input and output csv files. I've made this script separate to 
# the rest (and created csv files for inputs and outputs) as I prefer running 
# the model on a server.

library('lhs')

# set wd to where WSS model is stored
setwd('C:/Users/md624/OneDrive - University of Exeter/Microsoft/WSS/WSS-main')

generate_samples = function(var.names = var.names,upper.range = upper.range,
                            lower.range = lower.range,
                            no.design.points = 10*length(upper.range),
                            no.valid.points = NULL){
  # as function name suggests; this function takes upper, lower and names of 
  # variables to create latin hypercube sample with no.design.points points.
  # It also manipulates the R_0 sample range to skew more samples towards 0.
  # Also if one wishes to do validation runs, they can also be specified
  
  # build design points on [0,1]^d range
  samples = maximinLHS(no.design.points,length(upper.range))
  
  # scale them as per the ranges and label them
  samples = t(t(samples)*(upper.range-lower.range) + lower.range)
  colnames(samples) = var.names
  
  # manipulate R_0 to scale towards 0 as lower R_0 is more realistic
  samples[,'R_no'] = 6*pbeta(q = samples[,'R_no']/6, 1.25, 0.9)
  
  # do the same with the validation points (if they exist)
  if(!is.null(no.valid.points)){
    # build design points on [0,1]^d range
    samples_valid = maximinLHS(no.valid.points,length(upper.range))
    
    # scale them as per the ranges and label them
    samples_valid = t(t(samples_valid)*(upper.range-lower.range) + lower.range)
    colnames(samples_valid) = var.names
    
    # manipulate R_0 to scale towards 0 as lower R_0 is more realistic
    samples_valid[,'R_no'] = 6*pbeta(q = samples_valid[,'R_no']/6, 1.25, 0.9)
    
    return(list(samples = samples, samples_valid = samples_valid))
  }
  
  return(list(samples = samples))
}
# variable names
var.names = c('logmeanMild2R','logmeanILI2R','logmeanILI2SARI','logmeanSARI2R',
              'logmeanSARI2D','logmeanSARI2Crit','logmeanCrit2CritRecov',
              'logmeanCritRecov2Recov','Kentfac','Indiafac','Omicronfac',
              'Kenttrans','Indiatrans','vacCFR','R_decay','R_no','RawCFR_beta1',
              'RawCFR_beta2')

# specify ranges and number of design and validation points
upper.range = c(rep(15,8),rep(1,5),1,1,6,5,5)
lower.range = c(rep(2,8),rep(0,5),0,0,0,0,0)
no.design.points = 10*length(upper.range)
no.valid.points = 50

# to ensure reproducibility with results; one can set seed
set.seed(1)

# generate the samples
samples = generate_samples(upper.range = upper.range, lower.range = lower.range,
                           var.names = var.names, 
                           no.valid.points = no.valid.points)

samples.design = samples$samples
samples.valid = samples$samples_valid

# run model for each dp
for(iter in 1:nrow(samples.design)){
  #   read in parameters into getParms.R file
  inputs = list(logmeanMild2R = samples.design[iter,'logmeanMild2R'],
                logmeanILI2R = samples.design[iter,'logmeanILI2R'],
                logmeanILI2SARI = samples.design[iter,'logmeanILI2SARI'],
                logmeanSARI2R = samples.design[iter,'logmeanSARI2R'],
                logmeanSARI2D = samples.design[iter,'logmeanSARI2D'],
                logmeanSARI2Crit = samples.design[iter,'logmeanSARI2Crit'],
                logmeanCrit2CritRecov = samples.design[iter,'logmeanCrit2CritRecov'],
                logmeanCritRecov2Recov = samples.design[iter,'logmeanCritRecov2Recov'],
                Kentfac = samples.design[iter,'Kentfac'],
                Indiafac = samples.design[iter,'Indiafac'],
                Omicronfac = samples.design[iter,'Omicronfac'],
                Kenttrans = samples.design[iter,'Kenttrans'],
                Indiatrans = samples.design[iter,'Indiatrans'],
                vacCFR = samples.design[iter,'vacCFR'],
                R_decay = samples.design[iter,'R_decay'],
                R_no = samples.design[iter,'R_no'],
                RawCFR_beta1 = samples.design[iter,'RawCFR_beta1'],
                RawCFR_beta2 = samples.design[iter,'RawCFR_beta2'])
  
  # run model
  source("./getParms_ensemble.R")
  source('./covid_trimmed_oneregion.r')
  source('./Regional_oneregion.R')
  
  # if output folder doesn't exist; create one
  if(dir.exists('output_files')==FALSE){
    dir.create('output_files')
  }
  
  # read inputs and outputs into csv files
  write_csv(x = as.data.frame(t(samples.design[iter,])),
            file = paste0('output_files/run',iter,'input.csv'))
  write_csv(x = CCLon,
            file = paste0('output_files/run',iter,'output.csv'))
}

# run model for each vp
for(iter in 1:nrow(samples.valid)){
  #   read in parameters into getParms.R file
  inputs = list(logmeanMild2R = samples.valid[iter,'logmeanMild2R'],
                logmeanILI2R = samples.valid[iter,'logmeanILI2R'],
                logmeanILI2SARI = samples.valid[iter,'logmeanILI2SARI'],
                logmeanSARI2R = samples.valid[iter,'logmeanSARI2R'],
                logmeanSARI2D = samples.valid[iter,'logmeanSARI2D'],
                logmeanSARI2Crit = samples.valid[iter,'logmeanSARI2Crit'],
                logmeanCrit2CritRecov = samples.valid[iter,'logmeanCrit2CritRecov'],
                logmeanCritRecov2Recov = samples.valid[iter,'logmeanCritRecov2Recov'],
                Kentfac = samples.valid[iter,'Kentfac'],
                Indiafac = samples.valid[iter,'Indiafac'],
                Omicronfac = samples.valid[iter,'Omicronfac'],
                Kenttrans = samples.valid[iter,'Kenttrans'],
                Indiatrans = samples.valid[iter,'Indiatrans'],
                vacCFR = samples.valid[iter,'vacCFR'],
                R_decay = samples.valid[iter,'R_decay'],
                R_no = samples.valid[iter,'R_no'],
                RawCFR_beta1 = samples.valid[iter,'RawCFR_beta1'],
                RawCFR_beta2 = samples.valid[iter,'RawCFR_beta2'])
  
  # run model
  source("./getParms_ensemble.R")
  source('./covid_trimmed_oneregion.r')
  source('./Regional_oneregion.R')
  
  # if output folder doesn't exist; create one
  if(dir.exists('output_files_valid')==FALSE){
    dir.create('output_files_valid')
  }
  
  # read inputs and outputs into csv files
  write_csv(x = as.data.frame(t(samples.valid[iter,])),
            file = paste0('output_files_valid/run',iter,'input.csv'))
  write_csv(x = CCLon,
            file = paste0('output_files_valid/run',iter,'output.csv'))
}