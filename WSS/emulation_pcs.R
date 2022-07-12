# this script takes the pca determined in 'PCA.R' and fits emulators to each 
# princ. component. this gives the ability to specify an input and then obtain a 
# probability distribution of timeseries as an output. this can be compared with
# a validation timeseries.

setwd('C:/Users/md624/OneDrive - University of Exeter/Microsoft/WSS/emulate_timeseries')

library(DiceKriging)
library(MASS)
library(tidyverse)

prediction.curves = function(input,emulators,no.emulator.draws,pca,ignore.weights=0,
                             error){
  # this function takes an input vector; uses the emulators to predict the pcs 
  # relating to the pca conducted previously and ultimately constructs a 
  # timeseries prediction. as the emulators produce a prob. dist. of each pc 
  # this means we can construct confidence intervals. one can also ignore 
  # certain pcs as the first few explain most variance which can be found in the
  # outliers, causing overfitting on the data and poorer emulation
  
  # uses emulators to make mean prediction of beta parameters which define 
  # timeseries
  
  # load in basis functions
  basis.functions = pca$basis.functions
  
  # first obtain mean principal components by using the mean of each emulator
  no.outputs = dim(basis.functions)[2]
  mean_pca_pred = upper_pca_pred = lower_pca_pred = vector('numeric',no.outputs)
  for(i in 1:no.outputs){
    pca_pred = predict.km(emulators[[i]],newdata = t(as.matrix(input)),
                      type = 'UK',checkNames = FALSE)
    
    mean_pca_pred[i] = pca_pred$mean
    
    upper_pca_pred[i] = mean_pca_pred[i] + 2*pca_pred$sd
    lower_pca_pred[i] = mean_pca_pred[i] - 2*pca_pred$sd
  }
  
  # make multiple draws of gp to obtain the principal components
  prediction_pca_weights = matrix(0,no.emulator.draws,no.outputs)
  for(j in 1:no.outputs){
    set.seed(1)
    single_draw_data = predict.km(emulators[[j]],newdata = t(as.matrix(input)),
                                  type = 'UK',cov.compute = TRUE,checkNames = FALSE)
    
    prediction_pca_weights[,j] = mvrnorm(no.emulator.draws,mu = single_draw_data$mean,
                                    Sigma = single_draw_data$cov)
  }
  
  # ignore pcs
  mean_pca_pred[ignore.weights] = 0 # set weights to 0 
  upper_pca_pred[ignore.weights] = 0
  lower_pca_pred[ignore.weights] = 0
  
  prediction_pca_weights[,ignore.weights] = 0
  
  # define timeseries points
  t = 0:54
  
  # use principal components to plot mean timeseries
  timeseries_mean = mean_pca_pred[1]*basis.functions[,1]
  timeseries_upper = upper_pca_pred[1]*basis.functions[,1]
  timeseries_lower = lower_pca_pred[1]*basis.functions[,1]
  for(k in 2:no.outputs){
    timeseries_mean = timeseries_mean + mean_pca_pred[k]*basis.functions[,k]
    timeseries_upper = timeseries_upper + upper_pca_pred[k]*basis.functions[,k] # +2sd
    timeseries_lower = timeseries_lower + lower_pca_pred[k]*basis.functions[,k] # -2sd
  }
  
  plot = ggplot()
  
  # the timeseries of all the gp draws. add them to the plot of the mean timeseries
  timeseries = list()
  for(i in 1:no.emulator.draws){
    error_ = rnorm(1,0,sqrt(error)) # add est. fitted model error
    
    timeseries_ = prediction_pca_weights[i,1]*basis.functions[,1]
    for(k in 2:no.outputs){
      timeseries_ = timeseries_ + prediction_pca_weights[i,k]*basis.functions[,k]
    }
    timeseries_ = timeseries_ + error_
    
    timeseries = append(timeseries,list(timeseries_))
    rm(timeseries_)
    
    plot = plot + geom_line(aes_string(x=t,y=timeseries[[i]]),col = 'black',
                            size = 0.75,show.legend = TRUE)
  }
  
  # collate timeseries
  timeseries = append(timeseries,list(timeseries_mean,timeseries_upper,
                                      timeseries_lower))
  plot = plot + geom_ribbon(aes(x = t, y = timeseries_mean, 
                                ymin = timeseries_lower, ymax = timeseries_upper),fill = 'green') + 
    geom_line(aes(x=t,y=timeseries_mean, col = 'timeseries_mean'),size=1.5)
  
  return(list(timeseries = timeseries, mean_pca_coeffs = mean_pca_pred, 
              pca_pred_weights = prediction_pca_weights, basis_functions = basis.functions,
              plot = plot))
}


#source('PCA.R')

# combine inputs and outputs into one matrix
training_data = cbind(all.inputs[-ignore.design.points,],pca_WSS1$weights)
validation_data = validation_data[-ignore.valid.points,]

fit.error = lapply(1:(dim(all.outputs)[1]-length(ignore.design.points)),
                   function(i) test_pca(pca_WSS1,i)$error)
fit.error = Reduce('rbind',fit.error)
error = var(fit.error)

# build emulators for each of the principal components of the pca. the inputs 
# are the design points
emulators = list()
for (i in 1:no.pcs){
  emulator = km(design = training_data[,1:18],response = training_data[,18+i],
                covtype = 'matern5_2',multistart = 5,
                control = list(trace=FALSE, pop.size=200, max.generations=50))
  
  emulators = append(emulators,list(emulator)) # add emulator i to list
  rm('emulator')
}


{
# select validation point to test accuracy and number of emulator draws
valid.no = 32
no.emulator.draws = 100
ignore.weights = c(0) # if you don't want to ignore any; set to 0

# use above function to obtain plot of predicted timeseries mean and random 
# draws given an input
draw = prediction.curves(input = validation_data[valid.no,1:18],
                              emulators = emulators,
                              no.emulator.draws = no.emulator.draws,
                              pca_WSS1,ignore.weights = ignore.weights,error = error)

# plot the predictions and the validation plot
draw$plot + geom_line(aes(x=0:54,y=validation_data[valid.no,19:73],
                               col = 'validation'),size=1.5)# + ylim(c(0,20))
}

