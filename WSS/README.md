# WSS_UQ
Uncertainty Quantification on gjackland's Weight-Scale-Shift (WSS) model for Covid epidemic modelling

## Background on model
The WSS model takes current real-world case data and (among other things) predicts current and future R numbers (often days/weeks before other R_no predictions which are based on hosp./deaths) and from there predicts hospitalisations and deaths for a number of days into the future (we've imposed a cut off at 54 days after the last day of case data). In this framework we are only considering the number of deaths.

## Uncertainty Quantification
The WSS model is expensive to run (around 5 minutes per model run) so to get a representation across the entire input space would take an unfeasible amount of time. Therefore an alternative is to create a statistical approximation to the model (in this case a Gaussian process emulator) which is trained by a small number of model runs (usually 10p where p is no. of input dims.) and can quantify the uncertainty in the areas of input space which haven't been trained.

## Framework for UQ with WSS model
### Inputs
This model can be split into two parts: the first being using the case data to predict the R number and the second part being using those R numbers to predict future case, hospitalisations and deaths. As the first part can be represented by an R number then by treating this as a variable we can span all possible case data.

Knowing this we can now bring together all the inputs:
- this model is compartmental with those being: 1) Mild; 2) ILI (influenza-like illness); 3) SARI (severe acute respiratory illness); 4) Critial; 5) Death; 6) Recovery and 7) Critical Recovery. There are 8 model parameters which is the logmean number of days it takes someone to go between the following compartments: 1->7, 2->7, 2->3, 3->7, 3->5, 3->4, 4->7, 4->5 and 7->6 with 4->7 and 4->5 having the same logmean. 
- 5 coefficients that dictate the severity and transmissability of the alpha, delta and omicron variants (except tranmissability of omicron)
- vaccine efficacy
- rate at which R number decays back to 1
- 2 beta parameters that define case-fatality-ratio (CFR) for each age-group.
- R_no from first part

### Outputs
We have imposed a cut off of 54 days after the R number has been set. More specifically we are looking at 01/06/22 - 25/07/22. 

### Principal Component Analysis


### Emulation and Prediction
