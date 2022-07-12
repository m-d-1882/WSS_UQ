# WSS_UQ
Uncertainty Quantification on gjackland's Weight-Scale-Shift (WSS) model for Covid epidemic modelling

## Background on model
The WSS model takes current real-world case data and (among other things) predicts current R number (often days/weeks before other R_no predictions which are based on hosp./deaths), hospitalisations and deaths for a number of days into the future (we've imposed a cut off at 54 days after the last day of cases). In this framework we are only considering the number of deaths.

## Uncertainty Quantification
The WSS model is expensive to run (around 5 minutes per model run) so to get a representation across the entire input space would take an unfeasible amount of time. Therefore an alternative is to create a statistical approximation to the model (in this case a Gaussian process emulator) which is trained by a small number of model runs (usually 10xp where p is no. of input dims.) and can quantify the uncertainty in the areas of input space which haven't been trained.

## Framework for UQ with WSS model
