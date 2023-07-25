# MixME (Mixed-fishery Multi-stock Evaluation)
An R package for mixed fisheries Management Strategy Evaluation (MSE) using multi-stock and multi-fleet Operating Models.

- can carry out full MSE to evaluate the performance of Management Procedures.
- can carry out smaller-scale simulations with perfect stock observations to evaluate advice rule behaviour.
- can accommodate a range of fleet and stock parameter uncertainties.
- makes use of FLR libraries () 
- has built-in helper functions to condition Operating Models from fitted SAM () assessment models with uncertainty
- has built-in diagnostic tools to check for errors occuring in the Management Procedure (e.g. assessment model fitting errors) or the Operating Model (e.g. fleet effort optimisation)

## Installation
MixME depends on TMB, FLR libraries and several other packages. Install these first:

```{r}
install.packages(c("FLCore","FLFishery,"FLasher","mse"), repos = "http://flr-project.org/R")
install.packages(c("TMB","doParallel","foreach"))
```

MixME can be installed using the 'devtools' package

```{r}
install.packages("devtools")
devtools::install_github("CefasRepRes/MixME")
```
