# Fast-and-Frugal Trees ####
#
# Hyperparameters of interest are:
# 1. Forest Method {ifan, dfan, max, zigzag}.
#   The algorithm used to build the trees. "ifan" stands for
# independent fan and makes the split decision that maximises
# the selection goal for the variable (a.k.a. "cue") at 
# hand, independent of all other variables. "dfan" stands for
# dependent fan and makes the split decision that maximises the
# selection goal for the variable (a.k.a. "cue"), taking
# subsequent split decisions into account. "max" prioritises
# splitting on variables based on their maximum positive and
# negative predictive values, with the larger of the two 
# deciding the split direction. "zigzag" enforces alternating
# split directions before prioritising which variables to
# split on, and the split direction being choosen based on the
# larger of the two.
#

# Set up
#rm(list = ls())
source("dataGen.r")
df<-dataGen()#10, 4, 5, 5)
list_of_packages <- c("FFTrees")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
for (i in 1:length(list_of_packages))
{
  require(list_of_packages[i],character.only = T)
}

# Specify lists of hyperparameter values.
hyperparams_fft <- list(
                       hp_forestMethod = c("ifan", "dfan", "max", "zigzag")
                       )

# Build models
iForestMethod <- 1

mod_FFT <- FFTrees(formula = progRecur ~., 
                   data = df,
                   algorithm = hyperparams_fft$hp_forestMethod[iForestMethod],
                   goal = "bacc",
                   goal.chase = "bacc",
                   decision.labels = c("No", "Prog/Recurr"),
                   do.comp = FALSE)

mod_FFT_pred <- predict(
                        object = mod_FFT,
                        newdata = df,
                        type = "prob",
                        method = "laplace"
                        )


plot(myFFT)


# Next####