# This script generates data.
#
# The dataset will have nCont-many continuous variables and 
# nCat-many categorical variables, for nPat-many patients with
# an average of nEvent-many events.
#
# nPat = Count of patients to be simulated.
# nEvent = Average count of events per patient.
# nCont = Count of continuous variables.
# nCat = Count of categorical variables.
# simMissing = TRUE/FALSE to indicate whether missinness should
#              be simulated.
# prctMissing = The percentage of missingness that will be
#               simulated, if simMissing is TRUE.

dataGen <- function(nPat, nEvent, nCont, nCat, simMissing, prctMissing)
{
# Set up.
list_of_packages <- c("tidyverse")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
for (i in 1:length(list_of_packages))
{
  require(list_of_packages[i],character.only = T)
}
set.seed(1)
if(missing(nPat)) nPat = 100;
if(missing(nEvent)) nEvent = 10;
if(missing(nCont)) nCont = 20;
if(missing(nCat)) nCat = 50;
if(missing(simMissing)) {simMissing = FALSE; warning("Missingness will not be simulated")};
if(missing(prctMissing)) prctMissing = 0.2;
  
# Begin the dataset.
df <- data.frame('PatientID' = rep(1:nPat, rpois(nPat, nEvent)))

# Define a reference dataframe that specifies the count of events
# that each patient will have.
nPatientsAndEvents <- data.frame(table(df))
names(nPatientsAndEvents) <- c('PatientID', 'nEvents')

# Get a sorted, random sample of dates for each PatientID.
ts <- integer()
for (i in 1:nPat)
{
 ts <- c(ts, 
             sort(
                  sample(
                         seq(as.Date('1999/01/01'), as.Date('2012/01/01'), by="day"),
                         nPatientsAndEvents$nEvents[i]
                         )
                  )
             ) 
}
df$ts <- ts

# Generate data for all required continuous variables.
contVars <-  data.frame(replicate(nCont, expr = rnorm(nrow(df), 10, 4)))
names(contVars) <- c(paste("contVar", 1:nCont, sep = "."))

# Generate data for all required categorical variables.
catVars <- data.frame(replicate(nCat, expr = factor(rpois(nrow(df), sample(1:4, 1)))))
names(catVars) <- c(paste("catVar", 1:nCat, sep = "."))

# Bind variables to main dataframe.
df <- cbind(df, contVars, catVars)

# Simulate missingness.
if (simMissing == TRUE)
{
  df <- cbind(rn = 1:nrow(df), df)
  c_names <- colnames(df)[4:ncol(df)]
  df <- df %>%
    gather(var, value, -rn) %>%    # reshape data
    mutate(r = runif(nrow(.)),     # simulate a random number from 0 to 1 for each row
           value = ifelse(var %in% c_names & r <= prctMissing, NA, value)) %>%  # if it's one of the variables you specified and the random number is less than your threshold update to NA
    select(-r) %>%                 # remove random number
    spread(var, value)        # reshape back to original format
  df <- select(df, -rn)
}

# Simulate progression/recurrence.
df$progRecur <- as.logical(rbinom(nrow(df), 1, 0.1))
# Define data type of timestamp column.
class(df$ts) <- 'Date'
# Add a unique row identifier.
df$rowID <- c(1:nrow(df))

return(df)
}
  
