# This script generates data.
#
# The dataset will have nCont-many continuous variables and 
# nCat-many categorical variables, for nPat-many patients with
# an average of nEvent-many events.

dataGen <- function(nPat, nEvent, nCont, nCat)
{
# Set up.
set.seed(1)
if(missing(nPat)) nPat = 100;
if(missing(nEvent)) nEvent = 10;
if(missing(nCont)) nCont = 20;
if(missing(nCat)) nCat = 50;
  
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
class(ts) <- 'Date'
df$ts <- ts

# Generate data for all required continuous variables.
contVars <-  data.frame(replicate(nCont, expr = rnorm(nrow(df), 10, 4)))
names(contVars) <- c(paste("contVar", nCont, sep = "."))

# Generate data for all required categorical variables.
catVars <- data.frame(replicate(nCat, expr = rpois(nrow(df), sample(1:4, 1))))
names(catVars) <- c(paste("catVar", nCat, sep = "."))

# Bind variables to main dataframe.
df <- cbind(df, contVars, catVars)
return(df)
}
