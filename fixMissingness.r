# The purpose of this script to impute the missing values using
# the approach outlined in the publication.
#
# Any missing values for categorical variables are imputed with an
# 'Unknown' category, continuous variables are imputed with a
# meaningful default or zero, time variables are not imputed.
#
# The fixMissing() function takes the following arguments:
#   dataset = the dataset to be imputed.
#   colIdx_cat = the indices of the columns containing categorical
#                 predictors.
#   colIdx_cont = the indices of the columns containing continuous
#                 predictors.
#   contImp_list = a list the same length as colIdx_cont that 
#                 indicates what values or methods to use for 
#                 imputation. List elements must either contains
#                 the key terms "median" or "mean" (which will
#                 impute missing values with the median
#                 and arithmetic mean values, respectively), a 
#                 specified value to be used for umputation, or
#                 an zero as a numeric data type (which will be
#                 used as the preferred imputation value. Note:
#                 this option assumes a meaningful zero).
#                 


fixMissing <- function(dataset, colIdx_cat, colIdx_cont, contImp_list)
{
# Set up.
list_of_packages <- c("radiant.data")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
for (i in 1:length(list_of_packages))
{
  require(list_of_packages[i],character.only = T)
}

# Identify the categorical predictors.
idx_col_isCat <- which(
                      lapply(
                            dataset[,!(names(dataset) %in% c("rowID", "PatientID","ts", "progRecur"))],
                            is.factor
                             )==T
                       )
# Impute categorical variables with 'Unknown'.
d_temp <- dataset[,idx_col_isCat]
d_temp <- as.data.frame(
                        lapply(
                              d_temp,
                              function(x) {refactor(addNA(x), levs = levels(x), repl = "Unknown")}
                               )
                        )
d[,idx_col_isfactor] <- d_temp
# Impute numeric variables.
# For the fake dataset, I will impute with zero.
idx_col_isnumeric <- which(lapply(d[,!(names(d) %in% c("rowID","worsen"))],is.numeric)==T)
d_temp <- d[,idx_col_isnumeric]
d_temp[is.na(d_temp)] <- 0
d[,idx_col_isnumeric] <- d_temp

}