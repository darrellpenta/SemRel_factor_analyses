StatRep <- function(subset.names) {
  subset.list = data.list[subset.names]
  
  ds      <- data.frame(data = c(subset.names))
  ds$n    <- lapply(subset.list, nrow)
  ds$N    <- lapply(subset.list, nrow)
  ds$mean <- lapply(subset.list, function(item) mean(item$error))
  ds$sd   <- lapply(subset.list, function(item) sd(item$error))
  ds$se   <- lapply(subset.list, function(item) sd(item$error)/sqrt(length(item$error)))
  return(ds)  
}



#--------------------------------------------------------------------------
# This function creates a dataframe containing descriptive statistics for specificed subsets of an original data file

#  Args:
#    subset.names: a vector containing the names for the subsets to include, assuming subsets have been specified in the anaylses scrtipt

#  Returns:
#      a data frame with descriptive stats