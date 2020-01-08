#------------------------
# check if an input is a positive integer
is_not_positive_integer <- function(x){
  if (is.na(x) || !is.numeric(x) || x <= 0 || x %% 1 != 0 || length(x) != 1){
    res <- TRUE

  } else {
    res <- FALSE
  }
  return(res)
}
#-------------------
#validate tendril() input and intermediate table
validate_tendril_data <- function(mydata, rotations, Treatments, Terms, Unique.Subject.Identifier, Treat, StartDay, SubjList, SubjList.subject, SubjList.dropoutday, AEfreqThreshold, filter_double_events, suppress_warnings){
  if (!"data.frame" %in% class(mydata)){
    stop("The dataset is not a dataframe")
  }
  if ((!"data.frame" %in% class(SubjList)) && (!is.null(SubjList))){
    stop("SubjList is not a dataframe")
  }
  if (!(dim(mydata)[1] == length(rotations) || length(rotations) == 1) || !is.numeric(rotations)){
    stop("Rotations must be a numeric vector of length 1 or equal to the number of rows in mydata")
  }
  if (sum(c(Treat, Terms, Unique.Subject.Identifier, StartDay) %in% colnames(mydata)) != 4){
    stop("One or more columns not available in the dataset")
  }
  if (length(Treatments) != 2){
    stop("2 Treatment must be provided")
  }
  if (all(Treatments %in% mydata[[Treat]]) == FALSE){
    stop("At least one of the Treatments is not available in the Treatment column")
  }
  if (!is.numeric(mydata[[StartDay]])){
    stop("Days column must contain only numeric values")
  }
  if (is_not_positive_integer(AEfreqThreshold)){
    stop("The frequency must be a positive integer")
  }
  if (!(is.logical(filter_double_events) && !is.na(filter_double_events))|| !length(filter_double_events)==1){
    stop("filter_double_events must be a boolean of length 1")
  }
  if (!(is.logical(suppress_warnings) && !is.na(suppress_warnings))|| !length(suppress_warnings)==1){
    stop("suppress_warnings must be a boolean of length 1")
  }
}

validate_tendril_results <- function(tab){
  if (dim(tab)[1] == 0){
    stop("No sample with the defined frequency threshold")
  }
}
#-------------------
#validate tendril.stat() input
validate_tendril_stat <- function(dataset, SubjList, Unique.Subject.Identifier, treatment){
  if (!"Tendril" %in% class(dataset)){
    stop("Input data is not of class tendril")
  }
  if (!"data.frame" %in% class(SubjList)){
    stop("SubjList is not of class data.frame")
  }
  if (sum(c(Unique.Subject.Identifier, treatment) %in% colnames(SubjList)) < 2){
    stop("Not all the columns are available in the SubjList")
  }
  if (sum(unique(as.character(SubjList[, treatment])) %in% dataset$Treatments) < 2){
    stop("Not all the treatments in the list are available in the dataframe")
  }

}
#-------------------
#validate tendril.perm() input dataset
validate_perm_data <- function(dataset, PermTerm, n.perm, perm.from.day, pi.low, pi.high){
  if (length(PermTerm) != 1 || !PermTerm %in% dataset$data$Terms){
    stop("PermTerm not valid")
  }
  if (is_not_positive_integer(n.perm)){
    stop("The number of permutations must be a positive integer")
  }
  if (!is.numeric(pi.low) | !is.numeric(pi.high)){
    stop("pi.low and pi.high must be numeric")
  }
  if (pi.low >= pi.high){
    stop("pi.low must be smaller than pi.high.")
  }
  if (pi.low <0 | pi.high<0 | pi.low >1 | pi.high>1){
    stop("pi.low and pi.high must be between 0 and 1")
  }
}

validate_perm_day <- function(data, perm.from.day){
  if (is_not_positive_integer(perm.from.day)){
    stop("The starting day for permutations must be positive")
  }

  if (length(which(data$StartDay>=perm.from.day)) < 1){
    stop("Not enough datapoint available. Lower the permutation starting day")
  }
}
