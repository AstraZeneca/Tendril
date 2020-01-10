#------------------------
# check if an input is a positive integer
is.not.positive.integer <- function(x){
  if (is.na(x) || !is.numeric(x) || x <= 0 || x %% 1 != 0 || length(x) != 1){
    res <- TRUE

  } else {
    res <- FALSE
  }
  return(res)
}
#-------------------
#validate tendril.stat() input
validate.tendril.stat <- function(dataset, SubjList, Unique.Subject.Identifier, treatment){
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
validate.perm.data <- function(dataset, PermTerm, n.perm, perm.from.day, pi.low, pi.high){
  if (length(PermTerm) != 1 || !PermTerm %in% dataset$data$Terms){
    stop("PermTerm not valid")
  }
  if (is.not.positive.integer(n.perm)){
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

validate.perm.day <- function(data, perm.from.day){
  if (is.not.positive.integer(perm.from.day)){
    stop("The starting day for permutations must be positive")
  }

  if (length(which(data$StartDay>=perm.from.day)) < 1){
    stop("Not enough datapoint available. Lower the permutation starting day")
  }
}
