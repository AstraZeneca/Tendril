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
