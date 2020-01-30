#------------------------
# check if an input is a positive integer
is.not.positive.integer <- function(x){
  if (length(x) != 1 || is.na(x) || !is.numeric(x) || x <= 0 || x %% 1 != 0){
    res <- TRUE
  } else {
    res <- FALSE
  }
  return(res)
}
