#' Tendril permutations
#' @description
#' Function to compute the permutations of one specified tendril, starting from a specific day.
#' Permutations are simulated under the null hypothesis. Thus, on average, there will be an equal number of events on each treatment arm.
#' @details
#' Make permutation analysis to a specific type of event, as specified in PermTerm.
#' @param tendril an object of class tendril as produced by Tendril()
#' @param PermTerm the name of the type of event (tendril) to calculate permutations on
#' @param n.perm the number of permutations. Default 100
#' @param perm.from.day the starting day for the permutation calculations. Default 1
#' @param pi.low percentile low value. Default 0.1
#' @param pi.high percentile high value. Default 0.9
#' @return
#' The function return an object of class TendrilPerm containing all the input data and a dataframe of permutation results. Use:
#'
#' data$perm.data
#'
#' and
#'
#' data$tendril.pi
#'
#' and
#'
#' data$tendril
#'
#' to access the permutations, percentiles dataframes, and tendril data respectively
#'
#' @examples
#' # Create tendril
#' tendril <- Tendril(mydata = TendrilData,
#'   rotations = Rotations,
#'   AEfreqThreshold = 9,
#'   Tag = "Comment",
#'   Treatments = c("placebo", "active"),
#'   Unique.Subject.Identifier = "subjid",
#'   Terms = "ae",
#'   Treat = "treatment",
#'   StartDay = "day",
#'   SubjList = SubjList,
#'   SubjList.subject = "subjid",
#'   SubjList.treatment = "treatment"
#' )
#'
#' # Compute permutations
#' perm.data <- TendrilPerm(tendril = tendril,
#'   PermTerm="AE40",
#'   n.perm = 200,
#'   perm.from.day = 1)
#'
#' # Plot results
#' plot(perm.data)
#' plot(perm.data, percentile = TRUE)
#' @export

TendrilPerm <- function(tendril, PermTerm, n.perm=100, perm.from.day=1, pi.low=0.1, pi.high=0.9) {
  `%>%` <- magrittr::`%>%`
  #check input data
  .validate_perm_data(tendril, PermTerm, n.perm, perm.from.day, pi.low, pi.high)

  #prepare data
  Unique.Subject.Identifier <- tendril$SubjList.subject
  treatment <- tendril$SubjList.treatment
  dropoutday <- tendril$SubjList.dropoutday
  SubjList <- tendril$SubjList
  Treats <- tendril$Treatments
  data <- tendril$data[tendril$data$Terms==PermTerm, ]
  rotation_vector <- tendril$rotation_vector[tendril$data$Terms==PermTerm]
  compensate_imbalance_groups <- tendril$compensate_imbalance_groups

  # Prepare TendrilPerm object to return
  retval <- list(tendril = tendril)
  retval$tendril$data <- data
  retval$tendril$rotation_vector <- rotation_vector
  class(retval) <- "TendrilPerm"

  #check perm.from.day
  .validate_perm_day(data, perm.from.day)

  perm.nr <- which(data$StartDay>=perm.from.day)

  tendril.perm.all <- NULL

  calc_proportional_factor <- function(treatment_event, day, SubjList, dropoutday, Treats){
    if (is.null(dropoutday)){
      nrow(SubjList)/(length(Treats)*nrow(SubjList[SubjList[, "PermTreat"] == treatment_event,]))
    } else {
      nrow(SubjList)/(length(Treats)*sum(SubjList[SubjList[,"PermTreat"] == treatment_event, "dropoutday"] >= day))
    }
  }

  for(i in 1:n.perm) {

    permdf<-NULL
    ## Make permutations to treatment assignment  ##
    if (!is.null(dropoutday)){
      PermData <- data.frame(SubjID=as.character(SubjList[, Unique.Subject.Identifier]), dropoutday=as.integer(SubjList[, dropoutday]),
                             PermTreat=sample(SubjList[, treatment])) # Draw from actual distribution of treatments
    } else {
      PermData <- data.frame(SubjID=as.character(SubjList[, Unique.Subject.Identifier]),
                             PermTreat=sample(SubjList[, treatment])) # Draw from actual distribution of treatments
    }

    data <- data %>% dplyr::mutate_if(is.factor, as.character)
    PermData <- PermData %>% dplyr::mutate_if(is.factor, as.character)
    permdf <- dplyr::left_join(data, PermData, by = c("Unique.Subject.Identifier" = "SubjID"))
    permdf$Treat[perm.nr]<-as.character(permdf$PermTreat[perm.nr])
    permdf$StartDay<-as.integer(permdf$StartDay)
    permdf$rot.factor<-as.numeric(permdf$rot.factor)

    if (compensate_imbalance_groups){
      permdf$rot.factor[perm.nr] <- as.numeric(rotation_vector[perm.nr]*unlist(mapply(calc_proportional_factor,
                                                                               permdf$Treat[perm.nr],
                                                                               permdf$StartDay[perm.nr],
                                                                               MoreArgs=list(SubjList=PermData,
                                                                               dropoutday=dropoutday,
                                                                               Treats = Treats))))
    }

    res <- tendril_cx(permdf, Treats)
    res$label <- paste("Permutation",i, sep = " ")

    tendril.perm.all <- rbind(tendril.perm.all, res)

  }

  # Calculate coordinates and arguments etc
  val <- tendril.perm.all$cx
  tendril.perm.all$type <- "Permutation"
  tendril.perm.all <- cxDataFormat(tendril.perm.all)
  tendril.perm.all$PermTreat <- NULL
  tendril.perm.all$perm.from.day<-perm.from.day

  # Keep only required variables in tendril.perm.all
  tendril.perm.all <- dplyr::select(tendril.perm.all,
                                    StartDay,
                                    Terms,
                                    x,
                                    y,
                                    label,
                                    type,
                                    ang,
                                    perm.from.day)
  #add results
  retval$Permterm <- PermTerm
  retval$perm.data <- tendril.perm.all
  retval$tendril.pi <- TendrilPi(tendril, retval$Permterm, retval$perm.data, pi.low, pi.high, perm.from.day)

  return(retval)
}

########## Private ######

# validate tendril.perm() input dataset
.validate_perm_data <- function(dataset, PermTerm, n.perm, perm.from.day, pi.low, pi.high){
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


.validate_perm_day <- function(data, perm.from.day){
  if (is.not.positive.integer(perm.from.day)){
    stop("The starting day for permutations must be positive")
  }

  if (length(which(data$StartDay>=perm.from.day)) < 1){
    stop("Not enough datapoint available. Lower the permutation starting day")
  }
}
