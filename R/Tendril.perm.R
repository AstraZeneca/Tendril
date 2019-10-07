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
#'   AEfreqTreshold = 9,
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
  validate.perm.data(tendril, PermTerm, n.perm, perm.from.day, pi.low, pi.high)

  retval = list(tendril = tendril)
  class(retval) <- "TendrilPerm"

  #prepare data
  Unique.Subject.Identifier <- tendril$SubjList.subject
  treatment <- tendril$SubjList.treatment
  dropoutday <- tendril$SubjList.dropoutday
  SubjList <- tendril$SubjList
  Treats <- tendril$Treatments
  data <- tendril$data[tendril$data$Terms==PermTerm, ]
  rotation_vector <- tendril$rotation_vector
  compensate_imbalance_groups <- tendril$compensate_imbalance_groups

  #check perm.from.day
  validate.perm.day(data, perm.from.day)

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

    res <- Tendril.cx(permdf, Treats)
    res$label <- paste("Permutation",i, sep = " ")

    tendril.perm.all <- rbind(tendril.perm.all, res)

  }

  # Calculate coordinates and arguments etc
  val <- tendril.perm.all$cx
  tendril.perm.all$type <- "Permutation"
  tendril.perm.all <- cxDataFormat(tendril.perm.all)
  tendril.perm.all$PermTreat <- NULL
  tendril.perm.all$perm.from.day<-perm.from.day

  #add results
  retval$Permterm <- PermTerm
  retval$perm.data <- tendril.perm.all
  retval$tendril.pi <- TendrilPi(tendril, retval$Permterm, retval$perm.data, pi.low, pi.high, perm.from.day)

  return(retval)
}
