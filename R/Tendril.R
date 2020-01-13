#' Tendril
#' @description
#' Function to calculate coordinates and statistical measures used to create a tendril plot
#' @param mydata A dataframe containing the data for the tendril calculations
#' @param rotations a vector of same length as mydata containing the rotation factors for all the events
#' @param Unique.Subject.Identifier The name of the column containing the unique patients IDs
#' @param Terms The name of the column containing the name of the tendrils (e.g. adverse event terms)
#' @param Treat The name of the column containing the name of the treatments
#' @param StartDay The name column containing the days of the events
#' @param Treatments The names of the two treatments to be included in the tendril. The first treatment bends to the right and second treatment bends to the left. Must be a vector of two elements and the two elements must be found in the Treatment column
#' @param AEfreqThreshold The minimum frequency threshold of events to be included in the analysis. Default is 50
#' @param Tag A tag or comment associated with the analysis
#' @param SubjList A dataframe containing subject IDs and treatments
#' @param SubjList.subject The name of the columns in SubjList containing the subjects IDs
#' @param SubjList.treatment The name of the columns in SubjList containing the treatments
#' @param SubjList.dropoutday The name of the column in SubjList containing the dropoutday
#' @param compensate_imbalance_groups Boolean Whether the rotation factors have been compensated for imbalance in the groups
#' @param filter_double_events Boolean whether to filter out events duplicated in subject id and adverse effect
#' @param suppress_warnings Boolean whether to suppress warnings from chi squared approximation may be incorrect
#' @return
#' The function return an object of class tendril. The object contains the original dataset added with the tendril coordinates,
#' all the function arguments and a dataframe with the results from statistical analysis
#' \itemize{
#' \item{data$data }{: Dataframe of orginal data, coordinates and stat results}
#' \item{data$Unique.Subject.Identifier }{: Column containing subject IDs}
#' \item{data$Terms }{: Column containing the name of the tendrils}
#' \item{data$Treat }{: Column containing the name of the treatments}
#' \item{data$StartDay }{: Column containing the days of the events}
#' \item{data$Treatments }{: The names of the treatments causing the tendrils to bend}
#' \item{data$AEfreqThreshold }{: The frequency threshold for the events to be included in the analysis}
#' \item{data$Tag }{: A tag or comment associated with the analysis}
#' \item{data$n.tot }{: A dataframe with the total number of events for each treatment. Used in the statistical calculations}
#' \item{data$SubjList }{: A dataframe containing subject IDs and treatments}
#' \item{data$SubjList.subject }{: The name of the columns in SubjList containing the subjects IDs}
#' \item{data$SubjList.treatment }{: The name of the columns in SubjList containing the treatments}
#' \item{data$SubjList.dropoutday }{: The name of the column in SubjList containing the dropoutday}
#' \item{data$rotation_vector }{: Rotation vector used to generate the tendril}
#' \item{data$compensate_imbalance_groups }{: Boolean Whether the rotation factors have been compensated for imbalance in the groups}
#' }
#' @details
#' The function accepts a dataframe with at least 4 columns named as the arguments Unique.Subject.Identifier, Terms, Treat and StartDay.
#'
#' Two treatments must be given as arguments, and at least one of the two treatments must be found in the Treatment column
#'
#' The function returns an object of class tendril. The object contains the coordinates for the tendril plot and the arguments of the tendril function
#'
#' The result of the function can be plotted with plot()
#'
#' The result can be saved to file using write.table() with argument row.names = FALSE
#' @examples
#' data <- Tendril(mydata = TendrilData,
#' rotations = Rotations,
#' AEfreqThreshold=9,
#' Tag = "Comment",
#' Treatments = c("placebo", "active"),
#' Unique.Subject.Identifier = "subjid",
#' Terms = "ae",
#' Treat = "treatment",
#' StartDay = "day",
#' SubjList = SubjList,
#' SubjList.subject = "subjid",
#' SubjList.treatment = "treatment"
#' )
#'
#' plot(data)
#' @export

Tendril <- function(mydata,
                    rotations,
                    AEfreqThreshold=50,
                    Tag = "Comment",
                    Treatments = c("Active", "Placebo"),
                    Unique.Subject.Identifier = "Unique.Subject.Identifier",
                    Terms = "Dictionary.Derived.Term",
                    Treat = "Actual.Treatment...DB",
                    StartDay = "Analysis.Start.Relative.Day",
                    SubjList = NULL,
                    SubjList.subject = NULL,
                    SubjList.treatment = NULL,
                    SubjList.dropoutday = NULL,
                    compensate_imbalance_groups = FALSE,
                    filter_double_events = FALSE,
                    suppress_warnings = FALSE){

  #check input data
  .validate_tendril_data(
    mydata, rotations, Treatments, Terms, Unique.Subject.Identifier,
    Treat, StartDay, SubjList, SubjList.subject, SubjList.dropoutday,
    AEfreqThreshold, filter_double_events, suppress_warnings
    )

  if (length(rotations) == 1){
    rotations <- rep(rotations, nrow(mydata))
  }

  rotations <- rotations[order(mydata[, StartDay])]
  mydata <- mydata[order(mydata[, StartDay]),]

  mydata <- dataSetup(mydata, rotations, Unique.Subject.Identifier, Terms, Treat, StartDay)

  SubjList <- SubjList[SubjList[, SubjList.treatment] %in% Treatments,]       # Remove unwanted treatments from SubjList

  # Remove any records with unwated treatments, non-positice start days or missing values of key variables from mydata
  mydata <- (mydata[mydata$Treat %in% Treatments & mydata$StartDay>0, ])
  mydata <- (mydata[!is.na(Unique.Subject.Identifier), ])
  mydata <- (mydata[!is.na(Terms), ])
  mydata <- (mydata[!is.na(Treat), ])
  mydata <- (mydata[!is.na(StartDay), ])

  if (!is.null(SubjList)){
    mydata <- mydata[mydata$Unique.Subject.Identifier %in% unique(SubjList[, SubjList.subject]), ] # Remove unwanted data
  }
  mydata$Treat <- factor(as.character(mydata$Treat), levels = Treatments)  # Only relevant levels of treatments wanted

  # Internal function to test whether dropoutday is before a given value for a specified subject
  if ((!is.null(SubjList.dropoutday)) &&
      (SubjList.dropoutday %in% names(SubjList))){
    check_dropout_day <- function(subject,
                                  day,
                                  SubjList,
                                  SubjList.subject,
                                  SubjList.dropoutday){
      SubjList[as.character(SubjList[, SubjList.subject]) == as.character(subject), SubjList.dropoutday] < day
    }

    # Determine whether an event is after the specified dropoutday
    event_after_dropout <- mapply(check_dropout_day, mydata[, "Unique.Subject.Identifier"], mydata[, "StartDay"],
                                  MoreArgs=list(SubjList=SubjList, SubjList.subject=SubjList.subject, SubjList.dropoutday=SubjList.dropoutday))

    # Remove events after the specified dropoutday
    mydata <- mydata[!event_after_dropout,]
    if (any(event_after_dropout)){
      warning("There are events in mydata that occur on a day after the patient dropoutday specified in SubjList. Those events are filtered out before the analysis was performed.")
    }
  }

  # Filter out double events if input argument "filter_double_events" is set to true
  if (filter_double_events){
    mydata <- mydata[!duplicated(mydata[, c("Unique.Subject.Identifier", "Terms")]),]
  }

  # If no data is given on day of dropout. Assume there were no dropouts of patients
  if (!is.null(SubjList)){
    if (!is.null(SubjList.dropoutday)){
      if (!SubjList.dropoutday %in% names(SubjList)){
        SubjList[,SubjList.dropoutday] = max(mydata$StartDay)
      }
    }
  }

  # Calculate rotations based on proportionality

  if (compensate_imbalance_groups){
    if (!is.null(SubjList)){
      if (!is.null(SubjList.dropoutday)){
        mydata$rot.factor <- mydata$rot.factor*
                              mapply(function(day,              # anonymous function to calulate rotation adjustments
                                              treatment,
                                              SubjList,
                                              SubjList.treatment,
                                              SubjList.dropoutday){
                            nrow(SubjList)/(length(Treatments)*
                                              sum(SubjList[SubjList[,SubjList.treatment] == treatment,
                                                           SubjList.dropoutday] >= day))
                          },
                          mydata$StartDay,                      # per event day
                          mydata$Treat,                         # and per treatment
                          MoreArgs=list(SubjList=SubjList,      # given SubjList
                                        SubjList.treatment=SubjList.treatment,
                                        SubjList.dropoutday=SubjList.dropoutday))
      } else {
        mydata$rot.factor <- mydata$rot.factor*
                              mapply(function(treatment,
                                              SubjList,         # anonymous function to calculate rotation adjustments
                                              SubjList.treatment){
                            nrow(SubjList)/(length(Treatments)*
                                              nrow(SubjList[SubjList[,SubjList.treatment] == treatment,]))
                          }, mydata$Treat,                      # per treatment
                          MoreArgs=list(SubjList=SubjList,      # given SubjList
                                        SubjList.treatment=SubjList.treatment))
      }
    } else {
      stop("Imbalance groups can not be compensated when no SubjList is supplied.")
    }
  }

  if (any(is.infinite(rotations))){
    stop("Rotations based on proportionality have become Inf. This indicates that Inf has been supplied as rotations value or a problem has been encountered in the calculation.")
  }

  # Only work with AE terms that have AEfreqThreshold or more AEs in at least one treatment arm
  tab.all <- as.data.frame(with(mydata, table(Terms, Treat)))
  tab <- tab.all[tab.all$Freq>=AEfreqThreshold,]

  .validate_tendril_results(tab)

  AE <- unique(tab$Terms)
  mydata <- mydata[mydata$Terms %in% AE, ]

  # Prepare dataset for running through tendril algorithm
  tendril.data <- NULL

  #compute all arms
  for(i in AE) {
    subdata <- mydata[mydata$Terms==i, ]
    tendril.data <- rbind(tendril.data, tendril_cx(subdata, Treatments))
  }

  # Calculate coordinates and arguments etc
  tendril.data <- cxDataFormat(tendril.data)

  tendril.data$Tag <- Tag

  #add a count for number of events in each arm
  for (i in 1: length(unique(tendril.data$Terms))){
    subset.length <- length(which(tendril.data$Terms == unique(tendril.data$Terms)[i]))
    tendril.data$TermsCount[tendril.data$Terms == unique(tendril.data$Terms)[i]] <- subset.length
  }

  # remove variables not required from the dataset
  tendril.data <- dplyr::select(tendril.data,
                                -old.day,
                                -mod,
                                -dir.x,
                                -k,
                                -dir.y,
                                -angsum,
                                -cx,
                                -ang)

  # Set Terms to factor
  tendril.data$Terms <- factor(tendril.data$Terms)

  #list of results
  tendril.retval <- list(data = tendril.data,
                         Terms = Terms,
                         Unique.Subject.Identifier = Unique.Subject.Identifier,
                         Treat = Treat,
                         StartDay = StartDay,
                         Treatments = Treatments,
                         AEfreqThreshold = AEfreqThreshold,
                         Tag = Tag,
                         SubjList = SubjList,
                         SubjList.subject = SubjList.subject,
                         SubjList.treatment = SubjList.treatment,
                         SubjList.dropoutday = SubjList.dropoutday,
                         rotation_vector = mydata$rot.factor,
                         compensate_imbalance_groups = compensate_imbalance_groups
                         )

  class(tendril.retval) <- "Tendril"

  if (!is.null(SubjList)){
    tendril.retval <- tendril_stat(tendril.retval, suppress_warnings)
  } else {
    tendril.retval$n.tot <- NULL
    warning("No SubjList specified. You will still be able to plot a tendril plot. However, care should be taken when intepreting, since an imbalance of the amount of persons per treatment will affect the bending.", call. = FALSE)
  }

  return(tendril.retval)
}

#### Private functions ######

# Validates the data passed to Tendril
.validate_tendril_data <- function(
                                  mydata,
                                  rotations,
                                  Treatments,
                                  Terms,
                                  Unique.Subject.Identifier,
                                  Treat,
                                  StartDay,
                                  SubjList,
                                  SubjList.subject,
                                  SubjList.dropoutday,
                                  AEfreqThreshold,
                                  filter_double_events,
                                  suppress_warnings){

  if (!"data.frame" %in% class(mydata)) {
    stop("The dataset is not a dataframe")
  }
  if ((!"data.frame" %in% class(SubjList)) && (!is.null(SubjList))) {
    stop("SubjList is not a dataframe")
  }
  if (!(dim(mydata)[1] == length(rotations) || length(rotations) == 1) ||
      !is.numeric(rotations)) {
    stop("Rotations must be a numeric vector of length 1 or equal to the number of rows in mydata")
  }
  if (sum(
        c(Treat, Terms, Unique.Subject.Identifier, StartDay) %in% colnames(mydata)
      ) != 4){
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
  if (is.not.positive.integer(AEfreqThreshold)){
    stop("The frequency must be a positive integer")
  }
  if (!(is.logical(filter_double_events) && !is.na(filter_double_events)) ||
      !length(filter_double_events) == 1) {
    stop("filter_double_events must be a boolean of length 1")
  }
  if (!(is.logical(suppress_warnings) && !is.na(suppress_warnings)) ||
      !length(suppress_warnings) == 1) {
    stop("suppress_warnings must be a boolean of length 1")
  }
}

.validate_tendril_results <- function(tab){
  if (dim(tab)[1] == 0){
    stop("No sample with the defined frequency threshold")
  }
}
