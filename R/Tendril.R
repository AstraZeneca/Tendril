#' @importFrom dplyr left_join mutate_if
#' @importFrom magrittr %>%
#' @importFrom stats aggregate fisher.test na.omit p.adjust prop.test
#' @importFrom utils tail
#' @import ggplot2
#' @importFrom reshape2 dcast
#' @importFrom scales rescale
NULL


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
#' @param AEfreqTreshold The minimum frequency treshold of events to be included in the analysis. Default is 50
#' @param Tag A tag or comment associated with the analysis
#' @param SubjList A dataframe containing subject IDs and treatments
#' @param SubjList.subject The name of the columns in SubjList containing the subjects IDs
#' @param SubjList.treatment The name of the columns in SubjList containing the treatments
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
#' \item{data$AEfreqTreshold }{: The frequency treshold for the events to be included in the analysis}
#' \item{data$Tag }{: A tag or comment associated with the analysis}
#' \item{data$n.tot }{: A dataframe with the total number of events for each treatment. Used in the statistical calculations}
#' \item{data$SubjList }{: A dataframe containing subject IDs and treatments}
#' \item{data$SubjList.subject }{: The name of the columns in SubjList containing the subjects IDs}
#' \item{data$SubjList.treatment }{: The name of the columns in SubjList containing the treatments}
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
#' AEfreqTreshold=9,
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
                    AEfreqTreshold=50,
                    Tag = "Comment",
                    Treatments = c("Active", "Placebo"),
                    Unique.Subject.Identifier = "Unique.Subject.Identifier",
                    Terms = "Dictionary.Derived.Term",
                    Treat = "Actual.Treatment...DB",
                    StartDay = "Analysis.Start.Relative.Day",
                    SubjList,
                    SubjList.subject = "SubjList.Unique.Subject.Identifier",
                    SubjList.treatment = "SubjList.treatment.name"){

  #check input data
  validate.tendril.data(mydata, rotations, Treatments, Terms, Unique.Subject.Identifier, Treat, StartDay, AEfreqTreshold)

  mydata$col.id<-seq(1,dim(mydata)[1],1)
  full.dataset <- mydata #create backup to preserve extracolumn

  mydata <- dataSetup(mydata, rotations, Unique.Subject.Identifier, Terms, Treat, StartDay)

  SubjList <- SubjList[SubjList[, SubjList.treatment] %in% Treatments,] # Remove unwanted data
  mydata <- na.omit(mydata[mydata$Treat %in% Treatments & mydata$StartDay>0 & mydata$Unique.Subject.Identifier %in% unique(SubjList[, SubjList.subject]), ]) # Remove unwanted data
  mydata$Treat <- factor(as.character(mydata$Treat), levels = Treatments)  # Only relevant levels of treatments wanted

  # Only work with AE terms that have AEfreqTreshold or more AEs in at least one treatment arm
  tab.all <- as.data.frame(with(mydata, table(Terms, Treat)))
  tab <- tab.all[tab.all$Freq>=AEfreqTreshold,]

  validate.tendril.results(tab)

  AE <- unique(tab$Terms)
  mydata <- mydata[mydata$Terms %in% AE, ]

  # Prepare dataset for running through tendril algorithm
  tendril.data <- NULL

  #compute all arms
  for(i in AE) {
    subdata <- mydata[mydata$Terms==i, ]
    tendril.data <- rbind(tendril.data, Tendril.cx(subdata, Treatments))
  }

  # Calculate coordinates and arguments etc
  tendril.data <- cxDataFormat(tendril.data)

  tendril.data <- joinData(tendril.data, full.dataset, Tag)

  #add a count for number of events in each arm
  for (i in 1: length(unique(tendril.data$Terms))){
    subset.length <- length(which(tendril.data$Terms == unique(tendril.data$Terms)[i]))
    tendril.data$TermsCount[tendril.data$Terms == unique(tendril.data$Terms)[i]] <- subset.length
  }

  #list of results
  tendril.retval <- list(data = tendril.data,
                         Terms = Terms,
                         Unique.Subject.Identifier = Unique.Subject.Identifier,
                         Treat = Treat,
                         StartDay = StartDay,
                         Treatments = Treatments,
                         AEfreqTreshold = AEfreqTreshold,
                         Tag = Tag,
                         SubjList = SubjList,
                         SubjList.subject = SubjList.subject,
                         SubjList.treatment = SubjList.treatment
                         )

  class(tendril.retval) <- "tendril"

  tendril.retval <- Tendril.stat(tendril.retval)

  #format data: assign column names and remove unwanted columns
  data <- data.frame(Unique.Subject.Identifier = as.character(tendril.retval$data$Unique.Subject.Identifier),
                     Terms = tendril.retval$data$Terms,
                     Treat = tendril.retval$data$Treat,
                     StartDay = tendril.retval$data$StartDay,
                     rot.factor = tendril.retval$data$rot.factor,
                     x = tendril.retval$data$x,
                     y = tendril.retval$data$y,
                     Tag = tendril.retval$data$Tag,
                     p = tendril.retval$data$p,
                     p.adj = tendril.retval$data$p.adj,
                     fish = tendril.retval$data$fish,
                     rdiff = tendril.retval$data$rdiff,
                     RR = tendril.retval$data$RR,
                     OR = tendril.retval$data$OR,
                     FDR.tot = tendril.retval$data$FDR.tot,
                     TermsCount = tendril.retval$data$TermsCount
  )

  tendril.retval$data <- data

  return(tendril.retval)
}
