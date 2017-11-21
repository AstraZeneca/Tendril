#Do the Percentile calculations on the permutations
Tendril.pi <- function(dataset, pi.low=0.1, pi.high=0.9, perm.from.day = 1) {

  actualdata <- dataset$data[dataset$data$Terms == dataset$Permterm,]
  actualdata$type <- "Actual"
  actualdata$label <- "Actual"
  actualdata$perm.from.day = perm.from.day
  data <- dataset$perm.data
  polydata <- data[data$type == "Permutation" & data$StartDay >= perm.from.day, ]

  #polydata[polydata$ang<0,]$ang <- polydata[polydata$ang<0,]$ang + 2*pi # OBS, a fix that e.g. does not take angles > 2pi and other specialities into account!
  polydata <- polydata[order(polydata$ang, decreasing = FALSE), ]
  labelleddata <- NULL
  for(i in unique(polydata$StartDay)) {
    subset<-polydata[polydata$StartDay==i,]
    size <- dim(subset)[1]
    limit.low <- size*pi.low
    limit.high <- size*pi.high

    subset$label[limit.low]<-"Low.percentile"
    subset$label[limit.high]<-"High.percentile"
    labelleddata<-rbind(labelleddata, subset)
}

  labelleddata <- with(labelleddata, labelleddata[ order(type,StartDay),])
  labelleddata$type[labelleddata$label=="Low.percentile" | labelleddata$label=="High.percentile"] <- "Percentile"
  labelleddata <- labelleddata[,colnames(labelleddata) %in% colnames(actualdata)]
  data <- rbind(labelleddata, actualdata)
  data$perm.from.day <- perm.from.day
  data <- data[data$type == "Percentile",]
  class(data) <- "tendrilPi"

  return(data)
}
