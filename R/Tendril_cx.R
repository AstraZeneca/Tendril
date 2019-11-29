#function to compute the x-y and angles of the tendrils
Tendril_cx <- function(data, Treatments) {

  # Sort in time
  data <- data[order(data$StartDay, decreasing = FALSE),]

  #required to pass R CMD check
  StartDay<-NULL
  old.day<-NULL
  mod<-NULL
  angsum<-NULL

  data$old.day <- c(0,data$StartDay[1:(length(data$StartDay)-1)]) # Add column shifted 1 step to calculate difference
  data <- transform(data, mod=StartDay-old.day, dir=ifelse(data$Treat==Treatments[1], -1*data$rot.factor, 1*data$rot.factor)) # Calculate length and Translate treatment sequence to angles
  data <- transform(data, k=as.numeric(mod != 0)) # Make a 0/1 vector that is used to decide which angles that should be summed up to angsum
  temp <- stats::aggregate(dir~StartDay, data=data, sum) # Sum the rotations for each StartDay
  data <- merge(data, temp, by = "StartDay") # Create a new column with new rotation data
  data$angsum <- cumsum(data$dir.y*data$k) # Calculate cumulative tendril angles
  data <- transform(data, cx=complex(modulus = mod, argument = (pi/2 + angsum*pi/180)))
  data$cx <- cumsum(data$cx)

  return(data)
}
