# Function that modifies the passed data in place, adding the x-y and angles
# of the tendrils. This function is technically an internal method to the
# Tendril object, but we don't have proper OO in this implementation so
# take it at face value.
tendril_cx <- function(data, Treatments) {
  # Sort in time
  data <- data[order(data$StartDay, decreasing = FALSE),]

  # Required to pass R CMD check
  StartDay <- NULL
  old.day <- NULL
  mod <- NULL
  angsum <- NULL

  # Add column shifted 1 step to calculate difference
  data$old.day <- c(0, data$StartDay[1:(length(data$StartDay)-1)])

  # Calculate length and Translate treatment sequence to angles
  data <- transform(data,
                    mod = StartDay - old.day,
                    dir = ifelse(
                      data$Treat == Treatments[1],
                      -1 * data$rot.factor,
                      1 * data$rot.factor
                      )
                    )

  # Make a 0/1 vector that is used to decide which angles
  # that should be summed up to angsum
  data <- transform(data, k = as.numeric(mod != 0))

  # Sum the rotations for each StartDay
  temp <- stats::aggregate(dir ~ StartDay, data = data, sum)

  # Create a new column with new rotation data
  data <- merge(data, temp, by = "StartDay")

  # Calculate cumulative tendril angles
  data$angsum <- cumsum(data$dir.y * data$k)
  data <- transform(data,
                    cx = complex(modulus = mod,
                                 argument = (pi/2 + angsum * pi/180)))
  data$cx <- cumsum(data$cx)

  return(data)
}
