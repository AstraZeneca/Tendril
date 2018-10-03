## ----example_plot, echo=TRUE, warning=FALSE, message=FALSE, fig.width=6, fig.height=5----
library(Tendril)

data("TendrilData")

test <- Tendril(mydata = TendrilData,
                rotations = Rotations,
                AEfreqTreshold = 9,
                Tag = "Comment",
                Treatments = c("placebo", "active"),
                Unique.Subject.Identifier = "subjid",
                Terms = "ae",
                Treat = "treatment",
                StartDay = "day",
                SubjList = SubjList,
                SubjList.subject = "subjid",
                SubjList.treatment = "treatment"
)
  
plot(test, type = "basic")
plot(test, type = "basic", coloring = "p.adj")


## ----tendrildata_head, echo=FALSE, warning=FALSE, message=FALSE----------

head(TendrilData)

## ----call_tendril, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE----
#  test <- Tendril(mydata = TendrilData,
#                  rotations = Rotations,
#                  AEfreqTreshold = 9,
#                  Tag = "Comment",
#                  Treatments = c("placebo", "active"),
#                  Unique.Subject.Identifier = "subjid",
#                  Terms = "ae",
#                  Treat = "treatment",
#                  StartDay = "day",
#                  SubjList = SubjList,
#                  SubjList.subject = "subjid",
#                  SubjList.treatment = "treatment"
#  )
#  

## ----tendrilcx_alg, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE----
#  
#  #Calculate time between events (proportional to distance between points in the plot)
#  #Assign rotation to the left or to the right. EVENTS ON THE FIRST TREATMENT BENDS TO THE RIGHT
#  data$old.day <- c(0,data$StartDay[1:(length(data$StartDay)-1)])
#  data <- transform(data, mod=StartDay-old.day, dir=ifelse(data$Treat==Treatments[1], -1*data$rot.factor, 1*data$rot.factor))
#  
#  # Make a 0/1 vector that is used to decide which angles that should be summed up to angsum
#  data <- transform(data, k=as.numeric(mod != 0))
#  temp <- aggregate(dir~StartDay, data=data, sum) # Sum the rotations for each StartDay
#  data <- merge(data, temp, by = "StartDay") # Create a new column with new rotation data
#  
#  #Calculate cumulative tendril angles
#  data$angsum <- cumsum(data$dir.y*data$k) # Calculate cumulative tendril angles
#  
#  #Calculate complex numbers
#  data <- transform(data, cx=complex(modulus = mod, argument = (pi/2 + angsum*pi/180)))
#  data$cx <- cumsum(data$cx)
#  
#  #retrive coordinates from complex numbers
#  tendril.data <- transform(data,
#                              x=Re(cumsum(data$cx)),
#                              y=Im(cumsum(data$cx)),
#                              ang=Arg(cumsum(data$cx)),
#                              mod=Mod(cumsum(data$cx))

## ----example_tendril, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE----
#  test <- Tendril(mydata = TendrilData,
#                  rotations = Rotations,
#                  AEfreqTreshold = 9,
#                  Tag = "Comment",
#                  Treatments = c("placebo", "active"),
#                  Unique.Subject.Identifier = "subjid",
#                  Terms = "ae",
#                  Treat = "treatment",
#                  StartDay = "day",
#                  SubjList = SubjList,
#                  SubjList.subject = "subjid",
#                  SubjList.treatment = "treatment"
#  )
#  

## ----plot, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE-----------
#  # First create a tendril.res object with
#  #tendril.res <- Tendril(...)
#  p <- plot(tendril.res)

## ----plot2, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE----------
#  print(p)
#  #or simply
#  p

## ----plotbasic, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE------
#  # First create a tendril.res object with
#  #tendril.res <- Tendril(...)
#  p <- plot(tendril.res, type = "basic")

## ----perm.example, echo=TRUE, warning=FALSE, message=FALSE---------------
test1 <- Tendril.perm(dataset = test,
                               PermTerm = "AE40",
                               n.perm = 200,
                               perm.from.day = 1)

## ----perm.example2, echo=FALSE, warning=FALSE, message=FALSE-------------
test2 <- Tendril.perm(dataset = test,
                               PermTerm = "AE44",
                               n.perm = 200,
                               perm.from.day = 1)

## ----plot_perm, echo=TRUE, warning=FALSE, message=FALSE, fig.width=6, fig.height=5----
# First create a tendril.res object with
#tendril.res <- Tendril(...)

#To plot permutations:
#plot(test, type = "permutations") 

#To plot permutations and percentile:
plot(test1, type = "percentile") # Significantly more AEs on active treatment 
plot(test2, type = "percentile") # Balance of AEs on treatment arms

## ----example_single_rotation_value, eval = TRUE, echo=TRUE, warning=FALSE, message=FALSE, fig.width=6, fig.height=5----
# #load library
# library("Tendril")
# #compute tendril data
# data.tendril <- Tendril(mydata = TendrilData,
#                         rotations = 5,
#                         AEfreqTreshold = 9,
#                         Tag = "Comment",
#                         Treatments = c("placebo", "active"),
#                         Unique.Subject.Identifier = "subjid",
#                         Terms = "ae",
#                         Treat = "treatment",
#                         StartDay = "day",
#                         SubjList = SubjList,
#                         SubjList.subject = "subjid",
#                         SubjList.treatment = "treatment",
#                         filter_double_events = FALSE,
#                         suppress_warnings = FALSE)
# )
# 
# #compute permutations
# data.tendril <- Tendril.perm(dataset = data.tendril,
#                              PermTerm="AE40",
#                              n.perm = 200,
#                              perm.from.day = 1)
#

## ----full_example, echo=TRUE, eval=FALSE, warning=FALSE, message=FALSE----
#  #load library
#  library("Tendril")
#  #compute tendril data
#  data.tendril <- Tendril(mydata = TendrilData,
#                          rotations = Rotations,
#                  AEfreqTreshold = 9,
#                  Tag = "Comment",
#                  Treatments = c("placebo", "active"),
#                  Unique.Subject.Identifier = "subjid",
#                  Terms = "ae",
#                  Treat = "treatment",
#                  StartDay = "day",
#                  SubjList = SubjList,
#                  SubjList.subject = "subjid",
#                  SubjList.treatment = "treatment"
#  )
#  
#  #compute permutations
#  data.tendril <- Tendril.perm(dataset = data.tendril,
#                                 PermTerm="AE40",
#                                 n.perm = 200,
#                                 perm.from.day = 1)
#  
#  #do plot
#  p <- plot(data.tendril, type = "basic")
#  
#  #plot permutations
#  p <- plot(data.tendril, type = "permutations")
#  
#  #plot permutations and percentile
#  p <- plot(data.tendril, type = "percentile")
#  
#  #save tendril coordinates and stat results
#  write.table(data.tendril$data, "C:mydata.txt", sep="\t", row.names = FALSE)
#  
#  #save permutation coordinates
#  write.table(data.tendril$perm.data, "C:my_permutation_data.txt", sep="\t", row.names = FALSE)
#  
#  #save permutation percentiles
#  write.table(data.tendril$tendril.pi, "C:my_percentile_data.txt", sep="\t", row.names = FALSE)
#  

