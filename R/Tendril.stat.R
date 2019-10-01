Tendril.stat <- function(dataset, suppress_warnings) {

  SubjList <- dataset$SubjList
  Unique.Subject.Identifier <- dataset$SubjList.subject
  treatment <- dataset$SubjList.treatment

  #check the SubjList
  validate.tendril.stat(dataset, SubjList, Unique.Subject.Identifier, treatment)

  n.treat1 <- length(which(SubjList[, treatment] == dataset$Treatments[1]))
  n.treat2 <- length(which(SubjList[, treatment] == dataset$Treatments[2]))

  Treats <- dataset$Treatments

  data <- data.frame(SubjID = dataset$data$Unique.Subject.Identifier,
                     Terms = dataset$data$Terms,
                     TreatArm = dataset$data$Treat,
                     AEstartDay = dataset$data$StartDay,
                     Count = 1)

  # Count number of AEs for each AE term and AE start day in both arms
  CountAE <- aggregate(data=data, Count ~ Terms + TreatArm + AEstartDay, length)

  # Prepare data for making statistical tests
  CountAE.wide <- dcast(CountAE, Terms + AEstartDay ~ TreatArm, value.var="Count")
  CountAE.wide <- cbind(CountAE.wide, cum.treat1=0, cum.treat2=0, tot.treat1=0, tot.treat2=0, p=1, p.adj=1, fish=1, rdiff=0, RR=0, OR=0)
  CountAE.wide[[Treats[1]]][is.na(CountAE.wide[[Treats[1]]])] <- 0
  CountAE.wide[[Treats[2]]][is.na(CountAE.wide[[Treats[2]]])] <- 0

  n1 <- "tot.treat1"
  n2 <- "tot.treat2"
  ae1 <- "cum.treat1"
  ae2 <- "cum.treat2"

  test <- function(m) prop.test(c(m[ae1], m[ae2]),c(m[n1], m[n2]))$p.value
  f.test <- function(m) fisher.test(matrix(c(m[ae1], m[ae2], m[n1]-m[ae1], m[n2]-m[ae2]),2))$p.value
  r.diff <- function(m) (m[ae2]/m[n2] - m[ae1]/m[n1])
  r.ratio <- function(m) ((m[ae2]/m[n2]) / (m[ae1]/m[n1]))
  o.ratio <- function(m) ((m[ae2]/(m[n2]-m[ae2])) / (m[ae1]/(m[n1]-m[ae1])))

  for(i in unique(CountAE.wide$Terms)) {
    idx <- CountAE.wide$Terms==i

    CountAE.wide[idx, ]$cum.treat1 <- cumsum(CountAE.wide[CountAE.wide$Terms==i,][[Treats[1]]])
    CountAE.wide[idx, ]$cum.treat2 <- cumsum(CountAE.wide[CountAE.wide$Terms==i,][[Treats[2]]])
    CountAE.wide[idx, ]$tot.treat1 <- n.treat1
    CountAE.wide[idx, ]$tot.treat2 <- n.treat2

    cols <- c("cum.treat1", "cum.treat2", "tot.treat1", "tot.treat2")
    m <- as.matrix(CountAE.wide[idx,cols])
    if (suppress_warnings){
      CountAE.wide[idx, ]$p <- suppressWarnings(apply(m, 1, test))
    } else {
      CountAE.wide[idx, ]$p <- apply(m, 1, test)
    }
    CountAE.wide[idx, ]$p.adj <- p.adjust(CountAE.wide[CountAE.wide$Terms==i,]$p, "fdr")
    CountAE.wide[idx,]$fish <- apply(m, 1, f.test)
    CountAE.wide[idx,]$rdiff <- apply(m, 1, r.diff)
    CountAE.wide[idx,]$RR <- apply(m, 1, r.ratio)
    CountAE.wide[idx,]$OR <- apply(m, 1, o.ratio)


  }
  
  CountAE.wide$Terms <- as.character(CountAE.wide$Terms)
  dataset$data <- left_join(dataset$data, CountAE.wide, by = c("Terms" = "Terms", "StartDay" = "AEstartDay"))

  dataset$data$FDR.tot <- p.adjust(dataset$data$p, "fdr")

  dataset$n.tot <- data.frame(n.treat1 = n.treat1,
                              n.treat2 = n.treat2
                             )


  return(dataset)
}
