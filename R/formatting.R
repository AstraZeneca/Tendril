dataSetup <- function(data, rotations, Unique.Subject.Identifier, Terms, Treat, StartDay){
  data <- dplyr::rename_(data, "Unique.Subject.Identifier" = Unique.Subject.Identifier)
  data <- dplyr::rename_(data, "Terms" = Terms)
  data <- dplyr::rename_(data, "Treat" = Treat)
  data <- dplyr::rename_(data, "StartDay" = StartDay)
  data <- plyr::mutate(data, rot.factor = rotations)
  return(data)
}


cxDataFormat <- function(dataset){
  dataset$x <- Re(dataset$cx)
  dataset$y <- Im(dataset$cx)
  dataset$ang <- Arg(dataset$cx)
  dataset$mod <- Mod(dataset$cx)
  return(dataset)
}

createDataset <- function(coordx, coordy, label, type, StartDay, x){
  data <- data.frame(x=coordx,
                     y=coordy,
                     label=label,
                     type=type,
                     StartDay=StartDay,
                     perm.from.day=unique(x$perm.from.day),
                     Terms = unique(x$Terms))
  return(data)
}

joinData <- function(dataset1, dataset2, Tag){
  #make merging columns characters to avoid warning of merging columns
  #with different levels
  `%>%` <- magrittr::`%>%`

  dataset1 <- dataset1 %>% dplyr::mutate_if(is.factor, as.character)
  dataset2 <- dataset2 %>% dplyr::mutate_if(is.factor, as.character)
  dataset1$col.id<-as.numeric(dataset1$col.id)
  dataset2$col.id<-as.numeric(dataset2$col.id)
  joined <- dplyr::left_join(dataset1, dataset2, by = c("col.id" = "col.id"))
  joined$Tag <- Tag
  if (length(which(is.na(colnames(joined))))>0){
    joined <- joined[,-c(which(is.na(colnames(joined))))]
  }

  return(joined)
}
