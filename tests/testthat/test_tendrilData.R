context("tendril input and output check")

test_that("input_dataset_is_dataframe",{

  #get data from example
  data("TendrilData")
  data <- TendrilData
  data("SubjList")
  SubjList <- SubjList

  #data is not dataframe
  expect_error(
    Tendril(mydata = list(1,2),
            rotations = Rotations,
            AEfreqThreshold = 9,
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
  )
})

test_that("input_frequency_and_rot_are positive_numbers",{

  #get data from example
  data("TendrilData")
  data <- TendrilData
  data("SubjList")
  SubjList <- SubjList

  #negative frequency
  expect_error(
    validate.tendril.data(mydata = TendrilData,
                          rotations = Rotations,
            AEfreqThreshold = -9,
            Tag = "Comment",
            Treatments = c("placebo", "active"),
            Unique.Subject.Identifier = "subjid",
            Terms = "ae",
            Treat = "treatment",
            StartDay = "day")
    )

  #frequency not a number
  expect_error(
    validate.tendril.data(mydata = TendrilData,
                          rotations = Rotations,
            AEfreqThreshold = c(1,2),
            Tag = "Comment",
            Treatments = c("placebo", "active"),
            Unique.Subject.Identifier = "subjid",
            Terms = "ae",
            Treat = "treatment",
            StartDay = "day")
  )

  #negative rot.factor
  expect_error(
    validate.tendril.data(mydata = TendrilData,
                          rotations = Rotations,
            AEfreqThreshold = 9,
            Tag = "Comment",
            Treatments = c("placebo", "active"),
            Unique.Subject.Identifier = "subjid",
            Terms = "ae",
            Treat = "treatment",
            StartDay = "day")
  )

  #rotations not a valid vector
  expect_error(
    validate.tendril.data(mydata = lTendrilData,
                          rotations = c(1,2),
                          AEfreqThreshold = 9,
            Tag = "Comment",
            Treatments = c("placebo", "active"),
            Unique.Subject.Identifier = "subjid",
            Terms = "ae",
            Treat = "treatment",
            StartDay = "day")
  )

})

test_that("columns_exists",{

  #get data from example
  data("TendrilData")
  data <- TendrilData
  data("SubjList")
  SubjList <- SubjList

  #SartDay does not exist
  expect_error(
    validate.tendril.data(mydata = TendrilData,
                          rotations = Rotations,
            AEfreqThreshold = 9,
            Tag = "Comment",
            Treatments = c("placebo", "active"),
            Unique.Subject.Identifier = "subjid",
            Terms = "ae",
            Treat = "treatment",
            StartDay = "wrong")
  )

  #Unique.Subject.Identifier does not exist
  expect_error(
    validate.tendril.data(mydata = TendrilData,
                          rotations = Rotations,
                          AEfreqThreshold = 9,
            Tag = "Comment",
            Treatments = c("placebo", "active"),
            Unique.Subject.Identifier = "wrong",
            Terms = "ae",
            Treat = "treatment",
            StartDay = "day")
  )

  #Terms does not exist
  expect_error(
    validate.tendril.data(mydata = TendrilData,
                          rotations = Rotations,
            AEfreqThreshold = 9,
            Tag = "Comment",
            Treatments = c("placebo", "active"),
            Unique.Subject.Identifier = "subjid",
            Terms = "wrong",
            Treat = "treatment",
            StartDay = "day")
  )

  #Treat does not exist
  expect_error(
    validate.tendril.data(mydata = TendrilData,
                          rotations = Rotations,
            AEfreqThreshold = 9,
            Tag = "Comment",
            Treatments = c("placebo", "active"),
            Unique.Subject.Identifier = "subjid",
            Terms = "ae",
            Treat = "wrong",
            StartDay = "day")
  )

})

test_that("treatments exists",{

  #get data from example
  data("TendrilData")
  data <- TendrilData
  data("SubjList")
  list <- SubjList

  #the treatments does not exist
  expect_error(
    validate.tendril.data(mydata = TendrilData,
                          rotations = Rotations,
                          AEfreqThreshold = 9,
            Tag = "Comment",
            Treatments = c("wrong", "active"),
            Unique.Subject.Identifier = "subjid",
            Terms = "ae",
            Treat = "treatment",
            StartDay = "day")
  )

})

test_that("filtering_keeps_some_data",{

  #get data from example
  data("TendrilData")
  data <- TendrilData

  #pass an empty dataframe
  expect_error(
    validate.tendril.results(data[NULL, ])
  )

})

test_that("tendril_result_correct",{

  #get data from example
  data("TendrilData")
  data <- TendrilData
  data("SubjList")
  SubjList <- SubjList
  data("Tendril.res")
  Tendril.res <- Tendril.res

  #this calculation is wrapped inside expect_warning because the dataset generates
  # "Chi-squared approximation may be incorrect" warnings
  expect_warning(
    res <- Tendril(mydata = TendrilData,
                   rotations = Rotations,
                 AEfreqThreshold = 9,
                 Tag = "Comment",
                 Treatments = c("placebo", "active"),
                 Unique.Subject.Identifier = "subjid",
                 Terms = "ae",
                 Treat = "treatment",
                 StartDay = "day",
                 SubjList = SubjList,
                 SubjList.subject = "subjid",
                 SubjList.treatment = "treatment")
  )

  #class must be tendril
  expect_equal(
    "Tendril", class(res)
  )

  #list must contain 15 items
  expect_equal(
    15, length(res)
    )

  #value of list item: test$Treat
  expect_equal(
    "treatment", res$Treat
  )

  #value of list item: test$StartDay
  expect_equal(
    "day", res$StartDay
  )

  #value of list item: test$Terms
  expect_equal(
    "ae", res$Terms
  )

  #value of list item: test$Unique.Subject.Identifier
  expect_equal(
    "subjid", res$Unique.Subject.Identifier
  )

  #value of list item: test$AEfreqThreshold
  expect_equal(
    9, res$AEfreqThreshold
  )

  #value of list item: test$Tag
  expect_equal(
    "Comment", res$Tag
  )

  #class of list item: test$SubjList
  expect_equal(
    "data.frame", class(res$SubjList)
  )

  #value of list item: test$SubjList.subject
  expect_equal(
    "subjid", res$SubjList.subject
  )

  #value of list item: test$SubjList.treatment
  expect_equal(
    "treatment", res$SubjList.treatment
  )

  #check final result is correct
  expect_equal(
    res, Tendril.res
  )
})

test_that("tendril_result_correct_filter_double_events",{

  #get data from example
  data("TendrilData")
  data <- TendrilData
  data("SubjList")
  SubjList <- SubjList
  load(file = "../Tendril.res.single.events.rda")
  Tendril.res.single.events <- Tendril.res.single.events

  expect_warning(
    res <- Tendril(mydata = TendrilData,
                   rotations = Rotations,
                   AEfreqThreshold = 9,
                   Tag = "Comment",
                   Treatments = c("placebo", "active"),
                   Unique.Subject.Identifier = "subjid",
                   Terms = "ae",
                   Treat = "treatment",
                   StartDay = "day",
                   SubjList = SubjList,
                   SubjList.subject = "subjid",
                   SubjList.treatment = "treatment",
                   filter_double_events = TRUE,
                   suppress_warnings = TRUE)
  )

  #check final result is correct
  expect_equal(
    res, Tendril.res.single.events
  )
})

test_that("tendril_result_no_SubjList_supplied",{

  #get data from example
  data("TendrilData")
  data <- TendrilData
  data("SubjList")
  SubjList <- SubjList
  data("Tendril.res")
  Tendril.res <- Tendril.res

  #check that a warning is given on the intepretation when no SubjList is supplied
  expect_warning(res <- Tendril(mydata = TendrilData,
                 rotations = Rotations,
                 AEfreqThreshold = 9,
                 Tag = "Comment",
                 Treatments = c("placebo", "active"),
                 Unique.Subject.Identifier = "subjid",
                 Terms = "ae",
                 Treat = "treatment",
                 StartDay = "day",
                 suppress_warnings = TRUE))

  #check final result is correct
  expect_equal(
    list(res$data$x, res$data$y), list(Tendril.res$data$x, Tendril.res$data$y)
  )
})

test_that("tendril_result_no_SubjList_supplied_single_rotation_factor",{

  #get data from example
  data("TendrilData")
  data <- TendrilData
  data("SubjList")
  SubjList <- SubjList
  data("Tendril.res")
  Tendril.res <- Tendril.res

  #check that a warning is given on the intepretation when no SubjList is supplied
  expect_warning(res <- Tendril(mydata = TendrilData,
                                rotations = 3,
                                AEfreqThreshold = 9,
                                Tag = "Comment",
                                Treatments = c("placebo", "active"),
                                Unique.Subject.Identifier = "subjid",
                                Terms = "ae",
                                Treat = "treatment",
                                StartDay = "day",
                                SubjList.subject = "subjid",
                                SubjList.treatment = "treatment",
                                suppress_warnings = TRUE))

  #check final result is correct
  expect_equal(
    list(res$data$x, res$data$y), list(Tendril.res$data$x, Tendril.res$data$y)
  )
})

test_that("tendril_proportional_rotation_factor",{

  #get data from example
  load(file = "../Tendril.data.with.dropouts.rda")
  data <- Tendril.data.with.dropouts
  load(file = "../SubjList.with.dropouts.rda")
  SubjList.with.dropouts <- SubjList.with.dropouts
  load(file = "../Tendril.res.proportional.rotation.factor.rda")
  Tendril.res.proportional.rotation.factor <- Tendril.res.proportional.rotation.factor

  expect_warning(
    res <- Tendril(mydata = data,
                    rotations = 3,
                    AEfreqThreshold = 9,
                    Tag = "Comment",
                    Treatments = c("placebo", "active"),
                    Unique.Subject.Identifier = "subjid",
                    Terms = "ae",
                    Treat = "treatment",
                    StartDay = "day",
                    SubjList = SubjList.with.dropouts,
                    SubjList.subject = "subjid",
                    SubjList.treatment = "treatment",
                    SubjList.dropout = "dropoutday",
                    suppress_warnings = TRUE)
  )

  #check final result is correct
  expect_equal(
    res, Tendril.res.proportional.rotation.factor
  )
})

test_that("tendril_error_event_after_dropout",{

  #get data from example
  data("TendrilData")
  data <- TendrilData
  load(file = "../SubjList.with.dropouts.rda")
  SubjList.with.dropouts <- SubjList.with.dropouts
  load(file = "../Tendril.res.proportional.rotation.factor.rda")
  Tendril.res.proportional.rotation.factor <- Tendril.res.proportional.rotation.factor

  #check whether an error is given if an event is specified for a subject after it's dropoutdate
  expect_warning(res <- Tendril(mydata = TendrilData,
                                rotations = 3,
                                AEfreqThreshold = 9,
                                Tag = "Comment",
                                Treatments = c("placebo", "active"),
                                Unique.Subject.Identifier = "subjid",
                                Terms = "ae",
                                Treat = "treatment",
                                StartDay = "day",
                                SubjList = SubjList.with.dropouts,
                                SubjList.subject = "subjid",
                                SubjList.treatment = "treatment",
                                SubjList.dropout = "dropoutday",
                                suppress_warnings = TRUE))

  #check final result is correct
  expect_equal(
    res, Tendril.res.proportional.rotation.factor
  )
})

test_that("tendril_check_correct_imbalance_and_variable_rotation",{

  #get data from example
  load(file = "../Tendril.data.with.dropouts.rda")
  data <- Tendril.data.with.dropouts
  load(file = "../SubjList.with.dropouts.rda")
  SubjList.with.dropouts <- SubjList.with.dropouts
  load(file = "../Tendril.res.proportional.correct.imbalance.rda")
  Tendril.res.proportional.correct.imbalance <- Tendril.res.proportional.correct.imbalance
  load(file = "../Variable.rotation.rda")
  Variable.rotation <- Variable.rotation

  expect_warning(
    res <- Tendril(mydata = Tendril.data.with.dropouts,
                                  rotations = Variable.rotation,
                                  AEfreqThreshold = 9,
                                  Tag = "Comment",
                                  Treatments = c("placebo", "active"),
                                  Unique.Subject.Identifier = "subjid",
                                  Terms = "ae",
                                  Treat = "treatment",
                                  StartDay = "day",
                                  SubjList = SubjList.with.dropouts,
                                  SubjList.subject = "subjid",
                                  SubjList.treatment = "treatment",
                                  SubjList.dropout = "dropoutday",
                                  suppress_warnings = TRUE,
                                  compensate_imbalance = TRUE)
  )

  #check final result is correct
  expect_equal(
    res, Tendril.res.proportional.correct.imbalance
  )
})
