context("TendrilPerm input check")

test_that("input_perm_valid",{

  #get data from example
  data("TendrilData")
  data <- TendrilData
  data("SubjList")
  SubjList <- SubjList

  #this calculation is wrapped inside expect_warning because the dataset generates
  # "Chi-squared approximation may be incorrect" warnings
  expect_warning({
    tendril.data <- Tendril(mydata = TendrilData,
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
  })

  #wrong dataset
  expect_error(
    TendrilPerm(tendril = list(1,2),
                PermTerm="V1",
                n.perm = 500,
                perm.from.day = 1,
                pi.low = 0.1,
                pi.high = 0.9)
  )


  #PerTerm not available
  expect_error(
    TendrilPerm(tendril = tendril.data,
                PermTerm="wrong",
                n.perm = 500,
                perm.from.day = 1,
                pi.low = 0.1,
                pi.high = 0.9)
  )

  #n.perm not a positive integer
  expect_error(
    TendrilPerm(tendril = tendril.data,
                PermTerm="V1",
                n.perm = -500,
                perm.from.day = 1,
                pi.low = 0.1,
                 pi.high = 0.9)
  )

  #perm.from.day not a positive integer
  expect_error(
    TendrilPerm(tendril = tendril.data,
                PermTerm="V1",
                n.perm = 500,
                perm.from.day = -1,
                pi.low = 0.1,
                pi.high = 0.9)
  )

  #pi not a numeric
  expect_error(
    TendrilPerm(dataset = tendril.data,
                PermTerm="V1",
                n.perm = 500,
                perm.from.day = -1,
                pi.low = "a",
                pi.high = 0.9)
  )

  #pi.low>pi.high
  expect_error(
    TendrilPerm(dataset = tendril.data,
                PermTerm="V1",
                n.perm = 500,
                perm.from.day = -1,
                pi.low = 0.9,
                pi.high = 0.1)
  )

  #pi <0
  expect_error(
    TendrilPerm(dataset = tendril.data,
                PermTerm="V1",
                n.perm = 500,
                perm.from.day = -1,
                pi.low =  -1,
                pi.high = 0.9)
  )

  #pi >1
  expect_error(
    TendrilPerm(dataset = tendril.data,
                PermTerm="V1",
                n.perm = 500,
                perm.from.day = -1,
                pi.low = 0.1,
                pi.high = 2)
  )

})

test_that("output_perm_valid",{

  #set seed to make results reproducible
  set.seed(1)

  #get dataset from example
  data("TendrilData")
  data <- TendrilData
  data("SubjList")
  SubjList <- SubjList
  load(file = "../Output.perm.constant.rda")

  #this calculation is wrapped inside expect_warning because the dataset generates
  # "Chi-squared approximation may be incorrect" warnings
  expect_warning({
    tendril.data <- Tendril(mydata = TendrilData,
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
  })

  res<-TendrilPerm(tendril = tendril.data,
                  PermTerm="AE44",
                  n.perm = 50,
                  perm.from.day = 1)

  #must be of class TendrilPerm
  expect_equal("TendrilPerm", class(res))

  expect_named(res, c('tendril', 'Permterm', 'perm.data', 'tendril.pi'))

})

test_that("output_perm_proportional_valid",{

  #set seed to make results reproducible
  set.seed(1)

  #get dataset from example
  load(file = "../Tendril.res.proportional.rotation.factor.rda")
  data <- Tendril.res.proportional.rotation.factor

  load(file = "../Output.perm.proportional.rda")

  res<-TendrilPerm(tendril = data,
                   PermTerm="AE44",
                   n.perm = 50,
                   perm.from.day = 1)
  expect_equal("TendrilPerm", class(res))
  expect_named(res, c('tendril', 'Permterm', 'perm.data', 'tendril.pi'))
})
