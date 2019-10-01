context("tendril.cx input and output check")

test_that("input_data_cx_valid",{

  data("TendrilData")
  data <- TendrilData

  #wrong size of treatment
  expect_error(
   validate.tendril.cx(treatment = data$treatment[1],
                                     evt.day = data$day,
                                     ae.term = data$ae[1],
                                     subj.id = data$subjid,
                                     rot.factor = 1)
    )

  #wrong size of day
  expect_error(
    validate.tendril.cx(treatment = data$treatment,
                           evt.day = data$day[1],
                           ae.term = data$ae[1],
                           subj.id = data$subjid,
                           rot.factor = 1)
  )

  #wrong size of ae
  expect_error(
    validate.tendril.cx(treatment = data$treatment,
                             evt.day = data$day,
                             ae.term = data$ae[c(1,2)],
                             subj.id = data$subjid,
                             rot.factor = 1)
  )

  #wrong size of subjid
  expect_error(
    validate.tendril.cx(treatment = data$treatment,
                             evt.day = data$day,
                             ae.term = data$ae[1],
                             subj.id = data$subjid[1],
                             rot.factor = 1)
  )

  #wrong value for rot.factor
  expect_error(
    validate.tendril.cx(treatment = data$treatment,
                             evt.day = data$day,
                             ae.term = data$ae[1],
                             subj.id = data$subjid,
                             rot.factor = -1)
  )

})
