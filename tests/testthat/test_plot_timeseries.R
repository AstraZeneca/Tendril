context("plot_timeseries input and output check")

test_that("Creation of plot timeseries",{

  data("TendrilData")

  suppressWarnings({
    mydata = Tendril(
      mydata = TendrilData,
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

    p <- plot_timeseries(mydata, term="AE33")
    expect_s3_class(p, "gg")
  })
})
