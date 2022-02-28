# jaspTools::testAll()

context("Classical GLM Unit Testing")
#if you don't specify this, you get error saying "Error in (function (x) : attempt to apply non-function"
#see https://stackoverflow.com/questions/50083521/error-in-xmethod-attempt-to-apply-non-function-in-testthat-test-when

#### Test the binomial distribution with different links ####
# Specifically, the model coefficients, based on the GLM book (Dunn and Smyth, 2018) (page 337)
test_that("Binomial regression results match", {
  options <- jaspTools::analysisOptions("glmClassical")
  options$covariates <- c("Hours")
  options$weights    <- "Turbines"
  options$dependent  <- "DV"
  options$modelTerms <- list(
    list(components="Hours", isNuisance=FALSE)
  )
  options$family     <- "binomial"
  options$coefCI <- TRUE
  options$coefCIInterval <- 0.95

  #test logit link
  options$link       <- "logit"
  results <- jaspTools::runAnalysis("glmClassical", "turbines.csv", options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("(Intercept)", -3.924, 0.378, -10.381, .000, -4.704, -3.219,
                                             "Hours",       0.001,  0.000, 8.754,   .0001, 0.0008, 0.0012))

  #test probit
  options$link       <- "probit"
  results <- jaspTools::runAnalysis("glmClassical", "turbines.csv", options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("(Intercept)", -2.276, 0.1974, -11.528, .000, -2.676, -1.901,
                                             "Hours",       0.0006, 0.000,  9.239,   .0001,0.0005,  0.0007))

  #test cloglog
  options$link       <- "cloglog"
  results <- jaspTools::runAnalysis("glmClassical", "turbines.csv", options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("(Intercept)", -3.603, 0.3247, -11.096,  .000, -4.268, -3.003,
                                             "Hours",       0.0008, 0.0001,  8.992,   .000, 0.0006,  0.001))
})


#### test other aspects ####
options <- jaspTools::analysisOptions("glmClassical")
options$covariates <- c("Hours")
options$weights    <- "Turbines"
options$dependent  <- "DV"
options$modelTerms <- list(
  list(components="Hours", isNuisance=FALSE)
)
options$family     <- "binomial"
options$link       <- "logit"

options$gofDeviance <- TRUE
options$gofPearson  <- TRUE

options$plotDevResVsY  <- TRUE
options$plotPrsResVsY  <- TRUE
options$plotQuanResVsY <- TRUE

options$plotDevResVsX  <- TRUE
options$plotPrsResVsX  <- TRUE
options$plotQuanResVsX <- TRUE

options$plotDevResQQ   <- TRUE
options$plotPrsResQQ   <- TRUE
options$plotQuanResQQ  <- TRUE

options$plotPartial    <- TRUE

options$plotZVsEta     <- TRUE

options$tabOutlierQuan     <- TRUE
options$tabOutlierQuanTopN <- 3

options$tabOutlierStd      <- TRUE
options$tabOutlierStdTopN  <- 3

options$tabOutlierStu      <- TRUE
options$tabOutlierStuTopN  <- 3

options$DFBETAS  <- TRUE
options$DFFITS   <- TRUE
options$covRatio <- TRUE
options$cooksD   <- TRUE
options$leverage <- TRUE

results <- jaspTools::runAnalysis("glmClassical", "turbines.csv", options)

# model summary table
test_that("Model summary table results match", {
  table <- results[["results"]][["modelSummary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("H\u2080", 112.67, 150.147, 150.545,  10, "", "",
                                      "H\u2081", 10.33,  49.808,  50.604,   9,  102.339, 0.000))})



# model fit table
test_that("Model fit table results match", {
  table <- results[["results"]][["modelFitTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("Deviance", 10.331, 9, 0.3243,
                                      "Pearson",  9.251,  9, 0.4144))})

# residual plots
test_that("The deviance residual vs. fitted plot matches", {
  plotName <- results[["results"]][["glmPlotResVsFitted"]][["collection"]][["glmPlotResVsFitted_plotDevResVsY"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "plotDevResVsY")
  })

test_that("The Pearson residual vs. fitted plot matches", {
  plotName <- results[["results"]][["glmPlotResVsFitted"]][["collection"]][["glmPlotResVsFitted_plotPrsResVsY"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "plotPrsResVsY")
})

test_that("The quantile residual vs. fitted plot matches", {
  plotName <- results[["results"]][["glmPlotResVsFitted"]][["collection"]][["glmPlotResVsFitted_plotQuanResVsY"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "plotQuanResVsY")
})

test_that("The deviance residual vs. predictor plot matches", {
  plotName <- results[["results"]][["plotDevResVsX"]][["collection"]][["plotDevResVsX_Hours"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "plotDevResVsX")
})

test_that("The Pearson residual vs. predictor plot matches", {
  plotName <- results[["results"]][["plotPrsResVsX"]][["collection"]][["plotPrsResVsX_Hours"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "plotPrsResVsX")
})

test_that("The quantile residual vs. predictor plot matches", {
  plotName <- results[["results"]][["plotQuanResVsX"]][["collection"]][["plotQuanResVsX_Hours"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "plotQuanResVsX")
})

test_that("The deviance residual Q-Q plot matches", {
  plotName <- results[["results"]][["glmPlotResQQ"]][["collection"]][["glmPlotResQQ_plotDevResQQ"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "plotDevResQQ")
})

test_that("The Pearson residual Q-Q plot matches", {
  plotName <- results[["results"]][["glmPlotResQQ"]][["collection"]][["glmPlotResQQ_plotPrsResQQ"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "plotPrsResQQ")
})

test_that("The quantile residual Q-Q plot matches", {
  plotName <- results[["results"]][["glmPlotResQQ"]][["collection"]][["glmPlotResQQ_plotQuanResQQ"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "plotQuanResQQ")
})

test_that("The partial residual plot matches", {
  plotName <- results[["results"]][["plotPartial"]][["collection"]][["plotPartial_Hours"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "plotPartial")
})

test_that("The working response vs eta plot matches", {
  plotName <- results[["results"]][["glmPlotZVsEta"]][["collection"]][["glmPlotZVsEta_glmPlotZVsEta"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "glmPlotZVsEta")
})

# outliers table
test_that("Outlier table based on quantile residuals matches", {
  table <- results[["results"]][["outlierTables"]][["collection"]][["outlierTables_tabOutlierQuan"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 2.079,
                                      1, -1.325,
                                      2, 0.9768))})

test_that("Outlier table based on standardized deviance residuals matches", {
  table <- results[["results"]][["outlierTables"]][["collection"]][["outlierTables_tabOutlierStd"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9,  2.327,
                                      1,  -1.607,
                                      11, -1.233))})

test_that("Outlier table based on studentized deviance residuals matches", {
  table <- results[["results"]][["outlierTables"]][["collection"]][["outlierTables_tabOutlierStu"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9,  2.326,
                                      1,  -1.558,
                                      11, -1.240))})

# influential cases table
test_that("Influential cases table matches", {
  table <- results[["results"]][["influenceTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4,  -0.1795, 0.146,  -0.1918, 1.693,  0.0228, 0.2708,
                                      9,  -0.290,  0.750,  1.439,    0.3537, 0.6321, 0.1902,
                                      10, 0.1632,  -0.2761,-0.3971, 1.687, 0.0982,  0.3111))})


# multicollinearity table
test_that("Multicollinearity table matches", {
  options <- analysisOptions("glmClassical")

  options$family <- "bernoulli"
  options$link   <- "logit"
  options$covariates <- c("contNormal", "contOutlier")
  options$factors <- c("facFive")
  options$dependent <- "facGender"

  options$modelTerms <- list(
    list(components="contNormal",    isNuisance=FALSE),
    list(components="contOutlier",   isNuisance=FALSE),
    list(components="facFive", isNuisance=FALSE)
  )

  options$tolerance <- TRUE
  options$VIF       <- TRUE
  results <- jaspTools::runAnalysis("glmClassical", "debug.csv", options)

  table <- results[["results"]][["multicolliTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("contNormal",  0.9537, 1.049,
                                      "contOutlier", 0.9743, 1.026,
                                      "facFive",     0.9377, 1.066))})

# estimated marginal means table and contrast table
test_that("Estimated marginal means table matches", {
  options <- analysisOptions("glmClassical")

  options$family <- "bernoulli"
  options$link   <- "logit"
  options$covariates <- c("contNormal", "contOutlier")
  options$factors <- c("facFive")
  options$dependent <- "facGender"

  options$modelTerms <- list(
    list(components="contNormal",    isNuisance=FALSE),
    list(components="contOutlier",   isNuisance=FALSE),
    list(components="facFive", isNuisance=FALSE)
  )

  options$marginalMeansVars <- c("contNormal")
  options$marginalMeansCompare <- TRUE
  options$marginalMeansCompareTo <- 0

  options$marginalMeansContrast <- TRUE
  options$Contrasts <- list(list(isContrast = FALSE, levels = c("1", "2", "3"), name = "contNormal", values = c("-1", "0", "1")),
                            list(isContrast = TRUE,  levels = c("1", "2", "3"), name = "Contrast 1", values = c("1", "0", "-1")))

  results <- jaspTools::runAnalysis("glmClassical", "debug.csv", options)

  EMMtable <- results[["results"]][["EMMsummary"]][["data"]]
  jaspTools::expect_equal_tables(EMMtable,
                                 list(-1.247,  0.3772, 0.0723, 0.2489, 0.5254, -1.630, 0.1031, 1,
                                      -0.1887, 0.5023, 0.052,  0.4017, 0.6026, 0.0436, 0.9652, 2,
                                      0.8697,  0.6271, 0.0752, 0.4723, 0.7596, 1.615,  0.1063, 3))

  contrastsTable <- results[["results"]][["contrastsTable"]][["data"]]
  jaspTools::expect_equal_tables(contrastsTable,
                                 list("Contrast 1",  -0.2499, 0.1108, Inf, -2.255, 0.0242))

  })

# test error handling
test_that("Input error handling", {

  # check for bernoulli if dv is factor with two levels
  options <- analysisOptions("glmClassical")
  options$family <- "bernoulli"
  options$link   <- "logit"
  options$covariates <- c("contNormal", "contOutlier")
  options$dependent <- "facFive"
  options$modelTerms <- list(
    list(components="contNormal",    isNuisance=FALSE),
    list(components="contOutlier",   isNuisance=FALSE)
  )
  results <- jaspTools::runAnalysis("glmClassical", "debug.csv", options)
  expect_identical(results$status, "validationError", "Bernoulli dv factor check")

  # check if weights are non-negative
  options <- analysisOptions("glmClassical")
  options$family  <- "bernoulli"
  options$link    <- "logit"
  options$weights <- "contcor1"
  options$covariates <- c("contNormal", "contOutlier")
  options$dependent <- "facGender"
  options$modelTerms <- list(
    list(components="contNormal",    isNuisance=FALSE),
    list(components="contOutlier",   isNuisance=FALSE)
  )
  results <- jaspTools::runAnalysis("glmClassical", "debug.csv", options)
  expect_identical(results$status, "validationError", "Weights non-negative check")
})


# jaspTools::testAll()
