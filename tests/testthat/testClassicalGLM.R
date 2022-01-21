# test results against the GLM book (Dunn and Smyth, 2018)

# test the binomial distribution (page  337)
test_that("Binomial regression results match", {
  options <- jaspTools::analysisOptions("glmClassical")
  options$covariates <- c("Hours")
  options$weights    <- "Turbines"
  options$dependent  <- "DV"
  options$modelTerms <- list(
    list(components="Hours", isNuisance=FALSE)
  )
  options$family     <- "binomial"

  #test logit link
  options$link       <- "logit"
  results <- jaspTools::runAnalysis("glmClassical", "turbines.csv", options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("(Intercept)", -3.924, 0.378, -10.381, .000,
                                             "Hours",       0.001,  0.000, 8.754,   .0001))

  #test probit
  options$link       <- "probit"
  results <- jaspTools::runAnalysis("glmClassical", "turbines.csv", options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("(Intercept)", -2.276, 0.1974, -11.528, .000,
                                             "Hours",       0.0006, 0.000,  9.239,   .0001))

  #test cloglog
  options$link       <- "cloglog"
  results <- jaspTools::runAnalysis("glmClassical", "turbines.csv", options)
  table <- results[["results"]][["estimatesTable"]][["data"]]
  jaspTools::expect_equal_tables(table, list("(Intercept)", -3.603, 0.3247, -11.096,  .000,
                                             "Hours",       0.0008, 0.0001,  8.992,   .000))
})


# test the gaussian distribution (page  )

# test the gamma distribution (page)

# test the inverse gaussian distribution (page)

# test the poisson distribution (page)



# test model summary table

# test model fit table

# test coefficients table

# test error handling


