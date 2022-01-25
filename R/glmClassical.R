#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#still to do: multinomial, ordinal, negative binomial, quasi

glmClassical <- function(jaspResults, dataset = NULL, options, ...) {
  if (options$family == "binomial") {
    ready <- (options$dependent != "" && options$weights != "")

  } else {
    ready <- options$dependent != ""
  }


  if (ready) {
    dataset <- .glmReadData(dataset, options)
    .glmCheckDataErrors(dataset, options)
  }

  #output tables
  .glmModelSummaryTable(jaspResults, dataset, options, ready)
  .glmModelFitTable(jaspResults, dataset, options, ready)
  .glmEstimatesTable(jaspResults, dataset, options, ready)

  #diagnostic tables and plots
  .glmPlotResVsFitted(jaspResults, dataset, options, ready, position = 4)
  .glmPlotResVsPredictor(jaspResults, dataset, options, ready, residType = "deviance", position = 5)
  .glmPlotResVsPredictor(jaspResults, dataset, options, ready, residType = "Pearson", position = 6)
  .glmPlotResVsPredictor(jaspResults, dataset, options, ready, residType = "quantile", position = 7)
  .glmPlotResQQ(jaspResults, dataset, options, ready, position = 8)

  .glmPlotResPartial(jaspResults, dataset, options, ready, position = 9)
  .glmPlotZVsEta(jaspResults, dataset, options, ready, position = 10)

  return()
}

# Function to read data
.glmReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  }
  else {
    numericVars  <- c(options$covariates, options$weights)
    numericVars  <- numericVars[numericVars != ""]
    factorVars   <- c(options$factors)
    factorVars   <- factorVars[factorVars != ""]
    dependentVar <- c(options$dependent)
    dependentVar <- dependentVar[dependentVar != ""]

    if (options$family == "bernoulli") {
      return(.readDataSetToEnd(columns.as.numeric  = numericVars,
                               columns.as.factor   = c(factorVars, dependentVar),
                               exclude.na.listwise = c(numericVars, factorVars, dependentVar)))
    }
    else {
      return(.readDataSetToEnd(columns.as.numeric  = c(numericVars, dependentVar),
                               columns.as.factor   = factorVars,
                               exclude.na.listwise = c(numericVars, factorVars, dependentVar)))
    }
  }
}

# Function to check errors when reading data
.glmCheckDataErrors <- function(dataset, options){

  if (nrow(dataset) < length(c(options$covariates, options$factors)))
    .quitAnalysis("The dataset contains fewer observations than predictors (after excluding NAs/NaN/Inf).")

  if (options$weights != "")
    .hasErrors(dataset,
               type = "limits",
               limits.target = options$weights,
               limits.min = 0,
               limits.max = Inf,
               exitAnalysisIfErrors = TRUE)

#  if (length(options$covariates) != 0)
#    .hasErrors(dataset,
#               type = c("observations", "infinity", "variance", "varCovData"),
#               all.target = options$covariates,
#               observations.amount  = "< 2",
#               exitAnalysisIfErrors = TRUE)

  # check family-specific dependent variable errors
  if (options$family == "bernoulli") {

    if (length(levels(dataset[, options$dependent])) != 2)
      .quitAnalysis(gettextf("The %s family requires the dependent variable to be a factor with 2 levels.", options$family))

  } else if (options$family == "binomial") {

    if (any(dataset[, options$dependent] < 0) || any(dataset[, options$dependent] > 1))
      .quitAnalysis(gettextf("The %s family requires the dependent variable (i.e. proportion of successes) to be between 0 and 1 (inclusive).", options$family))

    if (any(dataset[, options$weights] < 0) || any(!.is.wholenumber(dataset[, options$weights])))
      .quitAnalysis(gettextf("The %s family requires the weights variable (i.e. total number of trials) to be an integer.", options$family))

  } else if (options$family %in% c("Gamma", "inverse.gaussian")) {

    if (any(dataset[, options$dependent] <= 0))
      .quitAnalysis(gettextf("The %s family requires the dependent variable to be positive.", options$family))

  } else if (options$family == "poisson") {

    if (any(dataset[, options$dependent] < 0 | any(!.is.wholenumber(dataset[, options$dependent]))))
      .quitAnalysis(gettextf("The %s family requires the dependent variable to be an integer.", options$family))

  } else if (options$family == "gaussian") {

    if (!is.numeric(dataset[, options$dependent]))
      .quitAnalysis(gettextf("The %s family requires the dependent variable tp be a numerical variable.", options$family))

  }

}

# Model Summary Table
.glmModelSummaryTable <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["modelSummary"]])) {
    return()
  }

  if (!ready) {
    modelSummary <- createJaspTable(gettext("Model Summary"))
  }
  else {
    modelSummary <- createJaspTable(gettextf("Model Summary - %s", options[['dependent']]))
  }

  dependList <- c("dependent", "family", "link", "modelTerms", "includeIntercept", "weights")
  modelSummary$dependOn(dependList)
  modelSummary$position <- 1
  modelSummary$showSpecifiedColumnsOnly <- TRUE

  modelSummary$addColumnInfo(name = "mod", title = gettext("Model"),    type = "string")
  modelSummary$addColumnInfo(name = "dev", title = gettext("Deviance"), type = "number")
  modelSummary$addColumnInfo(name = "aic", title = gettext("AIC"),      type = "number", format="dp:3")
  modelSummary$addColumnInfo(name = "bic", title = gettext("BIC"),      type = "number", format="dp:3")
  modelSummary$addColumnInfo(name = "dof", title = gettext("df"),       type = "integer")
  modelSummary$addColumnInfo(name = "chi", title = "\u03A7\u00B2",      type = "number", format="dp:3")
  modelSummary$addColumnInfo(name = "pvl", title = gettext("p"),        type = "pvalue")

  jaspResults[["modelSummary"]] <- modelSummary
  .glmModelSummaryTableFill(jaspResults, dataset, options, ready)

  return()
}

.glmModelSummaryTableFill <- function(jaspResults, dataset, options, ready) {
  if (ready) {
    # compute glm models
    glmModels <- .glmComputeModel(jaspResults, dataset, options)
    hasNuisance <- .hasNuisance(options)
    if (hasNuisance) {
      terms <- rownames(summary(glmModels[["nullModel"]])[["coefficients"]])
      terms <- sapply(terms[terms!="(Intercept)"], .formatTerm,
                      glmModel=glmModels[["nullModel"]])
      message <- gettextf("Null model contains nuisance parameters: %s",
                          paste(terms, collapse = ", "))
      jaspResults[["modelSummary"]]$addFootnote(message)
    }
    #log-likelihood ratio test to compare nested models (null vs full)
    if (options$family %in% c("bernoulli", "binomial", "poisson")) {
      testType <- "Chisq"
      pvalName <- "Pr(>Chi)"
    }

    else {
      testType <- "F"
      pvalName <- "Pr(>F)"
    }

    anovaRes <- anova(glmModels[["nullModel"]], glmModels[["fullModel"]],
                      test = testType)

    rows <- list(
      list(mod = "H\u2080",
           dev = glmModels[["nullModel"]][["deviance"]],
           aic = glmModels[["nullModel"]][["aic"]],
           bic = BIC(glmModels[["nullModel"]]),
           dof = glmModels[["nullModel"]][["df.residual"]],
           chi = "",
           pvl = ""),
      list(mod = "H\u2081",
           dev = glmModels[["fullModel"]][["deviance"]],
           aic = glmModels[["fullModel"]][["aic"]],
           bic = BIC(glmModels[["fullModel"]]),
           dof = glmModels[["fullModel"]][["df.residual"]],
           chi = anovaRes$Deviance[[2]],
           pvl = anovaRes[[pvalName]][[2]])
    )
  } else {
    rows <- list(
      list(mod = "H\u2080",
           dev = ".",
           aic = ".",
           bic = ".",
           dof = ".",
           chi = ".",
           pvl = "."),
      list(mod = "H\u2081",
           dev = ".",
           aic = ".",
           bic = ".",
           dof = ".",
           chi = ".",
           pvl = ".")
    )
  }
  jaspResults[["modelSummary"]]$addRows(rows)
}

# Model fit table
.glmModelFitTable <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["modelFit"]]) || (!options$gofDeviance & !options$gofPearson)) {
    return()
  }

  modelFitTable <- createJaspTable(gettext("Model Fit"))


  modelFitTable$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                         options             = c("gofDeviance", "gofPearson"))

  modelFitTable$position <- 2
  modelFitTable$showSpecifiedColumnsOnly <- TRUE

  modelFitTable$addColumnInfo(name = "gofType",   title = "",                    type = "string")
  modelFitTable$addColumnInfo(name = "gof",       title = gettext("Statistic"),  type = "number", format="dp:3")
  modelFitTable$addColumnInfo(name = "dof",       title = gettext("df"),         type = "integer")
  modelFitTable$addColumnInfo(name = "pval",      title = gettext("p"),          type = "pvalue")

  jaspResults[["modelFitTable"]] <- modelFitTable
  .glmModelFitTableFill(jaspResults, dataset, options, ready)

  return()
}

.glmModelFitTableFill <- function(jaspResults, dataset, options, ready) {
  if (!ready)
    return()

  # compute glm models
  glmModels <- .glmComputeModel(jaspResults, dataset, options)
  modelObj  <- glmModels[["fullModel"]]

  if (options$gofDeviance) {
    jaspResults[["modelFitTable"]]$addRows(
      list(gofType = "Deviance",
           gof     = modelObj$deviance,
           dof     = modelObj$df.residual,
           pval    = pchisq(modelObj$deviance,
                            df=modelObj$df.residual,
                            lower.tail=FALSE))
    )
  }

  if (options$gofPearson) {
    pearson  <- sum(modelObj$weights * modelObj$residuals^2)
    jaspResults[["modelFitTable"]]$addRows(
      list(gofType = "Pearson",
           gof     = pearson,
           dof     = modelObj$df.residual,
           pval    = pchisq(pearson,
                            df=modelObj$df.residual,
                            lower.tail=FALSE))
    )
  }
}

# GLM estimates table
.glmEstimatesTable <- function(jaspResults, dataset, options, ready) {
  if (!options$coefEstimates || !is.null(jaspResults[["estimatesTable"]]))
    return()

  estimatesTable <- createJaspTable(gettext("Coefficients"))
  estimatesTable$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                          options             = c("coefEstimates", "coefCI", "coefCIInterval"))
  estimatesTable$position <- 3
  estimatesTable$showSpecifiedColumnsOnly <- TRUE

  if (options$family %in% c("bernoulli", "binomial", "poisson"))
    testStat <- "z"
  else
    testStat <- "t"

  estimatesTable$addColumnInfo(name = "param",    title = "", type = "string")
  estimatesTable$addColumnInfo(name = "est",      title = gettext("Estimate"), type = "number", format="dp:3")
  estimatesTable$addColumnInfo(name = "se",       title = gettext("Standard Error"), type = "number", format="dp:3")
  estimatesTable$addColumnInfo(name = "testStat", title = gettext(testStat), type = "number")
  estimatesTable$addColumnInfo(name = "pval",     title = gettext("p"), type = "pvalue")

  if (options$coefCI) {
    ciPercentage <- options$coefCIInterval * 100
    if (floor(ciPercentage) == ciPercentage)
      ciPercentage <- as.integer(ciPercentage)
    ciTitle <- paste(ciPercentage, " % ", "Confidence Interval",sep = "")
    estimatesTable$addColumnInfo(name = "ciLow",    title = gettext("Lower Bound"), type = "number", format = "dp:3", overtitle = ciTitle)
    estimatesTable$addColumnInfo(name = "ciUpp",    title = gettext("Upper Bound"), type = "number", format = "dp:3", overtitle = ciTitle)
  }

  jaspResults[["estimatesTable"]] <- estimatesTable
  .glmEstimatesTableFill(jaspResults, dataset, options, ready)
}

.glmEstimatesTableFill <- function(jaspResults, dataset, options, ready) {
  if (!ready)
    return()
  # compute glm models
  glmModels <- .glmComputeModel(jaspResults, dataset, options)
  modelSummary <- summary(glmModels[["fullModel"]])[["coefficients"]]
  rowNames <- rownames(modelSummary)

  if (options$coefCI) {
    coefCISummary <- confint(glmModels[["fullModel"]], level = options$coefCIInterval)
  } else {
    coefCISummary <- matrix(nrow = length(rowNames),
                            ncol = 2,
                            data = rep(0, length(rowNames)*2))
  }

  for (i in seq_along(rowNames)) {
    jaspResults[["estimatesTable"]]$addRows(
      list(param     = .formatTerm(rowNames[i], glmModels[["fullModel"]]),
           est       = modelSummary[i, 1],
           se        = modelSummary[i, 2],
           testStat  = modelSummary[i, 3],
           pval      = modelSummary[i, 4],
           ciLow     = coefCISummary[i, 1],
           ciUpp     = coefCISummary[i, 2])
    )
  }

  if (options$family == "bernoulli") {
    dv      <- as.character(glmModels[["nullModel"]][["terms"]])[2]
    dvLevel <- levels(glmModels[["nullModel"]][["data"]][[dv]])[2]

    jaspResults[["estimatesTable"]]$addFootnote(gettextf("%1$s level '%2$s' coded as class 1.", dv, dvLevel))
  }
}


# Plots: Residuals vs. fitted
.glmPlotResVsFitted <- function(jaspResults, dataset, options, ready, position = 4) {
  if (!ready)
    return()

  plotNames <- c("plotDevResVsY", "plotPrsResVsY", "plotQuanResVsY")
  residNames <- c("deviance", "Pearson", "quantile")

  glmPlotResVsFittedContainer <- createJaspContainer(gettext("Residuals vs. Fitted Plots"))
  glmPlotResVsFittedContainer$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                                       options           = plotNames)
  glmPlotResVsFittedContainer$position <- position
  jaspResults[["glmPlotResVsFitted"]] <- glmPlotResVsFittedContainer


  if (!is.null(jaspResults[["glmModels"]])) {
    glmFullModel <- jaspResults[["glmModels"]][["object"]][["fullModel"]]
    for (i in 1:length(plotNames)) {
      if (options[[plotNames[[i]]]]) {
        .glmCreatePlotPlaceholder(glmPlotResVsFittedContainer,
                                  index = plotNames[[i]],
                                  title = gettextf("Standardized %1s residuals vs. fitted values", residNames[[i]]))

        .glmInsertPlot(glmPlotResVsFittedContainer[[plotNames[[i]]]],
                       .glmFillPlotResVsFitted,
                       residType = residNames[[i]],
                       model = glmFullModel,
                       family = options$family)
      }
    }
  }
  return()
}

.glmFillPlotResVsFitted <- function(residType, model, family) {

  # compute residuals and fitted values
  stdResid <- .glmStdResidCompute(model = model, residType = residType)
  fittedY  <- fitted(model)

  # decide on constant-information scale transformations of fitted values
  fittedY  <- .constInfoTransform(family, fittedY)
  xlabText <- .constInfoTransName(family)


  # make plot
  thePlot <- ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y),
                             data = data.frame(y = stdResid,
                                               x = fittedY)) +
    ggplot2::geom_point(size  = 4,
                        shape = 1) +
    ggplot2::xlab(expression()) +
    ggplot2::ylab(gettextf("Standardized %1s residual", residType)) +
    ggplot2::geom_smooth(se = FALSE,
                         size = 0.6,
                         method = "loess",
                         method.args = list(degree = 1, family = "symmetric")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(thePlot)
}



# Plots: Residuals vs. predictor
.glmPlotResVsPredictor <- function(jaspResults, dataset, options, ready, residType, position) {
  if (!ready)
    return()

  plotType <- switch(residType,
                     "deviance" = "plotDevResVsX",
                     "Pearson"  = "plotPrsResVsX",
                     "quantile" = "plotQuanResVsX")

  if (!options[[plotType]])
    return()

  predictors <- c(options$covariates, options$factors)

  glmPlotResVsPredictorContainer <- createJaspContainer(gettextf("%1s Residuals vs. Predictor Plots", .capitalize(residType)))
  glmPlotResVsPredictorContainer$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                                          options           = plotType)
  glmPlotResVsPredictorContainer$position <- position
  jaspResults[[plotType]] <- glmPlotResVsPredictorContainer


  if (!is.null(jaspResults[["glmModels"]])) {
    glmFullModel <- jaspResults[["glmModels"]][["object"]][["fullModel"]]
    for (predictor in predictors) {
        .glmCreatePlotPlaceholder(glmPlotResVsPredictorContainer,
                                  index = predictor,
                                  title = gettextf("Standardized %1s residuals vs. %2s", residType, predictor))

        .glmInsertPlot(glmPlotResVsPredictorContainer[[predictor]],
                       .glmFillPlotResVsPredictor,
                       residType = residType,
                       predictor = predictor,
                       model = glmFullModel,
                       options = options)
    }
  }
  return()
}

.glmFillPlotResVsPredictor <- function(residType, predictor, model, options) {

  # compute residuals
  stdResid <- .glmStdResidCompute(model = model, residType = residType)
  # get predictor values
  if (predictor %in% options$factors) {
    predictorVec <- factor(model$data[[predictor]])
  } else {
    predictorVec <- model$data[[predictor]]
  }

  # make plot
  d <- data.frame(y = stdResid,
                  x = predictorVec)
  if (is.factor(predictorVec)) {
    thePlot <- ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y),
                               data = d) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_point(size  = 4,
                          shape = 1) +
      ggplot2::xlab(gettext(predictor)) +
      ggplot2::ylab(gettextf("Standardized %1s residual", residType)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  } else {
    thePlot <- ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y),
                               data = d) +
      ggplot2::geom_point(size  = 4,
                          shape = 1) +
      ggplot2::geom_smooth(se = FALSE,
                           size = 0.6,
                           method = "loess",
                           method.args = list(degree = 1, family = "symmetric")) +
      ggplot2::xlab(gettext(predictor)) +
      ggplot2::ylab(gettextf("Standardized %1s residual", residType)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  }
  return(thePlot)
}



# Plots: Residuals Q-Q
.glmPlotResQQ <- function(jaspResults, dataset, options, ready, position) {
  if (!ready)
    return()

  plotNames <- c("plotDevResQQ", "plotPrsResQQ", "plotQuanResQQ")
  residNames <- c("deviance", "Pearson", "quantile")

  glmPlotResQQContainer <- createJaspContainer(gettext("Normal Q-Q Plots: Standardized Residuals"))
  glmPlotResQQContainer$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                                 options           = plotNames)
  glmPlotResQQContainer$position <- position
  jaspResults[["glmPlotResQQ"]] <- glmPlotResQQContainer


  if (!is.null(jaspResults[["glmModels"]])) {
    glmFullModel <- jaspResults[["glmModels"]][["object"]][["fullModel"]]
    for (i in 1:length(plotNames)) {
      if (options[[plotNames[[i]]]]) {
        .glmCreatePlotPlaceholder(glmPlotResQQContainer,
                                  index = plotNames[[i]],
                                  title = gettextf("Normal Q-Q plot: Standardized %1s residuals", residNames[[i]]))

        .glmInsertPlot(glmPlotResQQContainer[[plotNames[[i]]]],
                       .glmFillPlotResQQ,
                       residType = residNames[[i]],
                       model = glmFullModel,
                       family = options$family)
      }
    }
  }
  return()
}

.glmFillPlotResQQ <- function(residType, model, family) {

  # compute residuals
  stdResid <- .glmStdResidCompute(model = model, residType = residType)

  # make plot
  thePlot <- ggplot2::ggplot(mapping = ggplot2::aes(sample = y),
                             data = data.frame(y = stdResid)) +
    ggplot2::stat_qq(shape = 1,
                     size = 4) +
    ggplot2::stat_qq_line() +
    ggplot2::xlab(gettext("Theoretical Quantiles")) +
    ggplot2::ylab(gettext("Sample Quantiles")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(thePlot)
}


# Plots: Partial residuals
.glmPlotResPartial <- function(jaspResults, dataset, options, ready, position) {
  if (!ready)
    return()

  if (!options[["plotPartial"]])
    return()

  predictors <- c(options$covariates, options$factors)

  glmPlotResPartialContainer <- createJaspContainer(gettext("Partial Residual Plots"))
  glmPlotResPartialContainer$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                                      options           = "plotPartial")
  glmPlotResPartialContainer$position <- position
  jaspResults[["plotPartial"]] <- glmPlotResPartialContainer


  if (!is.null(jaspResults[["glmModels"]])) {
    glmFullModel <- jaspResults[["glmModels"]][["object"]][["fullModel"]]
    for (predictor in predictors) {
      .glmCreatePlotPlaceholder(glmPlotResPartialContainer,
                                index = predictor,
                                title = gettextf("Partial residual plot for %1s", predictor))

      .glmInsertPlot(glmPlotResPartialContainer[[predictor]],
                     .glmFillPlotResPartial,
                     predictor = predictor,
                     model = glmFullModel,
                     options = options)
    }
  }
  return()
}

.glmFillPlotResPartial <- function(predictor, model, options) {

  # compute residuals
  partResidDf <- as.data.frame(resid(model, type = "partial"))
  partResid   <- partResidDf[[predictor]]

  # get original predictor values
  if (predictor %in% options$factors) {
    predictorVec <- factor(model$data[[predictor]])
  } else {
    predictorVec <- model$data[[predictor]]
  }

  # make plot
  d <- data.frame(y = partResid,
                  x = predictorVec)
  if (is.factor(predictorVec)) {
    thePlot <- ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y),
                               data = d) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_point(size  = 4,
                          shape = 1) +
      ggplot2::xlab(gettext(predictor)) +
      ggplot2::ylab(gettextf("Partial residual for %1s", predictor)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  } else {
    thePlot <- ggplot2::ggplot(mapping = ggplot2::aes(x = x, y = y),
                               data = d) +
      ggplot2::geom_point(size  = 4,
                          shape = 1) +
      ggplot2::geom_smooth(se = FALSE,
                           size = 0.6,
                           method = "loess",
                           method.args = list(degree = 1, family = "symmetric")) +
      ggplot2::xlab(gettext(predictor)) +
      ggplot2::ylab(gettextf("Partial residual for %1s", predictor)) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  }
  return(thePlot)
}



# Plot: Working responses vs. linear predictor
.glmPlotZVsEta <- function(jaspResults, dataset, options, ready, position) {
  if (!ready)
    return()

  if (!options[["plotZVsEta"]])
    return()

  glmPlotZVsEtaContainer <- createJaspContainer(gettext("Plot: Working responses vs. linear predictor"))
  glmPlotZVsEtaContainer$dependOn(optionsFromObject = jaspResults[["modelSummary"]],
                                  options           = "plotZVsEta")
  glmPlotZVsEtaContainer$position <- position
  jaspResults[["glmPlotZVsEta"]] <- glmPlotZVsEtaContainer

  if (!is.null(jaspResults[["glmModels"]])) {
    glmFullModel <- jaspResults[["glmModels"]][["object"]][["fullModel"]]
    .glmCreatePlotPlaceholder(glmPlotZVsEtaContainer,
                              index = "glmPlotZVsEta",
                              title = gettext("Plot: Working responses vs. linear predictor"))

    .glmInsertPlot(glmPlotZVsEtaContainer[["glmPlotZVsEta"]],
                   .glmFillPlotZVsEta,
                   model = glmFullModel,
                   options = options)
  }
  return()
}

.glmFillPlotZVsEta <- function(model, options) {

  # compute linear predictor eta and working responses z
  eta <- model[["linear.predictors"]]
  z <- resid(model, type="working") + eta

  # make plot
  xlabText <-
  thePlot <- ggplot2::ggplot(mapping = ggplot2::aes(x = x,
                                                    y = y),
                             data = data.frame(x = z,
                                               y = eta)) +
    ggplot2::geom_point(size  = 4,
                        shape = 1) +
    ggplot2::geom_smooth(se = FALSE,
                         size = 0.6,
                         method = "loess",
                         method.args = list(degree = 1, family = "symmetric")) +
    ggplot2::xlab(gettext("Working responses, z")) +
    ggplot2::ylab(expression(paste("Linear predictor, ", hat(eta)))) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  return(thePlot)
}
