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
