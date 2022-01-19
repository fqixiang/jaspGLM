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

glmClassical <- function(jaspResults, dataset = NULL, options, ...) {
  if (options$family == "binomialAgg") {
    ready <- (options$dependent != "" && options$dependentAggregation != "")

  } else {
    ready <- options$dependent != ""
  }


  if (ready) {
    dataset <- .glmReadData(dataset, options)
    .glmCheckDataErrors(dataset, options)
  }

  #output tables
  .glmModelSummaryTable(jaspResults, dataset, options, ready)
  .glmEstimatesTable(jaspResults, dataset, options, ready)

  return()
}

# Preprocessing functions
.glmReadData <- function(dataset, options) {
  if (!is.null(dataset)) {
    return(dataset)
  }
  else {
    numericVars  <- unlist(c(options$covariates, options$weights))
    numericVars  <- numericVars[numericVars != ""]
    factorVars   <- unlist(c(options$factors))
    factorVars   <- factorVars[factorVars != ""]
    dependentVar <- unlist(c(options$dependent))
    dependentVar <- dependentVar[dependentVar != ""]

    if (options$family == "binomial") {
      return(.readDataSetToEnd(columns.as.numeric  = numericVars,
                               columns.as.factor   = c(factorVars, dependentVar),
                               exclude.na.listwise = c(numericVars, factorVars, dependentVar)))
    }
    else if (options$family == "binomialAgg") {
      dependentAggVar <- unlist(c(options$dependentAggregation))
      dependentAggVar <- dependentAggVar[dependentAggVar != ""]

      return(.readDataSetToEnd(columns.as.numeric  = c(numericVars, dependentVar, dependentAggVar),
                               columns.as.factor   = factorVars,
                               exclude.na.listwise = c(numericVars, factorVars, dependentVar, dependentAggVar)))
    }
    else {
      return(.readDataSetToEnd(columns.as.numeric  = c(numericVars, dependentVar),
                               columns.as.factor   = factorVars,
                               exclude.na.listwise = c(numericVars, factorVars, dependentVar)))
    }
  }
}

# Check errors
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

  if (length(options$covariates) != 0)
    .hasErrors(dataset,
               type = c("observations", "infinity", "variance", "varCovData"),
               all.target = options$covariates,
               observations.amount  = "< 2",
               exitAnalysisIfErrors = TRUE)

  # check family-specific dependent variable errors
  if (options$family == "binomial") {

    .hasErrors(dataset,
               type = "factorLevels",
               factorLevels.target  = options$dependent,
               factorLevels.amount  = '!= 2',
               exitAnalysisIfErrors = TRUE)

  } else if (options$family == "binomialAgg") {

    if (any(dataset[, options$dependent] < 0) || any(!.is.wholenumber(dataset[, options$dependent])))
      .quitAnalysis(gettextf("The %s family requires that the dependent variable is an integer.", options$family))

    if (any(dataset[, options$dependentAggregation] < 0) || any(!.is.wholenumber(dataset[, options$dependentAggregation])))
      .quitAnalysis(gettextf("The %s family requires that the number of trials variable is an integer.", options$family))

  } else if (options$family %in% c("Gamma", "inverse.gaussian")) {

    if (any(dataset[, options$dependent] <= 0))
      .quitAnalysis(gettextf("The %s family requires that the dependent variable is positive.", options$family))

  } else if (options$family == "poisson") {

    if (any(dataset[, options$dependent] < 0 | any(!.is.wholenumber(dataset[, options$dependent]))))
      .quitAnalysis(gettextf("The %s family requires that the dependent variable is an integer.", options$family))

  } else if (options$family == "gaussian") {

    if (!is.numeric(dataset[, options$dependent]))
      .quitAnalysis(gettextf("The %s family requires that the dependent variable is a numerical variable.", options$family))

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

  dependList <- c("dependent", "family", "link", "modelTerms", "includeIntercept")
  modelSummary$dependOn(dependList)
  modelSummary$position <- 1
  modelSummary$showSpecifiedColumnsOnly <- TRUE

  modelSummary$addColumnInfo(name = "mod", title = gettext("Model"),    type = "string")
  modelSummary$addColumnInfo(name = "dev", title = gettext("Deviance"), type = "number")
  modelSummary$addColumnInfo(name = "aic", title = gettext("AIC"),      type = "number", format="dp:3")
  modelSummary$addColumnInfo(name = "bic", title = gettext("BIC"),      type = "number", format="dp:3")

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
    rows <- list(
      list(mod = "H\u2080",
           dev = glmModels[["nullModel"]][["deviance"]],
           aic = glmModels[["nullModel"]][["aic"]],
           bic = BIC(glmModels[["nullModel"]])),
      list(mod = "H\u2081",
           dev = glmModels[["fullModel"]][["deviance"]],
           aic = glmModels[["fullModel"]][["aic"]],
           bic = BIC(glmModels[["fullModel"]]))
    )
  } else{
    rows <- list(
      list(mod = "H\u2080",
           dev = ".",
           aic = ".",
           bic = "."),
      list(mod = "H\u2081",
           dev = ".",
           aic = ".",
           bic = ".")
    )
  }
  jaspResults[["modelSummary"]]$addRows(rows)
}


# GLM estimates table
.glmEstimatesTable <- function(jaspResults, dataset, options, ready) {
  if (!options$coefEstimates || !is.null(jaspResults[["estimatesTable"]]))
    return()

  estimatesTable <- createJaspTable(gettext("Coefficients"))
  estimatesTable$dependOn(optionsFromObject   = jaspResults[["modelSummary"]],
                          options             = c("coefEstimates"))
  estimatesTable$position <- 2
  estimatesTable$showSpecifiedColumnsOnly <- TRUE

  estimatesTable$addColumnInfo(name = "param",   title = "", type = "string")
  estimatesTable$addColumnInfo(name = "est",     title = gettext("Estimate"), type = "number", format="dp:3")
  estimatesTable$addColumnInfo(name = "se",      title = gettext("Standard Error"), type = "number", format="dp:3")
  estimatesTable$addColumnInfo(name = "zval",    title = gettext("z"), type = "number")
  estimatesTable$addColumnInfo(name = "pval",    title = gettext("p"), type = "pvalue")

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

  if (length(rowNames) == 1) {
    jaspResults[["estimatesTable"]]$addRows(
      list(param = .formatTerm(rowNames, glmModels[["fullModel"]]),
           est   = modelSummary[1, 1],
           se    = modelSummary[1, 2],
           zval  = modelSummary[1, 3],
           pval  = modelSummary[1, 4])
    )
  } else {
    for (i in seq_along(rowNames)) {
      jaspResults[["estimatesTable"]]$addRows(
        list(param = .formatTerm(rowNames[i], glmModels[["fullModel"]]),
             est   = modelSummary[i, 1],
             se    = modelSummary[i, 2],
             zval  = modelSummary[i, 3],
             pval  = modelSummary[i, 4])
      )
    }
  }

  if (options$family == "binomial") {
    dv      <- as.character(glmModels[["nullModel"]][["terms"]])[2]
    dvLevel <- levels(glmModels[["nullModel"]][["data"]][[dv]])[2]

    jaspResults[["estimatesTable"]]$addFootnote(gettextf("%1$s level '%2$s' coded as class 1.", dv, dvLevel))
  }
}
