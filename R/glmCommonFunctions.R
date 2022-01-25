# as explained in ?is.integer
.is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# create GLM formula
.createGLMFormula <- function(options, nullModel = FALSE) {

  leftTerm <- options$dependent
  modelTerms <- options$modelTerms
  includeIntercept <- options$includeIntercept

  if (includeIntercept)
    rightTerms <- "1"
  else
    rightTerms <- "-1"

  if (length(modelTerms) == 0) {
    f <- formula(paste(leftTerm, "~", rightTerms))
  } else {

    if (nullModel) {
      for (i in seq_along(modelTerms)) {
        nuisance <- modelTerms[[i]][["isNuisance"]]
        if (!is.null(nuisance) && nuisance) {
          term <- modelTerms[[i]][["components"]]
          if (length(term) == 1)
            rightTerms <- c(rightTerms, term)
          else
            rightTerms <- c(rightTerms, paste(term, collapse = ":"))
        }
      }

    } else {
      for (i in seq_along(modelTerms)) {
        term <- modelTerms[[i]][["components"]]
        if (length(term) == 1)
          rightTerms <- c(rightTerms, term)
        else
          rightTerms <- c(rightTerms, paste(term, collapse = ":"))
      }
    }
    f <- formula(paste(leftTerm, "~", paste(rightTerms, collapse = "+")))
  }
  return(f)
}


.glmComputeModel <- function(jaspResults, dataset, options) {
  if (!is.null(jaspResults[["glmModels"]]))
    return(jaspResults[["glmModels"]]$object)

  # make formulas for the full model and the null model
  ff <- .createGLMFormula(options, nullModel = FALSE)
  nf <- .createGLMFormula(options, nullModel = TRUE)
  # specify family and link
  if (options$family == "bernoulli")
    family <- "binomial"
  else
    family <- options$family
  familyLink <- eval(call(family, link = options$link))
  # compute full and null models
  if (options$weights == "") {
    fullModel <- glm(ff, family = familyLink, data = dataset, weights = NULL)
    nullModel <- glm(nf, family = familyLink, data = dataset, weights = NULL)
  } else {
    fullModel <- glm(ff,
                     family = familyLink,
                     data = dataset,
                     weights = get(options$weights))
    nullModel <- glm(nf,
                     family = familyLink,
                     data = dataset,
                     weights = get(options$weights))
  }

  glmModels <- list("nullModel" = nullModel,
                    "fullModel" = fullModel)
  # combine both models
  jaspResults[["glmModels"]] <- createJaspState()
  jaspResults[["glmModels"]]$dependOn(optionsFromObject = jaspResults[["modelSummary"]])
  jaspResults[["glmModels"]]$object <- glmModels

  return(glmModels)

}

.hasNuisance <- function(options) {
  return(any(sapply(options$modelTerms, function(x) x[["isNuisance"]])))
}

.formatTerm <- function(term, glmModel) {
  # input: string of model term & glmObj
  vars <- names(glmModel[["model"]][-1])

  if (attr(glmModel[["terms"]], "intercept"))
    vars <- c(vars, .v("(Intercept)"))

  # escape special regex characters
  vars <- gsub("(\\W)", "\\\\\\1", vars, perl=TRUE)

  # regex patterns
  pat1 <- paste0("\\:","(?=(",paste(vars, collapse = ")|("),"))")
  pat2 <- paste0("(?<=(",paste(vars, collapse = ")|("),"))")

  # split up string into components
  spl  <- strsplit(term, pat1, perl = TRUE)[[1]]
  spl2 <- lapply(spl, function(t) strsplit(t, pat2, perl = TRUE))

  # format and add back together
  col <- lapply(spl2, function(s) {
    if (length(unlist(s)) > 1) {
      varname <- .unv(unlist(s)[1])
      levname <- unlist(s)[2]
      return(paste0(varname, " (", levname, ")"))
    } else
      return(.unv(unlist(s)))
  })
  col2 <- paste(unlist(col), collapse = " * ")

  return(col2)
}


.glmCreatePlotPlaceholder <- function(container, index, title) {
  jaspPlot            <- createJaspPlot(title = title)
  jaspPlot$status     <- "running"
  container[[index]]  <- jaspPlot
  return()
}


.glmStdResidCompute <- function(model, residType) {
  if (residType == "deviance") {
    stdResid <- rstandard(model)
  }
  else if (residType == "Pearson") {
    phiEst <- summary(model)[["dispersion"]]
    resid <- resid(model, type="pearson")
    stdResid <- resid / sqrt(phiEst * (1 - hatvalues(model)))
  }
  else if (residType == "quantile") {
    set.seed(123)
    resid <- statmod::qresid(model)
    stdResid <- resid / sqrt(1 - hatvalues(model))
  } else {
    stdResid <- NULL
  }

  return(stdResid)
}

.glmInsertPlot <- function(jaspPlot, func, ...) {
  p <- try(func(...))

  if (inherits(p, "try-error")) {
    errorMessage <- .extractErrorMessage(p)
    jaspPlot$setError(gettextf("Plotting is not possible: %s", errorMessage))
  } else {
    jaspPlot$plotObject <- p
    jaspPlot$status     <- "complete"
  }
}

.constInfoTransform <- function(family, x) {
  switch(family,
         "bernoulli" = 1/(sin(sqrt(x))),
         "binomial" = 1/(sin(sqrt(x))),
         "poisson" = sqrt(x),
         "Gamma" = log(x),
         "inverse.gaussian" = 1/sqrt(x),
         "gaussian" = x)
}

.constInfoTransName <- function(family) {
  switch(family,
         "bernoulli" = expression(sin^-1 * sqrt("Fitted values")),
         "binomial" = expression(sin^-1 * sqrt("Fitted values")),
         "poisson" = expression(sqrt("Fitted values")),
         "Gamma" = expression(log("Fitted values")),
         "inverse.gaussian" = expression(1/sqrt("Fitted Values")),
         "gaussian" = "Fitted values")
}

.capitalize <- function(x) {
  x_new <- paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep="")
  return(x_new)
}
