# as explained in ?is.integer
.is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# create GLM formula
.createGLMFormula <- function(options, nullModel = FALSE) {

  dependent <- options$dependent
  modelTerms <- options$modelTerms
  includeIntercept <- options$includeIntercept

  if (options$family == "binomialAgg") {
    dependentAgg <- options$dependentAggregation
    leftTerm <- paste("cbind(", dependent, ",", dependentAgg, ")", sep = "") }
  else {
    leftTerm <- dependent
  }

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
  if (options$family == "binomialAgg")
    family <- "binomial"
  else
    family <- options$family
  familyLink <- eval(call(family, link = options$link))
  # compute full and null models
  fullModel <- glm(ff, family = familyLink, data = dataset)
  nullModel <- glm(nf, family = familyLink, data = dataset)
  glmModels <- list("nullModel" = nullModel,
                    "fullModel" = fullModel)
  # combine both models
  jaspResults[["glmModels"]] <- createJaspState(glmModels)
  jaspResults[["glmModels"]]$dependOn(optionsFromObject = jaspResults[["modelSummary"]])
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
