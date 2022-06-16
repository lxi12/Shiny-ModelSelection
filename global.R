library(shiny)
library(shinyBS) # Additional Bootstrap Controls (tooltips)
library(DT)
library(GGally)
library(corrgram)
library(visdat)
library(shinycssloaders) # busy spinner
library(recipes) # The recipes package is central to preprocessing
library(doParallel) # We employ a form of parallelism that works for MAC/Windows/Ubuntu
library(caret) # This code implements the CARET framework: see http://topepo.github.io/caret/index.html for details
library(rlang)
library(rJava)
library(devtools)
library(BiocManager)
if (!library("mixOmics", logical.return = TRUE)) {
  BiocManager::install("mixOmics", update = FALSE, ask = FALSE)  # This has moved from CRAN to Bioconductor
}
library(mixOmics)
library(rlist)
library(ggplot2)
library(butcher)

options(digits = 3)

glmnet_initial <- c("impute_knn","dow","dummy","corr") # <-- These are arbitrary starting values. Set these to your best recommendation
pls_initial <- c("impute_knn", "dow","dummy","corr")   # <-- These are arbitrary starting values. Set these to your best recommendation
rpart_initial <- c("dow") # <-- These are arbitrary starting values. Set these to your best recommendation
lm_initial <-  c("impute_knn","dow","dummy", "corr")
lmStepAIC_initial <- c("impute_knn","dow","dummy","corr")

rlm_initial <- c("impute_knn","dow","corr")
M5_initial <- c("impute_knn","dow","dummy","corr")
bstTree_initial <- c("impute_knn", "dow")
qrf_initial <- c("impute_knn", "dow","dummy")
svmLinear_initial <- c("impute_knn","dow","dummy","corr")      

svmRadial_initial <- c("impute_knn","dow","dummy","corr")
gaussprPoly_initial <- c("impute_knn","dow","dummy","corr")
neuralnet_initial <- c("impute_knn","dow","dummy","corr")
nnet_initial <- c("dow", "center", "scale")  
knn_initial <- c("impute_knn","dow","dummy","center","scale","corr")

cubist_initial <- c("impute_knn","dow", "dummy","corr")
earth_initial <- c("impute_knn", "dow","dummy","corr","pls")
pcr_initial <- c("impute_knn","dow","dummy","corr","pls")
             
# maintenance point ---------------------------------------------------------------------------------------------------------------------------
# add further preprocessing choices for the new methods here


startMode <- function(Parallel = TRUE) {
  if (Parallel) {
    clus <- makeCluster(min(c(3,detectCores(all.tests = FALSE, logical = TRUE))))
    registerDoParallel(clus)
    clus
  } else {
    NULL
  }
}

stopMode <- function(clus) {
  if (!is.null(clus)) {
    stopCluster(clus)
    registerDoSEQ()
  }
}

ppchoices <- c("impute_knn", "impute_bag", "impute_median", "impute_mode", "YeoJohnson", "naomit", "pca", "pls", "ica", "center", "scale", "month", "dow", "dateDecimal", "nzv", "zv","other", "dummy","poly", "interact", "indicate_na", "lincomb", "corr")


# This function turns the method's selected preprocessing into a recipe that honours the same order. 
# You are allowed to add more recipe steps to this.
dynamicSteps <- function(recipe, preprocess) {
  if (is.null(preprocess)) {
    stop("The preprocess list is NULL - check that you are using the correct control identifier")
  }
  for (s in preprocess) {
    if (s == "impute_knn") {
      recipe <- step_impute_knn(recipe, all_numeric_predictors(), all_nominal_predictors(), neighbors = 5) # 5 is a reasonable guess
    } else if (s == "impute_bag") {
      recipe <- step_impute_bag(recipe, all_numeric_predictors(), all_nominal_predictors(), trees = 25)
    } else if (s == "impute_median") {
      recipe <- step_impute_median(recipe, all_numeric_predictors())
    } else if (s == "impute_mode") {
      recipe <- recipes::step_impute_mode(recipe, all_nominal_predictors())
    } else if (s == "YeoJohnson") {
      recipe <- recipes::step_YeoJohnson(recipe, all_numeric_predictors())
    } else if (s == "naomit") {
      recipe <- recipes::step_naomit(recipe, all_predictors(), skip = TRUE)
    } else if (s == "pca") {
      recipe <- recipes::step_pca(recipe, all_numeric_predictors(), num_comp = 25)
    } else if (s == "pls") {
      recipe <- recipes::step_pls(recipe, all_numeric_predictors(), outcome = "Response", num_comp = 25)
    } else if (s == "ica") {
      recipe <- recipes::step_ica(recipe, all_numeric_predictors(), num_comp = 25)
    } else if (s == "center") {
      recipe <- recipes::step_center(recipe, all_numeric_predictors())
    } else if (s == "scale") {
      recipe <- recipes::step_scale(recipe, all_numeric_predictors())
    } else if (s == "month") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("month"), ordinal = FALSE)
    } else if (s == "dow") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("dow"), ordinal = FALSE)
    } else if (s == "dateDecimal") {
      recipe <- recipes::step_date(recipe, has_type("date"), features = c("decimal"), ordinal = FALSE)
    } else if (s == "zv") {
      recipe <- recipes::step_zv(recipe, all_numeric_predictors())
    } else if (s == "nzv") {
      recipe <- recipes::step_nzv(recipe, all_numeric_predictors(), freq_cut = 95/5, unique_cut = 10)
    } else if (s == "other") {
      recipe <- recipes::step_other(recipe, all_nominal_predictors())
    } else if (s == "dummy") {
      recipe <- recipes::step_dummy(recipe, all_nominal_predictors(), one_hot = FALSE)
    } else if (s == "poly") {
      recipe <- recipes::step_poly(recipe, all_numeric_predictors(), degree = 2)
    } else if (s == "interact") {
      recipe <- recipes::step_interact(recipe, terms = ~ all_numeric_predictors():all_numeric_predictors())
    } else if (s == "indicate_na") {
      recipe <- recipes::step_indicate_na(recipe, all_predictors())     #shadow variables (this needs to precede dealing with NA)
    } else if (s == "lincomb") {
      recipe <- recipes::step_lincomb(recipe, all_numeric_predictors()) #remove numeric variables that have linear combinations between them
    } else if (s == "corr") {
      recipe <- step_corr(recipe, all_numeric_predictors())
    } else if (s == "rm") {
      # intentionally blank
    } else {
      stop(paste("Attempting to use an unknown recipe step:", s))
    }
  }
  recipe
}

description <- function(name) {
  regexName <- paste0("^", name, "$") # force an regular expression exact match
  mlist <- caret::getModelInfo(model = regexName)[[name]]
  line1 <- paste0("Method \"", name, "\" is able to do ", paste(collapse = " and ", mlist$type), ".")
  line2 <- paste0("It uses parameters: ", paste0(collapse = ", ", mlist$parameters$parameter), ".")
  line3 <- paste0("Its characteristics are: ", paste0(collapse = ", ", mlist$tags))
  paste(sep = "\n", line1, line2, line3)
}


# attempts to keep the model file size small by not saving the global environment with each model
saveToRds <- function(model, name) {
  try(
    # ensure that WEKA based models can be restored
    if (!is.null(model$finalModel$classifier)) {
      rJava::.jcache(model$finalModel$classifier)
    }, silent = TRUE
  )
  
  file <- paste0(".", .Platform$file.sep, "SavedModels", .Platform$file.sep, name, ".rds")
  model2 <- butcher::axe_env(model, verbose = TRUE) # strip environments from the object to keep its size small
  #print(butcher::weigh(model2, threshold = 5, units = "MB"))
  saveRDS(model2, file)
}

loadRds <- function(name, session) {
  rdsfile <- file.path(".","SavedModels", paste0(name, ".rds"))
  if (!file.exists(rdsfile)) {
    showNotification("Model needs to be trained first", session = session, duration = 3)
    return(NULL)
  }
  showNotification(paste("Loading trained model", name, "from file", rdsfile), session = session, duration = 3)
  model <- readRDS(file = rdsfile)
  
  # try to update the preprocessing steps with the ones that were used
  steps <- model$recipe$steps
  seld <- c()
  for (step in steps) {
    s <- gsub(pattern = "step_", replacement = "", x = class(step)[1])
    if (s == "date") {
      s <- step$features[1]
    }
    seld <- c(seld, s)
  }
  preprocessingInputId <- paste0(name, "_Preprocess")
  updateSelectizeInput(session = session, inputId = preprocessingInputId, choices = ppchoices, selected = seld)
  if (length(seld) > 0) {
    showNotification(paste("Setting preprocessing for", name, "to", paste(seld, collapse = ",")), session = session, duration = 5)
  }
  model
}

deleteRds <- function(name) {
  rdsfile <- file.path(".","SavedModels", paste0(name, ".rds"))
  if (file.exists(rdsfile)) {
    ok <- unlink(rdsfile, force = TRUE)
  } else {
    ok <- TRUE
  }
  ok
}

