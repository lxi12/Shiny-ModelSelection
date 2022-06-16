shinyServer(function(input, output, session) {
  
  # initialisation ----
  models <- reactiveValues()  # this is a collection of the models

    
  # Ensure the "SavedModels folder exists
  if (!"./SavedModels" %in% list.dirs()) {
    dir.create("./SavedModels")
  }
  
  shiny::onSessionEnded(stopApp)

  
  # reactive getData ----
  getData <- reactive({
    d <- read.csv(file = "data.csv", row.names = "Patient", stringsAsFactors = TRUE)  # "Patient" is no longer a variable
    d$ObservationDate <- as.Date(d$ObservationDate, "%Y-%m-%d")
    d
  })
  
  # output BoxPlots ----
  output$BoxPlots <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, input$Multiplier, length(numeric) > 0)
    d <- scale(d[,numeric], center = input$Normalise, scale = input$Normalise)
    boxplot(d, outline = TRUE, main = paste("Boxplot using IQR multiplier of", input$Multiplier), range = input$Multiplier, las = 2)
  })
  
  # output Missing ----
  output$Missing <- renderPlot({
    d <- getData()
    vis_dat(d)
  })
  
  # output Corr ----
  output$Corr <- renderPlot({
    d <- getData()
    numeric <- sapply(d, FUN = is.numeric)
    req(d, length(numeric) > 0)
    corrgram::corrgram(d, order = "OLO", main = "Numeric Data Correlation")
  })
  
  # output Pairs ----
  output$Pairs <- renderPlot({
    d <- getData()
    numd <- subset(d, select=c(21, 1:4))
    ggpairs(data = numd,  mapping = ggplot2::aes(colour = d[,'BloodType']), 
            title = "Numeric Pairs Plot coloured by Bloodtype")})
  
  # output DataSummary ----
  output$DataSummary <- renderPrint({
    str(getData())
  })
  
  # output Table ----
  output$Table <- DT::renderDataTable({
    d <- getData()
    numeric <- c(FALSE, sapply(d, is.numeric)) # never round rownames which are the first column (when shown)
    DT::datatable(d) %>%
      formatRound(columns = numeric, digits = 3)
  })
  
  # reactive get Split
  getSplit <- reactive({
    set.seed(199)
    createDataPartition(y = getData()$Response, p = input$Split, list = FALSE)
  })
  
  # reactive getMethods ----
  getMethods <- reactive({
    mi <- caret::getModelInfo()
    Label <- vector(mode = "character", length = length(mi))
    Package <- vector(mode = "character", length = length(mi))
    Hyperparams <- vector(mode = "character", length = length(mi))
    Regression <- vector(mode = "logical", length = length(mi))
    Classification <- vector(mode = "logical", length = length(mi))
    Tags <- vector(mode = "character", length = length(mi))
    ClassProbs <- vector(mode = "character", length = length(mi))
    for (row in 1:length(mi)) {
      Label[row] <- mi[[row]]$label
      libs <- mi[[row]]$library
      libs <- na.omit(libs[libs != ""]) # remove blank libraries
      if (length(libs) > 0) {
        present <- vector(mode = "logical", length = length(libs))
        suppressWarnings({
          for (lib in 1:length(libs)) {
            present[lib] <- require(package = libs[lib], warn.conflicts = FALSE, character.only = TRUE, quietly = TRUE)
          }
        })
        check <- ifelse(present, "", as.character(icon(name = "ban")))
        Package[row] <- paste(collapse = "<br/>", paste(mi[[row]]$library, check))
      }
      d <- mi[[row]]$parameters
      Hyperparams[row] <- paste(collapse = "<br/>", paste0(d$parameter, " - ", d$label, " [", d$class,"]"))
      Regression[row] <- ifelse("Regression" %in% mi[[row]]$type, as.character(icon("check-square", class = "fa-3x")), "")
      Classification[row] <- ifelse("Classification" %in% mi[[row]]$type , as.character(icon("check-square", class = "fa-3x")),"")
      Tags[row] <- paste(collapse = "<br/>", mi[[row]]$tags)
      ClassProbs[row] <- ifelse(is.function(mi[[row]]$prob), as.character(icon("check-square", class = "fa-3x")), "")
    }
    data.frame(Model = names(mi), Label, Package, Regression, Classification, Tags, Hyperparams, ClassProbs, stringsAsFactors = FALSE)
  })
  
  # output Available ----
  output$Available <- DT::renderDataTable({
     m <- getMethods()
     m <- m[m$Regression != "", !colnames(m) %in% c("Regression", "Classification", "ClassProbs")]  # hide columns because we are looking at regression methods only
     DT::datatable(m, escape = FALSE, options = list(pageLength = 5, lengthMenu = c(5,10,15)), rownames = FALSE, selection = "none")
  })
  
  # reactive getTrainData ----
  getTrainData <- reactive({
    getData()[getSplit(),]
  })
  
  # reactive getTestData ----
  getTestData <- reactive({
    getData()[-getSplit(),]
  })
  
  # reactive getTrControl ----
  getTrControl <- reactive({
    # shared bootstrap specification i.e. 25 x bootstrap
    y <- getTrainData()[,"Response"]
    n <- 25
    set.seed(673)
    seeds <- vector(mode = "list", length = n + 1)
    for (i in 1:n) {
      seeds[[i]] <- as.integer(c(runif(n = 55, min = 1000, max = 5000)))
    }
    seeds[[n + 1]] <- as.integer(runif(n = 1, min = 1000, max = 5000))
    trainControl(method = "boot", number = n, repeats = NA, allowParallel = TRUE, search = "grid", 
                 index = caret::createResample(y = y, times = n), savePredictions = "final", seeds = seeds, 
                 trim = TRUE)
  })
  
  # output SplitSummary ----
  output$SplitSummary <- renderPrint({
    cat(paste("Training observations:", nrow(getTrainData()), "\n", "Testing observations:", nrow(getTestData())))
  })
  
  # reactive getResamples ----
  getResamples <- reactive({
    models2 <- reactiveValuesToList(models) %>% 
      rlist::list.clean( fun = is.null, recursive = FALSE)
    req(length(models2) > 1)
    results <- caret::resamples(models2)
    
    #scale metrics using null model. Tough code to follow -sorry
    NullModel <- "null"
    if (input$NullNormalise & NullModel %in% results$models) {
      actualNames <- colnames(results$values)
      # Normalise the various hyper-metrics except R2 (as this is already normalised)
      for (metric in c("RMSE", "MAE")) {
        col <- paste(sep = "~", NullModel, metric)
        if (col %in% actualNames) {
          nullMetric <- mean(results$values[, col], na.rm = TRUE)
          if (!is.na(nullMetric) & nullMetric != 0) {
            for (model in results$models) {
              mcol <- paste(sep = "~", model, metric)
              if (mcol %in% actualNames) {
                results$values[, mcol] <- results$values[, mcol] / nullMetric
              }
            }
          }
        }
      }
    }
    
    # hide results worse than null model
    subset <- rep(TRUE, length(models2))
    if (input$HideWorse & NullModel %in% names(models2)) {
      actualNames <- colnames(results$values)
      col <- paste(sep = "~", "null","RMSE" )
      if (col %in% actualNames) {
        nullMetric <- mean(results$values[, col], na.rm = TRUE)
        if (!is.na(nullMetric)) {
          m <- 0
          for (model3 in results$models) {
            m <- m + 1
            mcol <- paste(sep = "~", model3, "RMSE")
            if (mcol %in% actualNames) {
              subset[m] <- mean(results$values[, mcol], na.rm = TRUE) <= nullMetric
            }
          }
        }
      }
      results$models <- results$models[subset]
    }
    
    updateRadioButtons(session = session, inputId = "Choice", choices = results$models)
    results
  })
  
  # output SelectionBoxPlot (plot) ----
  output$SelectionBoxPlot <- renderPlot({
    mod <- getResamples()
    bwplot(mod, notch = input$Notch)
  })
  
  # output Title (UI) ----
  output$Title <- renderUI({
    tags$h3(paste("Unseen data results for chosen model:", input$Choice))
  })
  
  # reactive getTestResults ----
  getTestResults <- reactive({
    dat <- getTestData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # reactive getTrainResults ----
  getTrainResults <- reactive({
    dat <- getTrainData()
    req(input$Choice)
    mod <- models[[input$Choice]]
    predictions <- predict(mod, newdata = dat)
    d <- data.frame(dat$Response, predictions, row.names = rownames(dat))
    colnames(d) <- c("obs", "pred")
    d
  })
  
  # Range for charts
  getResidualRange <- reactive({
    d1 <- getTrainResults()
    d1$residuals <- d1$obs - d1$pred
    d2 <- getTestResults()
    d2$residuals <- d2$obs - d2$pred
    d <- c(d1$residuals, d2$residuals)
    range(d, na.rm = TRUE)
  })
  
  # output TestSummary (print)
  output$TestSummary <- renderPrint({
    if (is.na(input$Choice) || input$Choice == "") {
      cat("No model chosen")
    } else {
      caret::defaultSummary(getTestResults())
    }
  })
  
  # output TestPlot (plot) ----
  output$TestPlot <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    par(pty = "s")
    range <- range(c(d$obs, d$pred), na.rm = TRUE)
    plot(d, xlim = range, ylim = range, main = "Predicted versus Observed for test data")
    abline(a = 0, b = 1, col = c("blue"), lty = c(2), lwd = c(3))
  })
  
  # output TestResiduals (plot) ----
  output$TestResiduals <- renderPlot({
    d <- getTestResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical", ) +
      ggrepel::geom_text_repel() +
      labs(title = "Test-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  # output TrainResiduals (plot) ----
  output$TrainResiduals <- renderPlot({
    d <- getTrainResults()
    req(nrow(d) > 0)
    d$residuals <- d$obs - d$pred
    coef <- input$IqrM
    limits <- boxplot.stats(x = d$residuals, coef = coef)$stats
    label <- ifelse(d$residuals < limits[1] | d$residuals > limits[5], rownames(d), NA)
    ggplot(d, mapping = aes(y = residuals, x = 0, label = label)) +
      ylim(getResidualRange()[1], getResidualRange()[2]) +
      geom_boxplot(coef = coef, orientation = "vertical") +
      ggrepel::geom_text_repel() +
      labs(title = "Train-Residual Boxplot",  subtitle = paste(coef, "IQR Multiplier")) +
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  
  # 0 METHOD * null ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getNullRecipe ----
  getNullRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData())
  })
  
  # observeEvent null_Go ----
  observeEvent(
    input$null_Go,
    {
      method <- "null"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getNullRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl())
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$null_Load,
    {
      method  <- "null"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$null_Delete,
    {
      models[["null"]] <- NULL
      gc()
    }
  )
  
  # observeEvent null_Metrics ----
  output$null_Metrics <- renderTable({
    req(models$null)
    models$null$results[ which.min(models$null$results[, "RMSE"]), ]
  })
  
  # output null_Recipe ---
  output$null_Recipe <- renderPrint({
    req(models$null)
    models$null$recipe
  })  
  

  
  
  # 1 METHOD * glmnet ---------------------------------------------------------------------------------------------------------------------------
  library(glmnet)   #  <------ Declare any modelling packages that are needed (see Method List tab)
  # reactive getGlmnetRecipe ----
  getGlmnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$glmnet_Preprocess) %>%           # use <method>_Preprocess 
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent glmnet_Go ----
  observeEvent(
    input$glmnet_Go,
    {
      method <- "glmnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getGlmnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      },
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
     }
  )
  
  observeEvent(
    input$glmnet_Load,
    {
      method  <- "glmnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$glmnet_Delete,
    {
      models[["glmnet"]] <- NULL
      gc()
    }
  )
  
  # output glmnet_ModelSummary (text) ----
  output$glmnet_ModelSummary0 <- renderText({
    description("glmnet")   # Use the caret method name here
  })
  
  # output glmnet_Metrics (table) ----
  output$glmnet_Metrics <- renderTable({
    req(models$glmnet)
    models$glmnet$results[ which.min(models$glmnet$results[, "RMSE"]), ]
  })
  
  # output glmnet_ModelPlots (plot) ----
  output$glmnet_ModelPlots <- renderPlot({
    req(models$glmnet)
    plot(models$glmnet)
  })

  # output glmnet_Recipe (print) ----
  output$glmnet_Recipe <- renderPrint({
    req(models$glmnet)
    models$glmnet$recipe
  })  
  
  # output glmnet_ModelSummary2 (print) ----
  output$glmnet_ModelSummary2 <- renderPrint({
    req(models$glmnet)
    print(models$glmnet)
  })

  # output glmnet_Coef (print) ----
  output$glmnet_Coef <- renderTable({
    req(models$glmnet)
    co <- as.matrix(coef(models$glmnet$finalModel, s  = models$glmnet$bestTune$lambda))  # special for glmnet
    df <- as.data.frame(co, row.names = rownames(co))
    df[df$s1 != 0.000, ,drop=FALSE]
  }, rownames = TRUE, colnames = FALSE)
  
  
  
  # 2 METHOD * pls ---------------------------------------------------------------------------------------------------------------------------
  library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getPlsRecipe ----
  getPlsRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pls_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent pls_Go ----
  observeEvent(
    input$pls_Go,
    {
      method <- "pls"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getPlsRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$pls_Load,
    {
      method  <- "pls"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pls_Delete,
    {
      models[["pls"]] <- NULL
      gc()
    }
  )
  
  # output pls_ModelSummary0 (text) ----
  output$pls_ModelSummary0 <- renderText({
    description("pls")   # Use the caret method name here
  })

  # output pls_Metrics (table) ----
  output$pls_Metrics <- renderTable({
    req(models$pls)
    models$pls$results[ which.min(models$pls$results[, "RMSE"]), ]
  })
  
  # output pls_ModelPlots (plot) ----
  output$pls_ModelPlots <- renderPlot({
    req(models$pls)
    plot(models$pls)
  })     
  
  # output pls_Recipe (print) ----
  output$pls_Recipe <- renderPrint({
    req(models$pls)
    models$pls$recipe
  })  

  # output pls_ModelSummary2 (print) ----
  output$pls_ModelSummary2 <- renderPrint({
    req(models$pls)
    summary(models$pls$finalModel)
  })
  
  # output pls_Coef (print) ----
  output$pls_Coef <- renderTable({
    req(models$pls)
    co <- coef(models$pls$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  
  # 3 METHOD * rpart ---------------------------------------------------------------------------------------------------------------------------
  library(rpart)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(rpart.plot)
  
  # reactive getRpartRecipe ----
  getRpartRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rpart_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent rpart_Go ----
  observeEvent(
    input$rpart_Go,
    {
      method <- "rpart"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getRpartRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5, na.action = na.rpart)  #<- note the rpart-specific value for na.action (not needed for other methods)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )

  observeEvent(
    input$rpart_Load,
    {
      method  <- "rpart"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rpart_Delete,
    {
      models[["rpart"]] <- NULL
      gc()
    }
  )
  
  # output rpart_ModelSummary0 (print) ----
  output$rpart_ModelSummary0 <- renderText({
    description("rpart")   # Use the caret method name here
  })
  
  # output rpart_Metrics (table) ----
  output$rpart_Metrics <- renderTable({
    req(models$rpart)
    models$rpart$results[ which.min(models$rpart$results[, "RMSE"]), ]
  })
  
  # output rpart_Recipe (print) ----
  output$rpart_Recipe <- renderPrint({
    req(models$rpart)
    models$rpart$recipe
  })  
  
  # output rpart_ModelPlots (plot) ----
  output$rpart_ModelPlots <- renderPlot({
    req(models$rpart)
    plot(models$rpart)
  })
  
  # output rpart_ModelTree (plot) ----
  output$rpart_ModelTree <- renderPlot({
    req(models$rpart)
    rpart.plot::rpart.plot(models$rpart$finalModel, roundint = FALSE)
  })     
  

  
  # maintenance point ---------------------------------------------------------------------------------------------------------------------------
  # add further methods here  
  
  # 4 METHOD * LM ---------------------------------------------------------------------------------------------------------------------------
  library(stats)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getlmRecipe ----
  getlmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$lm_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent lm_Go ----
  observeEvent(
    input$lm_Go,
    {
      method <- "lm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getlmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$lm_Load,
    {
      method  <- "lm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$lm_Delete,
    {
      models[["lm"]] <- NULL
      gc()
    }
  )

  output$lm_ModelSummary0 <- renderText({
    description("lm")   # Use the caret method name here
  })

  output$lm_Metrics <- renderTable({
    req(models$lm)
    models$lm$results[ which.min(models$lm$results[, "RMSE"]), ]
  })

  output$lm_Recipe <- renderPrint({
    req(models$lm)
    models$lm$recipe
  })  

  output$lm_ModelPlots <- renderPlot({
    req(models$lm)
    par(mfrow = c(2, 2))
    plot(models$lm$finalModel)
  })  

  output$lm_ModelSummary2 <- renderPrint({
    req(models$lm)
    summary(models$lm$finalModel)
  })

  output$lm_Coef <- renderTable({
    req(models$lm)
    co <- coef(models$lm$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  

  
  # 5 METHOD * LmStepAIC ---------------------------------------------------------------------------------------------------------------------------
  library(MASS)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getlmStepAICRecipe ----
  getlmStepAICRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$lmStepAIC_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent lmStepAIC_Go ----
  observeEvent(
    input$lmStepAIC_Go,
    {
      method <- "lmStepAIC"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getlmStepAICRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$lmStepAIC_Load,
    {
      method  <- "lmStepAIC"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$lmStepAIC_Delete,
    {
      models[["lmStepAIC"]] <- NULL
      gc()
    }
  )
  
  output$lmStepAIC_ModelSummary0 <- renderText({
    description("lmStepAIC")   # Use the caret method name here
  })

  output$lmStepAIC_Metrics <- renderTable({
    req(models$lmStepAIC)
    models$lmStepAIC$results[ which.min(models$lmStepAIC$results[, "RMSE"]), ]
  })

  output$lmStepAIC_Recipe <- renderPrint({
    req(models$lmStepAIC)
    models$lmStepAIC$recipe
  })  

  output$lmStepAIC_ModelPlots <- renderPlot({
    req(models$lmStepAIC)
    par(mfrow = c(2, 2))
    plot(models$lmStepAIC$finalModel)
  })  

  output$lmStepAIC_ModelSummary2 <- renderPrint({
    req(models$lmStepAIC)
    summary(models$lmStepAIC$finalModel)
  })

  output$lmStepAIC_Coef <- renderTable({
    req(models$lmStepAIC)
    co <- coef(models$lmStepAIC$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  

  
  # 6 METHOD * RLM ---------------------------------------------------------------------------------------------------------------------------
  library(MASS)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getrlmRecipe ----
  getrlmRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$rlm_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent rlm_Go ----
  observeEvent(
    input$rlm_Go,
    {
      method <- "rlm"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getrlmRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25, na.action = na.exclude)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$rlm_Load,
    {
      method  <- "rlm"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$rlm_Delete,
    {
      models[["rlm"]] <- NULL
      gc()
    }
  )
  
  output$rlm_ModelSummary0 <- renderText({
    description("rlm")   # Use the caret method name here
  })
  
  output$rlm_Metrics <- renderTable({
    req(models$rlm)
    models$rlm$results[ which.min(models$rlm$results[, "RMSE"]), ]
  })
  
  output$rlm_Recipe <- renderPrint({
    req(models$rlm)
    models$rlm$recipe
  })  
  
  output$rlm_ModelSummary2 <- renderPrint({
    req(models$rlm)
    summary(models$rlm$finalModel)
  })
  
  output$rlm_Coef <- renderTable({
    req(models$rlm)
    co <- coef(models$rlm$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  

  
  # 7 METHOD * M5 ---------------------------------------------------------------------------------------------------------------------------
  library(RWeka)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getM5Recipe ----
  getM5Recipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$M5_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent M5_Go ----
  observeEvent(
    input$M5_Go,
    {
      method <- "M5"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getM5Recipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5)   
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$M5_Load,
    {
      method  <- "M5"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$M5_Delete,
    {
      models[["M5"]] <- NULL
      gc()
    }
  )

  output$M5_ModelSummary0 <- renderText({
    description("M5")   # Use the caret method name here
  })

  output$M5_Metrics <- renderTable({
    req(models$M5)
    models$M5$results[ which.min(models$M5$results[, "RMSE"]), ]
  })

  output$M5_ModelPlots <- renderPlot({
    req(models$M5)
    plot(models$M5)
  })

  output$M5_Recipe <- renderPrint({
    req(models$M5)
    models$M5$recipe
  })  

  output$M5_ModelSummary2 <- renderPrint({
    req(models$M5)
    summary(models$M5$finalModel)
  })

  output$M5_Coef <- renderPrint({
    req(models$M5)
    models$M5$finalModel   
  })
  
  
  
  # 8 METHOD * BstTree ---------------------------------------------------------------------------------------------------------------------------
  library(bst)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  library(plyr)
  
  # reactive getbstTreeRecipe ----
  getbstTreeRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$bstTree_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))
  })
  
  # observeEvent bstTree_Go ----
  observeEvent(
    input$bstTree_Go,
    {
      method <- "bstTree"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getbstTreeRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(),
                              tuneLength = 5)   
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$bstTree_Load,
    {
      method  <- "bstTree"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$bstTree_Delete,
    {
      models[["bstTree"]] <- NULL
      gc()
    }
  )
  
  output$bstTree_ModelSummary0 <- renderText({
    description("bstTree")   # Use the caret method name here
  })
  
  output$bstTree_Metrics <- renderTable({
    req(models$bstTree)
    models$bstTree$results[ which.min(models$bstTree$results[, "RMSE"]), ]
  })
  
  output$bstTree_ModelPlots <- renderPlot({
    req(models$bstTree)
    plot(models$bstTree)
  })
  
  output$bstTree_Recipe <- renderPrint({
    req(models$bstTree)
    models$bstTree$recipe
  })  
  
  output$bstTree_ModelSummary2 <- renderPrint({
    req(models$bstTree)
    summary(models$bstTree$finalModel)
  })
  
  output$bstTree_ModelTree <- renderPrint({
    req(models$bstTree)
    models$bstTree$finalModel$ml.fit
  })
  
  
  
  # 9 METHOD * QRF ---------------------------------------------------------------------------------------------------------------------------
  library(quantregForest)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getqrfRecipe ----
  getqrfRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$qrf_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent qrf_Go ----
  observeEvent(
    input$qrf_Go,
    {
      method <- "qrf"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getqrfRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 5, na.action = na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$qrf_Load,
    {
      method  <- "qrf"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$qrf_Delete,
    {
      models[["qrf"]] <- NULL
      gc()
    }
  )
  
  output$qrf_ModelSummary0 <- renderText({
    description("qrf")   # Use the caret method name here
  })
  
  output$qrf_Metrics <- renderTable({
    req(models$qrf)
    models$qrf$results[ which.min(models$qrf$results[, "RMSE"]), ]
  })
  
  output$qrf_ModelPlots <- renderPlot({
    req(models$qrf)
    plot(models$qrf)
  })     
  
  output$qrf_Modelimportance <- renderPlot({
    req(models$qrf)
    library(randomForest)
    randomForest::varImpPlot(models$qrf$finalModel)
    
  })
  
  output$qrf_Recipe <- renderPrint({
    req(models$qrf)
    models$qrf$recipe
  })  
  
  output$qrf_ModelSummary2 <- renderPrint({
    req(models$qrf)
    print(models$qrf)
  })
  
  
  
  # 10 METHOD * SvmLinear ---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getsvmLinearRecipe ----
  getsvmLinearRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmLinear_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmLinear_Go ----
  observeEvent(
    input$svmLinear_Go,
    {
      method <- "svmLinear"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getsvmLinearRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 20, tuneGrid=expand.grid(C=seq(0.01, 1, length=20)), na.action=na.exclude)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$svmLinear_Load,
    {
      method  <- "svmLinear"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmLinear_Delete,
    {
      models[["svmLinear"]] <- NULL
      gc()
    }
  )

  output$svmLinear_ModelSummary0 <- renderText({
    description("svmLinear")   # Use the caret method name here
  })

  output$svmLinear_Metrics <- renderTable({
    req(models$svmLinear)
    models$svmLinear$results[ which.min(models$svmLinear$results[, "RMSE"]), ]
  })

  output$svmLinear_ModelPlots <- renderPlot({
    req(models$svmLinear)
    plot(models$svmLinear)
  })     

  output$svmLinear_Recipe <- renderPrint({
    req(models$svmLinear)
    models$svmLinear$recipe
  })  

  output$svmLinear_ModelSummary2 <- renderPrint({
    req(models$svmLinear)
    print(models$svmLinear)
  })

  

  # 11 METHOD * SvmRadial ---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getsvmRadialRecipe ----
  getsvmRadialRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$svmRadial_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent svmRadial_Go ----
  observeEvent(
    input$svmRadial_Go,
    {
      method <- "svmRadial"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getsvmRadialRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 20, na.action=na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$svmRadial_Load,
    {
      method  <- "svmRadial"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$svmRadial_Delete,
    {
      models[["svmRadial"]] <- NULL
      gc()
    }
  )
  
  output$svmRadial_ModelSummary0 <- renderText({
    description("svmRadial")   # Use the caret method name here
  })
  
  output$svmRadial_Metrics <- renderTable({
    req(models$svmRadial)
    models$svmRadial$results[ which.min(models$svmRadial$results[, "RMSE"]), ]
  })
  
  output$svmRadial_ModelPlots <- renderPlot({
    req(models$svmRadial)
    plot(models$svmRadial)
  })     
  
  output$svmRadial_Recipe <- renderPrint({
    req(models$svmRadial)
    models$svmRadial$recipe
  })  
  
  output$svmRadial_ModelSummary2 <- renderPrint({
    req(models$svmRadial)
    print(models$svmRadial)
  })
  
  
  
  # 12 METHOD * gaussprPoly---------------------------------------------------------------------------------------------------------------------------
  library(kernlab)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getgaussprPolyRecipe ----
  getgaussprPolyRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$gaussprPoly_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent gaussprPoly_Go ----
  observeEvent(
    input$gaussprPoly_Go,
    {
      method <- "gaussprPoly"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        deleteRds(method)
        trcont <- getTrControl()
        trcont$seeds <- NULL
        model <- caret::train(getgaussprPolyRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = trcont, 
                              tuneLength = 15)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$gaussprPoly_Load,
    {
      method  <- "gaussprPoly"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$gaussprPoly_Delete,
    {
      models[["gaussprPoly"]] <- NULL
      gc()
    }
  )
  
  output$gaussprPoly_ModelSummary0 <- renderText({
    description("gaussprPoly")   # Use the caret method name here
  })
  
  output$gaussprPoly_Metrics <- renderTable({
    req(models$gaussprPoly)
    models$gaussprPoly$results[ which.min(models$gaussprPoly$results[, "RMSE"]), ]
  })
  
  output$gaussprPoly_ModelPlots <- renderPlot({
    req(models$gaussprPoly)
    plot(models$gaussprPoly)
  })     
  
  output$gaussprPoly_Recipe <- renderPrint({
    req(models$gaussprPoly)
    models$gaussprPoly$recipe
  })  
  
  output$gaussprPoly_ModelSummary2 <- renderPrint({
    req(models$gaussprPoly)
    summary(models$gaussprPoly$finalModel)
  })
  
  
  
  # 13 METHOD * NeuralNet ---------------------------------------------------------------------------------------------------------------------------
  library(neuralnet)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getneuralnetRecipe ----
  getneuralnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$neuralnet_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent neuralnet_Go ----
  observeEvent(
    input$neuralnet_Go,
    {
      method <- "neuralnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getneuralnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", 
                              trControl = getTrControl())  
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$neuralnet_Load,
    {
      method  <- "neuralnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$neuralnet_Delete,
    {
      models[["neuralnet"]] <- NULL
      gc()
    }
  )

  output$neuralnet_ModelSummary0 <- renderText({
    description("neuralnet")   # Use the caret method name here
  })

  output$neuralnet_Metrics <- renderTable({
    req(models$neuralnet)
    models$neuralnet$results[ which.min(models$neuralnet$results[, "RMSE"]), ]
  })

  output$neuralnet_ModelPlots <- renderPlot({
    req(models$neuralnet)
    plot(models$neuralnet)
  })
  
  output$neuralnet_Recipe <- renderPrint({
    req(models$neuralnet)
    models$neuralnet$recipe
  })  

  output$neuralnet_ModelSummary2 <- renderPrint({
    req(models$neuralnet)
    print(models$neuralnet)
  })
  
  
  
  # 14 METHOD * NNet ---------------------------------------------------------------------------------------------------------------------------
  library(nnet)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getnnetRecipe ----
  getnnetRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$nnet_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent nnet_Go ----
  observeEvent(
    input$nnet_Go,
    {
      method <- "nnet"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        nnetgrid<- expand.grid(.size=c(1,3,5),.decay=c(0.1,0.2,0.3))
        model <- caret::train(getnnetRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                             linout=TRUE, trace=FALSE, maxit=1000, tuneGrid=nnetgrid)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$nnet_Load,
    {
      method  <- "nnet"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$nnet_Delete,
    {
      models[["nnet"]] <- NULL
      gc()
    }
  )
  
  output$nnet_ModelSummary0 <- renderText({
    description("nnet")   # Use the caret method name here
  })
  
  output$nnet_Metrics <- renderTable({
    req(models$nnet)
    models$nnet$results[ which.min(models$nnet$results[, "RMSE"]), ]
  })
  
  output$nnet_ModelPlots <- renderPlot({
    req(models$nnet)
    plot(models$nnet)
  })
  
  output$nnet_Recipe <- renderPrint({
    req(models$nnet)
    models$nnet$recipe
  })  
  
  output$nnet_ModelSummary2 <- renderPrint({
    req(models$nnet)
    print(models$nnet)
  })
  
  
  
  # 15 METHOD * KNN ---------------------------------------------------------------------------------------------------------------------------
  
  # reactive getknnRecipe ----
  getknnRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$knn_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent knn_Go ----
  observeEvent(
    input$knn_Go,
    {
      method <- "knn"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getknnRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 20, na.action=na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$knn_Load,
    {
      method  <- "knn"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$knn_Delete,
    {
      models[["knn"]] <- NULL
      gc()
    }
  )

  output$knn_ModelSummary0 <- renderText({
    description("knn")   # Use the caret method name here
  })

  output$knn_Metrics <- renderTable({
    req(models$knn)
    models$knn$results[ which.min(models$knn$results[, "RMSE"]), ]
  })

  output$knn_ModelPlots <- renderPlot({
    req(models$knn)
    plot(models$knn)
  })     

  output$knn_Recipe <- renderPrint({
    req(models$knn)
    models$knn$recipe
  })  

  output$knn_ModelSummary2 <- renderPrint({
    req(models$knn)
    print(models$knn)
  })
  
  
  
  # 16 METHOD * Cubist ---------------------------------------------------------------------------------------------------------------------------
  library(Cubist)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getcubistRecipe ----
  getcubistRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$cubist_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent cubist_Go ----
  observeEvent(
    input$cubist_Go,
    {
      method <- "cubist"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getcubistRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 35, na.action=na.pass)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$cubist_Load,
    {
      method  <- "cubist"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$cubist_Delete,
    {
      models[["cubist"]] <- NULL
      gc()
    }
  )
  
  output$cubist_ModelSummary0 <- renderText({
    description("cubist")   # Use the caret method name here
  })
  
  output$cubist_Metrics <- renderTable({
    req(models$cubist)
    models$cubist$results[ which.min(models$cubist$results[, "RMSE"]), ]
  })
  
  output$cubist_ModelPlots <- renderPlot({
    req(models$cubist)
    plot(models$cubist)
  })   

  output$cubist_ModelSummary1 <- renderPrint({
    req(models$cubist)
    summary(models$cubist$finalModel)
  })
  
  output$cubist_Recipe <- renderPrint({
    req(models$cubist)
    models$cubist$recipe
  })  
  
  output$cubist_ModelSummary2 <- renderPrint({
    req(models$cubist)
    print(models$cubist)
  })
  
  
  
  # 17 METHOD * Earth ---------------------------------------------------------------------------------------------------------------------------
  library(earth)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getearthRecipe ----
  getearthRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$earth_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent earth_Go ----
  observeEvent(
    input$earth_Go,
    {
      method <- "earth"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getearthRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 20, na.action=na.fail)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$earth_Load,
    {
      method  <- "earth"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$earth_Delete,
    {
      models[["earth"]] <- NULL
      gc()
    }
  )

  output$earth_ModelSummary0 <- renderText({
    description("earth")   # Use the caret method name here
  })

  output$earth_Metrics <- renderTable({
    req(models$earth)
    models$earth$results[ which.min(models$earth$results[, "RMSE"]), ]
  })

  output$earth_ModelPlots <- renderPlot({
    req(models$earth)
    plot(models$earth)
  })     

  output$earth_Recipe <- renderPrint({
    req(models$earth)
    models$earth$recipe
  })  

  output$earth_ModelSummary2 <- renderPrint({
    req(models$earth)
    print(models$earth)
  })
  
 
  
  # 18 METHOD * pcr ---------------------------------------------------------------------------------------------------------------------------
  library(pls)  #  <------ Declare any modelling packages that are needed (see Method List tab)
  
  # reactive getpcrRecipe ----
  getpcrRecipe <- reactive({
    form <- formula(Response ~ .)
    recipes::recipe(form, data = getTrainData()) %>%
      dynamicSteps(input$pcr_Preprocess) %>%   # use <method>_Preprocess
      step_rm(has_type("date"))   # remove original date variables
  })
  
  # observeEvent pcr_Go ----
  observeEvent(
    input$pcr_Go,
    {
      method <- "pcr"
      models[[method]] <- NULL
      showNotification(id = method, paste("Processing", method, "model using resampling"), session = session, duration = NULL)
      clus <- startMode(input$Parallel)
      tryCatch({
        model <- caret::train(getpcrRecipe(), data = getTrainData(), method = method, metric = "RMSE", trControl = getTrControl(), 
                              tuneLength = 25)
        deleteRds(method)
        saveToRds(model, method)
        models[[method]] <- model
      }, 
      finally = {
        removeNotification(id = method)
        stopMode(clus)
      })
    }
  )
  
  observeEvent(
    input$pcr_Load,
    {
      method  <- "pcr"
      model <- loadRds(method, session)
      if (!is.null(model)) {
        models[[method]] <- model
      }
    }
  )
  
  observeEvent(
    input$pcr_Delete,
    {
      models[["pcr"]] <- NULL
      gc()
    }
  )
  
  output$pcr_ModelSummary0 <- renderText({
    description("pcr")   # Use the caret method name here
  })
  
  output$pcr_Metrics <- renderTable({
    req(models$pcr)
    models$pcr$results[ which.min(models$pcr$results[, "RMSE"]), ]
  })
  
  output$pcr_ModelPlots <- renderPlot({
    req(models$pcr)
    plot(models$pcr)
  })     
  
  output$pcr_Recipe <- renderPrint({
    req(models$pcr)
    models$pcr$recipe
  })  
  
  output$pcr_ModelSummary2 <- renderPrint({
    req(models$pcr)
    summary(models$pcr$finalModel)
  })
  
  output$pcr_Coef <- renderTable({
    req(models$pcr)
    co <- coef(models$pcr$finalModel)
    as.data.frame(co, row.names = rownames(co))
  }, rownames = TRUE, colnames = FALSE)
  
  # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------

})
  
  

