shinyUI(fluidPage(
  
  # Application title
  titlePanel("Shiny - ModelsSelection"),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput(outputId = "DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput(inputId = "Multiplier", label = "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput(outputId = "BoxPlots"),
             plotOutput(outputId = "Missing"),
             plotOutput(outputId = "Corr"),
             plotOutput(outputId = 'Pairs'),
             DT::dataTableOutput(outputId = "Table")
    ), 
    tabPanel("Split",
             sliderInput(inputId = "Split", label = "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput(outputId = "SplitSummary")
    ),
    tabPanel("Available methods",
             h3("Regression methods in caret"),
             shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "Available"))
    ),
    tabPanel("Methods",
             checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = FALSE),
             bsTooltip(id = "Parallel", title = "Turn off parallel processing to view any training errors in the console"),
             "The preprocessing steps and their order are important.",
             HTML("See function <code>dynamicSteps</code> in global.R for interpretation of preprocessing options. "),
             "Documentation", tags$a("here", href = "https://www.rdocumentation.org/packages/recipes/versions/0.1.16", target = "_blank"),
             
             tabsetPanel(type = "pills",
                         tabPanel("NULL Model",
                                  br(),
                                  fluidRow(
                                    column(width = 4),
                                    column(width = 1,
                                           actionButton(inputId = "null_Go", label = "Train", icon = icon("play")),
                                           bsTooltip(id = "null_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "null_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "null_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "null_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "null_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "null_Recipe")
                         ),
                         tabPanel("1 GLMnet Model",
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "glmnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(glmnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = glmnet_initial),  # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "glmnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")

                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "glmnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "glmnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "glmnet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "glmnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "glmnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "glmnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "glmnet_Recipe"),
                                  verbatimTextOutput(outputId = "glmnet_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "glmnet_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("2 PLS Model",
                                  verbatimTextOutput(outputId = "pls_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pls_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pls_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pls_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pls_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pls_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "pls_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pls_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "pls_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pls_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pls_ModelPlots"),
                                  verbatimTextOutput(outputId = "pls_Recipe"),
                                  verbatimTextOutput(outputId = "pls_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "pls_Coef")  #  <- typically this is specific to OLS
                                  )
                         ),
                         tabPanel("3 Rpart Model",
                                  verbatimTextOutput(outputId = "rpart_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models                                 selectizeInput(inputId = "rpart_Preprocess",
                                           selectizeInput(inputId = "rpart_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rpart_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rpart_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "rpart_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "rpart_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "rpart_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rpart_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "rpart_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rpart_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "rpart_ModelPlots"),
                                  verbatimTextOutput(outputId = "rpart_Recipe"),
                                  plotOutput(outputId = "rpart_ModelTree")   #  <- this tree-plot is unique to the rpart method
                         ),
                         
                         
                         # maintenance point ------------------------------------------------------------------------------
                         # add further tabs (with controls) here
                         tabPanel("4 LM Model",
                                  verbatimTextOutput(outputId = "lm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4, 
                                           selectizeInput(inputId = "lm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(lm_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = lm_initial),  
                                           bsTooltip(id = "lm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Go", label = "Train", icon = icon("play")),  
                                           bsTooltip(id = "lm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "lm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lm_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "lm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "lm_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "lm_ModelPlots"),
                                  verbatimTextOutput(outputId = "lm_Recipe"),
                                  verbatimTextOutput(outputId = "lm_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "lm_Coef")    
                                  )
                         ),
                         tabPanel("5 LmStepAIC Model",
                                  verbatimTextOutput(outputId = "lmStepAIC_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4, 
                                           selectizeInput(inputId = "lmStepAIC_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(lmStepAIC_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = lmStepAIC_initial),  
                                           bsTooltip(id = "lmStepAIC_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lmStepAIC_Go", label = "Train", icon = icon("play")),  
                                           bsTooltip(id = "lmStepAIC_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lmStepAIC_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "lmStepAIC_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "lmStepAIC_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "lmStepAIC_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "lmStepAIC_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "lmStepAIC_ModelPlots"),
                                  verbatimTextOutput(outputId = "lmStepAIC_Recipe"),
                                  verbatimTextOutput(outputId = "lmStepAIC_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "lmStepAIC_Coef")    
                                  )
                         ),
                         tabPanel("6 RLM Model",
                                  verbatimTextOutput(outputId = "rlm_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(inputId = "rlm_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(rlm_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = rlm_initial),  
                                           bsTooltip(id = "rlm_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rlm_Go", label = "Train", icon = icon("play")),  
                                           bsTooltip(id = "rlm_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rlm_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "rlm_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "rlm_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "rlm_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "rlm_Metrics"),
                                  hr(),
                                  verbatimTextOutput(outputId = "rlm_Recipe"),
                                  verbatimTextOutput(outputId = "rlm_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "rlm_Coef")    
                                  )
                         ),
                         tabPanel("7 M5 Model",
                                  verbatimTextOutput(outputId = "M5_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(inputId = "M5_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(M5_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = M5_initial),  
                                           bsTooltip(id = "M5_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "M5_Go", label = "Train", icon = icon("play")),  
                                           bsTooltip(id = "M5_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "M5_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "M5_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "M5_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "M5_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "M5_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "M5_ModelPlots"),
                                  verbatimTextOutput(outputId = "M5_Recipe"),
                                  verbatimTextOutput(outputId = "M5_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    verbatimTextOutput(outputId = "M5_Coef")   
                                  )
                         ),
                         tabPanel("8 BstTree Model",
                                  verbatimTextOutput(outputId = "bstTree_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(inputId = "bstTree_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(bstTree_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = bstTree_initial),  
                                           bsTooltip(id = "bstTree_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bstTree_Go", label = "Train", icon = icon("play")), 
                                           bsTooltip(id = "bstTree_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bstTree_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "bstTree_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "bstTree_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "bstTree_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "bstTree_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "bstTree_ModelPlots"),
                                  verbatimTextOutput(outputId = "bstTree_Recipe"),
                                  verbatimTextOutput(outputId = "bstTree_ModelSummary2"),
                                  verbatimTextOutput(outputId = "bstTree_ModelTree")
                         ),
                         tabPanel("9 QRF Model",
                                  verbatimTextOutput(outputId = "qrf_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(inputId = "qrf_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(qrf_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = qrf_initial), 
                                           bsTooltip(id = "qrf_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "qrf_Go", label = "Train", icon = icon("play")),  
                                           bsTooltip(id = "qrf_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "qrf_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "gqrf_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "qrf_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "qrf_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "qrf_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "qrf_ModelPlots"),
                                  plotOutput(outputId = "qrf_Modelimportance"),
                                  verbatimTextOutput(outputId = "qrf_Recipe"),
                                  verbatimTextOutput(outputId = "qrf_ModelSummary2")
                         ),
                         tabPanel("10 SVMLinear Model",
                                  verbatimTextOutput(outputId = "svmLinear_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(inputId = "svmLinear_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmLinear_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmLinear_initial),  
                                           bsTooltip(id = "svmLinear_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Go", label = "Train", icon = icon("play")),  
                                           bsTooltip(id = "svmLinear_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "svmLinear_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmLinear_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "svmLinear_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmlinear_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "svmLinear_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmLinear_Recipe"),
                                  verbatimTextOutput(outputId = "svmLinear_ModelSummary2")
                         ),
                         tabPanel("11 SVMRadial Model",
                                  verbatimTextOutput(outputId = "svmRadial_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(inputId = "svmRadial_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(svmRadial_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = svmRadial_initial),  
                                           bsTooltip(id = "svmRadial_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmRadial_Go", label = "Train", icon = icon("play")),  
                                           bsTooltip(id = "svmRadial_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmRadial_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "svmRadial_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "svmRadial_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "svmRadial_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "svmRadial_Metrics"),             
                                  hr(),
                                  plotOutput(outputId = "svmRadial_ModelPlots"),
                                  verbatimTextOutput(outputId = "svmRadial_Recipe"),
                                  verbatimTextOutput(outputId = "svmRadial_ModelSummary2")
                         ),
                         
                         tabPanel("12 gaussprPoly Model",
                                  verbatimTextOutput(outputId = "gaussprPoly_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "gaussprPoly_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(gaussprPoly_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = gaussprPoly_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "gaussprPoly_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprPoly_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "gaussprPoly_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprPoly_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "gaussprPoly_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "gaussprPoly_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "gaussprPoly_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "gaussprPoly_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "gaussprPoly_ModelPlots"),
                                  verbatimTextOutput(outputId = "gaussprPoly_Recipe"),
                                  verbatimTextOutput(outputId = "gaussprPoly_ModelSummary2")
                         ),
                         tabPanel("13 NeuralNet Model",
                                  verbatimTextOutput(outputId = "neuralnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(inputId = "neuralnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(neuralnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = neuralnet_initial), 
                                           bsTooltip(id = "neuralnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "neuralnet_Go", label = "Train", icon = icon("play")),  
                                           bsTooltip(id = "neuralnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "neuralnet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "neuralnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "neuralnet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "neuralnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "neuralnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "neuralnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "neuralnet_Recipe"),
                                  verbatimTextOutput(outputId = "neuralnet_ModelSummary2")
                         ),
                         tabPanel("14 NNet Model",
                                  verbatimTextOutput(outputId = "nnet_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "nnet_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(nnet_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = nnet_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "nnet_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnet_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "nnet_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnet_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "nnet_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "nnet_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "nnet_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "nnet_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "nnet_ModelPlots"),
                                  verbatimTextOutput(outputId = "nnet_Recipe"),
                                  verbatimTextOutput(outputId = "nnet_ModelSummary2")
                         ),
                         tabPanel("15 KNN Model",
                                  verbatimTextOutput(outputId = "knn_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(inputId = "knn_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(knn_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = knn_initial),  
                                           bsTooltip(id = "knn_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "knn_Go", label = "Train", icon = icon("play")),  
                                           bsTooltip(id = "knn_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "knn_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "knn_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "knn_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "knn_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "knn_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "knn_ModelPlots"),
                                  verbatimTextOutput(outputId = "knn_Recipe"),
                                  verbatimTextOutput(outputId = "knn_ModelSummary2")
                         ),
                         tabPanel("16 Cubist Model",
                                  verbatimTextOutput(outputId = "cubist_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(inputId = "cubist_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(cubist_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = cubist_initial),  
                                           bsTooltip(id = "cubist_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Go", label = "Train", icon = icon("play")),  
                                           bsTooltip(id = "cubist_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "cubist_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "cubist_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "cubist_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "cubist_Metrics"),
                                  hr(),
                                  shinycssloaders::withSpinner(plotOutput(outputId = "cubist_ModelPlots")),
                                  verbatimTextOutput(outputId = "cubist_ModelSummary1"), 
                                  verbatimTextOutput(outputId = "cubist_Recipe"),
                                  verbatimTextOutput(outputId = "cubist_ModelSummary2")
                         ),
                         tabPanel("17 Earth Model",
                                  verbatimTextOutput(outputId = "earth_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           selectizeInput(inputId = "earth_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(earth_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = earth_initial),  
                                           bsTooltip(id = "earth_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "earth_Go", label = "Train", icon = icon("play")),  
                                           bsTooltip(id = "earth_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "earth_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "earth_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "earth_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "earth_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "earth_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "earth_ModelPlots"),
                                  verbatimTextOutput(outputId = "earth_Recipe"),
                                  verbatimTextOutput(outputId = "earth_ModelSummary2")
                         ),
                         tabPanel("18 PCR Model",
                                  verbatimTextOutput(outputId = "pcr_ModelSummary0"),
                                  fluidRow(
                                    column(width = 4,
                                           # The id of the recipe preprocessing steps control MUST be:  "<method>_Preprocess" in order to correctly load from the saved models
                                           selectizeInput(inputId = "pcr_Preprocess",
                                                          label = "Pre-processing",
                                                          choices = unique(c(pcr_initial, ppchoices)),
                                                          multiple = TRUE,
                                                          selected = pcr_initial), # <-- These are suggested starting values. Set these to your best recommendation
                                           bsTooltip(id = "pcr_Preprocess", title = "These entries will be populated in the correct order from a saved model once it loads", placement = "top")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pcr_Go", label = "Train", icon = icon("play")), # name this control <method>_Go
                                           bsTooltip(id = "pcr_Go", title = "This will train or retrain your model (and save it)")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pcr_Load", label = "Load", icon = icon("file-upload")),
                                           bsTooltip(id = "pcr_Load", title = "This will reload your saved model")
                                    ),
                                    column(width = 1,
                                           actionButton(inputId = "pcr_Delete", label = "Forget", icon = icon("trash-alt")),
                                           bsTooltip(id = "pcr_Delete", title = "This will remove your model from memory")
                                    )
                                  ),
                                  hr(),
                                  h3("Resampled performance:"),
                                  tableOutput(outputId = "pcr_Metrics"),
                                  hr(),
                                  plotOutput(outputId = "pcr_ModelPlots"),
                                  verbatimTextOutput(outputId = "pcr_Recipe"),
                                  verbatimTextOutput(outputId = "pcr_ModelSummary2"),
                                  wellPanel(
                                    h3("Coefficients"),
                                    tableOutput(outputId = "pcr_Coef")  #  <- typically this is specific to OLS
                                  )
                         )

                      
                         # end of maintenance point ---------------------------------------------------------------------------------------------------------------------------
             )
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput(inputId = "Notch", label = "Show notch", value = FALSE),
             checkboxInput(inputId = "NullNormalise", label = "Normalise", value = TRUE),
             checkboxInput(inputId = "HideWorse", label = "Hide models worse than null model", value = TRUE),
             plotOutput(outputId = "SelectionBoxPlot"),
             radioButtons(inputId = "Choice", label = "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput(outputId = "Title"),
             verbatimTextOutput(outputId = "TestSummary"),
             fluidRow(
               column(offset = 2, width = 4,
                      plotOutput(outputId = "TestPlot")
               ),
               column(width = 2,
                      plotOutput(outputId = "TestResiduals")
               ),
               column(width = 2,
                      plotOutput(outputId = "TrainResiduals"),
               )
             ),
             sliderInput(inputId = "IqrM", label = "IQR multiplier", min = 0, max = 5, value = 1.5, step = 0.1),
    )
  )
))
