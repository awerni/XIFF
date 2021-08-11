mlApplyModelUI_main <- function(id){
  ns <- NS(id)
  
  div(
    fluidRow(
      column_6(plotWrapperUI(ns("plot"))),
      column_6(plotWrapperUI(ns("metrics")))
    ),
    fluidRow(containerDT(ns("table")))
  )
}

mlApplyModelUI_sidebar <- function(id){
  ns <- NS(id)
  uiOutput(ns("sidebar"))
}

mlApplyModelUI_header <- function(id){
  ns <- NS(id)
  bsAlert(ns("error"))
}

mlApplyModel <- function(input, output, session, Model, classSelection, classLabel, AnnotationFocus){
  ns <- session$ns
  
  Data <- reactive({
    m <- Model()
    req(m)
    
    cs <- reactiveValuesToList(classSelection)
    assignment <- stackClasses(cs)
    validate(need(nrow(assignment) > 0,  "goto input tab and select cell lines"))
    
    getDataForModel(
      assignment = assignment,
      features = m
    )
  })
  
  Predictions <- reactive({
    df <- Data()
    m <- Model()
    req(df, m)
    logger::log_trace("XLIFF::mlApplyModel predicting values for {nrow(df)}",
                      " observations using {class(m)[1]} model.")
    withErrorHandler(
      expr = predict(m, newdata = df %>% select(-class)),
      errorId = ns("error"),
      session = session
    )
  })
  
  Results <- reactive({
    positive_model <- input$positive_model
    positive_cs <- input$positive_cs
    req(positive_model, positive_cs)
    
    preds <- Predictions()
    req(preds)
    
    d <- Data()
    XIFF::getPredictionSummary(
      items = d[[getOption("xiff.column")]],
      preds = preds,
      refs = d$class,
      positive_model = positive_model,
      positive_cs = positive_cs,
      classes = c("positive", "negative"),
      classes_model = names(Choices_model()),
      classes_cs = names(Choices_cs()),
      annoFocus = AnnotationFocus()
    )
  })
  
  Choices_cs <- reactive({
    cl <- reactiveValuesToList(classLabel)
    structure(
      c("class1", "class2"),
      names = c(cl$class1_name, cl$class2_name)
    )
  })
  
  Choices_model <- reactive({
    m <- Model()
    validate(need(m, "load the model first"))
    structure(
      c("class1", "class2"),
      names = c(m$classLabel$class1_name, m$classLabel$class2_name)
    )
  })
  
  output$sidebar <- renderUI({
    list(
      h4("Positive class"),
      selectInput(
        inputId = ns("positive_model"),
        label = "From the model",
        choices = Choices_model(),
        selected = isolate(input$positive_model) %||% "class1"
      ),
      selectInput(
        inputId = ns("positive_cs"),
        label = "From the current classification",
        choices = Choices_cs(),
        selected = isolate(input$positive_cs) %||% "class1"
      ),
      hr(),
      textInput(
        inputId = ns("download_filename"), 
        label = "Choose file name", 
        value = "predictions"
      ),
      downloadButton(
        outputId = ns("download"), 
        label = "Download"
      )
    )
  })
  
  # Table ---------------------------------------------------------------------
  TableData <- reactive({
    res <- Results()
    req(res)
    
    name <- rlang::sym(getOption("xiff.column"))
    
    res$data %>% select(
      !!name,
      tumortype,
      predicted = predicted_original,
      reference = reference_original,
      correct
    ) %>%
      mutate_at(c("predicted", "reference"), factor)
  })
  
  output$table <- DT::renderDataTable(
    expr = TableData(),
    rownames = FALSE,
    filter = "top",
    options = list(
      pageLength = 10,
      search = list(regex = TRUE)
    ),
    escape = FALSE,
    selection = "none"
  )
  
  # Table plot ----------------------------------------------------------------
  TablePlot <- reactive({
    res <- Results()
    req(res)
    df <- prepareTablePlotData(
      df = res$data,
      positive_preds = input$positive_model, 
      positive_refs <- input$positive_cs,
      labels_preds = names(Choices_model()), 
      labels_refs = names(Choices_cs()),
      labels = c("positive", "negative")
    )
    
    XIFF::generateTablePlot(df)
  })
  
  callModule(
    plotWrapper,
    id = "plot",
    PlotExpr = TablePlot
  )
  
  # Performance plot ----------------------------------------------------------
  PerformancePlot <- reactive({
    res <- Results()
    req(res)
    df <- XIFF::getPerformanceDataFrame(res$res$table)
    XIFF::generateApplyPerformancePlot(df)
  })
  
  callModule(
    plotWrapper,
    id = "metrics",
    PlotExpr = PerformancePlot
  )
  
  # Download ------------------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() { paste0(input$download_filename, ".csv") },
    content = function(file) {
      readr::write_excel_csv(TableData(), file, na = "")
    }
  )
}
