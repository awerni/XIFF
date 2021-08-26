getDataCommonById <- function(sql, cs = NULL, conditionSql = NULL, 
                                      idCol = NULL, callback = NULL){
  if (is.list(cs)){
    cs <- c(cs$class1, cs$class2)
  }
  
  clSql <- if (length(cs) > 0){
    paste0("WHERE ", getSQL_filter(getOption("xiff.column"), cs))
  }
  
  if (!is.null(conditionSql)){
    conditionSql <- paste("AND", conditionSql)
  }
  
  fullSql <- paste(sql, clSql, conditionSql)
  df <- getPostgresql(fullSql)
  
  if (is.function(callback)){
    df <- callback(df)
  }
  
  df
}

getDataGeneExpressionById <- function(ensg,
                                      cs,
                                      schema = getOption("xiff.schema"),
                                      column = getOption("xiff.column")) {
  
  sql <- glue::glue(
    "SELECT {column}, ensg, 2^log2tpm AS tpm ",
    "FROM {schema}.processedrnaseqview"
  )
  
  getDataCommonById(
    sql = sql,
    cs = cs,
    conditionSql = prepareConditionSql(ensg = ensg)
  )
}

machineLearningCreateModelTabUI_main <- function(id){
  ns <- NS(id)
  tabLayoutUI_main(ns("tab"))
}

#' Title
#'
#' @param id
#' @param input 
#' @param output 
#' @param session 
#' @param fm 
#' @param classSelection 
#' @param classLabel 
#' @param gsea_data_hallmark 
#' @param gene_anno 
#' @param AnnotationFocus 
#' @param Species 
#'
#' @rdname machineLearningCreateModelTab
#' @noMd
#'
#' @examples
#' 
machineLearningCreateModelTabUI_sidebar <- function(id){
  ns <- NS(id)
  
  ret <- div(
    selectInput(
      inputId = ns("gene_set"), 
      label = "Hallmark gene set:", 
      choices = NULL,
      selectize = FALSE
    ),
    selectInput(
      input = ns("method"),
      label = "Method",
      choices = XIFF::xiffSupportedModels(),
      selected = XIFF::xiffSupportedModels()[1]
    ),
    hr(),
    tabLayoutUI_sidebar(
      id = ns("tab"),
      defaults = list(
        left = "performance",
        middle = "point",
        right = "violin"
      ),
      additionalChoices = c(
        "Performance" = "performance",
        "Variable importance" = "varimp",
        "Error rates" = "errplot"
      ),
      hidden = "coverage"
    ),
    hr(),
    sliderInput(
      inputId = ns("validation_size"),
      label = "% data for validation set",
      min = 0,
      max = 50,
      value = 20
    ),
    hr(),
    textInput(
      inputId = ns("filename"), 
      label = "Choose file name", 
      value = "ml_model"
    ),
    downloadButton(
      outputId = ns("download"),
      label = "Download final model"
    )
  )
  
  div(
    uiOutput(ns("sidebar_run")),
    hr(),
    ret,
    uiOutput(ns("indicator"))
  )
}

#' @rdname machineLearningCreateModelTab
machineLearningCreateModelTab <- function(input, output, session, fm, classSelection, classLabel, 
                                          gsea_data_hallmark, gene_anno, AnnotationFocus, Species){
  # Sidebar --------------------------------------------------------------------
  ns <- session$ns
  output$sidebar_run <- renderUI({
    fmButtonOneClass(ns("run"), fm, classSelection)
  })
  
  output$indicator <- renderUI({
    hallmark_names <- names(gsea_data_hallmark())
    
    updateSelectInput(
      session = session,
      inputId = "gene_set",
      choices = getHallmarkGeneSetChoices(sort(hallmark_names)),
      selected = isolate(input$gene_set) %||% "HALLMARK_APOPTOSIS"
    )
    
    NULL
  })
  
  # FutureManager -------------------------------------------------------------
  Results <- reactiveVal()
  Args <- reactive({
    req(input$gene_set)
    
    list(
      cs = reactiveValuesToList(classSelection),
      geneSet = gsea_data_hallmark()[[input$gene_set]],
      method = input$method,
      geneAnno = gene_anno(),
      species = Species(),
      classLabel = reactiveValuesToList(classLabel),
      p_validation = input$validation_size / 100
    )
  })
  
  fm$registerRunObserver(
    inputId = ns("run"),
    label = "Machine Learning",
    statusVar = Results,
    longFun = XIFF::buildMachineLearning,
    Args = Args
  )
  
  # Reactives -----------------------------------------------------------------
  CurrentSpecies <- reactiveVal()
  
  ExpressionGene <- reactive({
    s <- SelectedRow()
    validate(need(s, "no gene selected"))
    names <- c("ensg", "symbol")
    names <- intersect(names, colnames(Results()$df))
    as.list(Results()$df[s, names, drop = FALSE])
  })
  
  Tpm <- reactive({
    
    res <- Results()
    FutureManager::fmValidate(res)
    mlGetTpmData(
      model = res[["value"]],
      ensg = ExpressionGene()$ensg,
      annoFocus = isolate(AnnotationFocus()))
    
  })
  
  # Layout --------------------------------------------------------------------
  plotFun <- function(plotType){
    res <- Results()
    FutureManager::fmValidate(res)
    
    res <- res[["value"]]
    cs <- mlModelSet2ClassSelectionList(res)
    cl <- reactiveValuesToList(classLabel)
    switch(
      EXPR = plotType,
      "varimp" = XIFF::generateVarImpPlot(res),
      "performance" = XIFF::generatePerformancePlot(res),
      "errplot" = XIFF::generateErrorPlot(res, cl),
      {
        gene <- ExpressionGene()
        mlGenerateExpressionPlot(
          res,
          df = Tpm(), 
          ca = makeClassAssignment(cs, cl), 
          plotType = plotType,
          gene = gene
        )
      }
      
    )
  }
  
  TableData <- reactive({
    res <- Results()
    FutureManager::fmValidate(res)
    CurrentSpecies(res[["value"]]$species)
    mlGetTableData(res[["value"]])
  })
  
  RowCallback <- reactive({
    sp <- CurrentSpecies()
    req(sp)
    res <- Results()
    FutureManager::fmValidate(res)
    
    idx <- if(!inherits(res[["value"]], "XiffGREP")) {
      1 # ensg is in first column
    } else {
      3:4 # GREP has two ensg columns
    }
    getEnsgRowCallback(sp, idx = idx)
  })
  
  rowInfo <- callModule(
    module = tabLayout,
    id = "tab",
    plotFun = plotFun, 
    TableData = TableData,
    jsRowCallback = RowCallback
  )
  SelectedRow <- rowInfo$SelectedRow
  
  # Download ------------------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() { 
      paste0(input$filename, ".rds") 
    },
    content = function(file){
      model <- Results()[["value"]]
      newClassLabel(model) <- reactiveValuesToList(classLabel)
      
      saveRDS(
        object = model, 
        file = file
      )
    }
  )
  
  Results
}
