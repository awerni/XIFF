#' @export
additionalColumnsUI_main <- function(id, style = "font-size:80%"){
  ns <- shiny::NS(id)
  shiny::div(
    style = style,
    DT::dataTableOutput(ns("table"))
  )
}

#' @export
additionalColumnsUI_sidebar <- function(id){
  ns <- shiny::NS(id)

  shiny::div(
    class = "column-picker",
    shiny::uiOutput(ns("picker"))
  )
}

#' @export
additionalColumns <- function(id, Table, defaultCols = NULL, maxAdditionalCols = 5, ...){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    Defaults <- shiny::reactiveVal()
    SelectedCols <- shiny::reactiveVal()
    
    shiny::observeEvent(
      eventExpr = Table(),
      handlerExpr = {
        # Reset settings on table change
        SelectedCols(NULL)
        shinyWidgets::updatePickerInput(
          session = session, 
          inputId = "showCols",
          selected = NULL
        )
      },
      ignoreNULL = FALSE
    )
    
    shiny::observeEvent(
      eventExpr = input$showCols,
      handlerExpr = {
        old <- SelectedCols()
        new <- input$showCols
        
        if (isDifferent(old, new)){
          SelectedCols(input$showCols)
        }
      },
      ignoreNULL = FALSE
    )
    
    Choices <- shiny::reactive({
      tab <- Table()
      tabNames <- names(tab)
      
      if (is.null(defaultCols)) {
        Defaults(head(tabNames, 3))
      } else {
        Defaults(defaultCols)
      }
      
      setdiff(tabNames, Defaults())
    })

    output$picker <- shiny::renderUI({
      shinyWidgets::pickerInput(
        inputId = ns("showCols"),
        label = NULL,
        choices = Choices(),
        selected = shiny::isolate(SelectedCols()),
        width = "100%",
        options = list(
          `actions-box` = FALSE,
          `live-search` = TRUE,
          `none-selected-text` = "Show/hide additional columns",
          `max-options` = maxAdditionalCols
        ),
        multiple = TRUE
      )
    })

    TableExpr <- shiny::reactive({
      tab <- Table()
      visibleCols <- c(Defaults(), SelectedCols())
      visibleCols <- intersect(names(tab), visibleCols)
      
      tab[visibleCols]
    })

    output$table <- DT::renderDataTable(
      expr = TableExpr(),
      ...
    )

    proxy <- DT::dataTableProxy("table")
    SelectedRows <- shiny::reactive({ input$table_rows_selected })
    AllRows <- shiny::reactive({ input$table_rows_all })

    list(
      proxy = proxy,
      SelectedRows = SelectedRows,
      AllRows = AllRows
    )
  })
}
