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
  ns <- NS(id)

  shiny::div(
    class = "column-picker",
    shiny::uiOutput(ns("picker"))
  )
}

#' @export
additionalColumns <- function(id, Table, defaultCols, maxAdditionalCols = 5, ...){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    Choices <- shiny::reactive({
      setdiff(names(Table()), defaultCols)
    })

    output$picker <- shiny::renderUI({
      shinyWidgets::pickerInput(
        inputId = ns("showCols"),
        label = NULL,
        choices = Choices(),
        selected = shiny::isolate(input$showCols),
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
      visibleCols <- c(defaultCols, input$showCols)
      Table()[visibleCols]
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
