#' Additional Columns module
#'
#' @param id shiny id
#' @param Table 
#' @param defaultCols 
#' @param maxAdditionalCols 
#' @param ... 
#'
#' @export
#' @rdname additionalColumns
#'
#' @examples
#' 
additionalColumnsUI_main <- function(id, style = "font-size:80%"){
  ns <- NS(id)
  div(
    style = style,
    DT::dataTableOutput(ns("table"))
  )
}

#' @export
#' @rdname additionalColumns
additionalColumnsUI_sidebar <- function(id){
  ns <- NS(id)

  div(
    class = "column-picker",
    uiOutput(ns("picker"))
  )
}

#' @export
#' @rdname additionalColumns
additionalColumns <- function(id, Table, defaultCols = NULL, maxAdditionalCols = 5, ...){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    theDots <- list(...)
    if ("options" %in% names(theDots)){
      theDots[["options"]][["stateSave"]] <- TRUE
    }

    Defaults <- reactiveVal()
    SelectedCols <- reactiveVal()
    ColumnState <- reactiveVal()

    observeEvent(
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

    observeEvent(
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

    Choices <- reactive({
      tab <- Table()
      req(tab)
      tabNames <- names(tab)

      if (is.null(defaultCols)) {
        Defaults(head(tabNames, 3))
      } else {
        Defaults(defaultCols)
      }

      setdiff(tabNames, Defaults())
    })

    output$picker <- renderUI({
      shinyWidgets::pickerInput(
        inputId = ns("showCols"),
        label = NULL,
        choices = Choices(),
        selected = isolate(SelectedCols()),
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

    VisibleCols <- reactive({
      tab <- Table()
      req(tab)
      visibleCols <- c(Defaults(), SelectedCols())
      intersect(names(tab), visibleCols)
    })

    output$table <- DT::renderDataTable({
      tab <- Table()
      validate(need(tab, "no data available"))
      vc <- VisibleCols()

      currentColState <- isolate(ColumnState())

      if (length(currentColState) > 0){
        # prepare empty state per each column
        colState <- rep(list(NULL), nrow(tab))
        names(colState) <- names(tab)

        colState[names(currentColState)] <- currentColState # include current state
        theDots[["options"]][["searchCols"]] <- unname(colState[vc]) # use only visible columns and keep order of columns
      }

      theDots[["data"]] <- tab[vc]
      do.call(DT::datatable, theDots)
    })

    observeEvent(
      eventExpr = input$table_state,
      handlerExpr = {
        currentState <- lapply(
          X = input$table_state$columns,
          FUN = function(x) x$search
        )
        names(currentState) <- VisibleCols()
        ColumnState(currentState)
      }
    )

    proxy <- DT::dataTableProxy("table")
    SelectedRows <- reactive({ input$table_rows_selected })
    AllRows <- reactive({ input$table_rows_all })

    list(
      proxy = proxy,
      SelectedRows = SelectedRows,
      AllRows = AllRows
    )
  })
}
