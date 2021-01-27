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

    theDots <- list(...)
    if ("options" %in% names(theDots)){
      theDots[["options"]][["stateSave"]] <- TRUE
    }

    Defaults <- shiny::reactiveVal()
    SelectedCols <- shiny::reactiveVal()
    ColumnState <- shiny::reactiveVal()

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

    VisibleCols <- shiny::reactive({
      tab <- Table()
      visibleCols <- c(Defaults(), SelectedCols())
      intersect(names(tab), visibleCols)
    })

    output$table <- DT::renderDataTable({
      tab <- Table()
      vc <- VisibleCols()

      currentColState <- shiny::isolate(ColumnState())

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

    shiny::observeEvent(
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
    SelectedRows <- shiny::reactive({ input$table_rows_selected })
    AllRows <- shiny::reactive({ input$table_rows_all })

    list(
      proxy = proxy,
      SelectedRows = SelectedRows,
      AllRows = AllRows
    )
  })
}
