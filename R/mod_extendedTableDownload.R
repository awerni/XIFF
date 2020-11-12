#' @export
extendedTableDownloadUI <- function(id, label, filename = label){
  ns <- shiny::NS(id)
  colname <- getOption("xiff.column")

  choices <- structure(
    c("result_table", colname),
    names = c("result table", paste(label, "<->",  getOption("xiff.label")))
  )

  shiny::tagList(
    shiny::radioButtons(
      inputId = ns("downloadType"),
      label = "Download content",
      choices = choices,
      selected = "result_table"
    ),
    shiny::textInput(
      inputId = ns("filename"),
      label = "Choose file name",
      value = paste0(filename, "_result")
    ),
    shiny::downloadButton(
      outputId = ns("downloadData"),
      label = "Download"
    )
  )
}

#' @export
extendedTableDownload <- function(input, output, session, Table, Subject, Item,
                                  classSelection, classLabel, by, additional = NULL, stripCol = c("higher", "location")){
  colname <- getOption("xiff.column")

  output$downloadData <- shiny::downloadHandler (
    filename = function() { paste0(input$filename, ".csv") },
    content = function(file) {
      tab <- tryCatch(
        expr = Table(),
        error = function(e) NULL
      )

      if (is.null(tab)){ # no results available
        res <- data.frame()
      } else if (input$downloadType == "result_table") {
        res <- tab

        for (col in stripCol){
          if (col %in% names(res)){
            col <- rlang::sym(col)
            res <- res %>% dplyr::mutate(!!col := stripHtml(!!col))
          }
        }

      } else {
        cs <- shiny::reactiveValuesToList(classSelection)
        cl <- shiny::reactiveValuesToList(classLabel)
        assignment <- stackClasses(cs, cl, return_factor = FALSE)

        df <- Subject() %>%
          dplyr::inner_join(tibble::as_tibble(Item()), by = by) %>%
          dplyr::inner_join(assignment, by = colname)

        nameOrder <- c(colname, "class", by, additional)
        tabSpecificNames <- setdiff(names(df), nameOrder)
        nameOrder <- c(nameOrder, tabSpecificNames)

        res <- df[nameOrder] %>% dplyr::arrange_at(c("class", tabSpecificNames))
      }
      
      readr::write_excel_csv(res, file, na = "")
    }
  )
}

stripHtml <- function(x){
  gsub("<[^>]*>", "", x)
}
