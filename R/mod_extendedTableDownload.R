#' @rdname extendedTableDownload
#' @export
extendedTableDownloadUI <- function(id, label, filename = label){
  ns <- NS(id)
  colname <- getOption("xiff.column")

  choices <- structure(
    c("result_table", colname),
    names = c("result table", paste(label, "<->",  getOption("xiff.label")))
  )

  tagList(
    radioButtons(
      inputId = ns("downloadType"),
      label = "Download content",
      choices = choices,
      selected = "result_table"
    ),
    textInput(
      inputId = ns("filename"),
      label = "Choose file name",
      value = paste0(filename, "_result")
    ),
    downloadButton(
      outputId = ns("downloadData"),
      label = "Download"
    )
  )
}


#' Module for downloading tables as csv files.
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#' @param Table 
#' @param Subject 
#' @param Item 
#' @param classSelection 
#' @param classLabel 
#' @param by 
#' @param additional 
#' 
#' @rdname extendedTableDownload
#' @export
#'
extendedTableDownload <- function(input, output, session, Table, Subject, Item,
                                  classSelection, classLabel, by, additional = NULL){
  colname <- getOption("xiff.column")

  output$downloadData <- downloadHandler (
    filename = function() { paste0(input$filename, ".csv") },
    content = function(file) {
      tab <- tryCatch(
        expr = Table(),
        error = function(e) NULL
      )

      if (is.null(tab)){ # no results available
        res <- data.frame()
      } else if (input$downloadType == "result_table") {
        res <- tab %>% mutate_all(stripHtml)
      } else {
        cs <- reactiveValuesToList(classSelection)
        cl <- reactiveValuesToList(classLabel)
        assignment <- stackClasses(cs, cl, return_factor = FALSE)

        df <- Subject() %>%
          inner_join(tibble::as_tibble(Item()), by = by) %>%
          inner_join(assignment, by = colname)

        nameOrder <- c(colname, "class", by, additional)
        tabSpecificNames <- setdiff(names(df), nameOrder)
        nameOrder <- c(nameOrder, tabSpecificNames)

        res <- df[nameOrder] %>% arrange_at(c("class", tabSpecificNames))
      }

      readr::write_excel_csv(res, file, na = "")
    }
  )
}

stripHtml <- function(x){
  gsub("<[^>]*>", "", x)
}
