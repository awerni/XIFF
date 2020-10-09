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

#' @export
extendedTableDownload <- function(input, output, session, Table, Subject, Item,
                                  classSelection, classLabel, by, additional = NULL, stripCol = "higher"){
  colname <- getOption("xiff.column")

  output$downloadData <- downloadHandler (
    filename = function() { paste0(input$filename, ".txt") },
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
            col <- sym(col)
            res <- res %>% mutate(!!col := stripHtml(!!col))
          }
        }

      } else {
        cs <- reactiveValuesToList(classSelection)
        cl <- reactiveValuesToList(classLabel)
        assignment <- stackClasses(cs, cl, return_factor = FALSE)

        df <- Subject() %>%
          inner_join(as.data.frame(Item()), by = by) %>%
          inner_join(assignment, by = colname)

        nameOrder <- c(colname, "class", by, additional)
        tabSpecificNames <-  setdiff(names(df), nameOrder)
        nameOrder <- c(nameOrder, tabSpecificNames)

        res <- df[nameOrder] %>% arrange_at(c("class", tabSpecificNames))
      }

      write.table(res, file, row.names = FALSE, sep = "\t", na = "", quote = FALSE)
    }
  )
}

stripHtml <- function(x){
  gsub("<[^>]*>", "", x)
}
