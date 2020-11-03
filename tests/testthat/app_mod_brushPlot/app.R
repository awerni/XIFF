library(shiny)
library(dplyr)
library(XIFF)

df <- iris %>%
  mutate(celllinename = paste(Species, seq_along(Species), sep = "_")) %>%
  select(celllinename, score = Sepal.Length, tumortype = Species) %>%
  mutate(celllinename = forcats::fct_reorder(celllinename, score, .desc = TRUE))

plotObj <- generateWaterfallPlot(
  data = df,
  dataCol = "score",
  trans = "sqrt"
)

shinyApp(
  ui = fluidPage(
    tags$head(
      shinyjs::useShinyjs(),
      useXIFF()
    ),
    brushPlotUI("test")
  ),
  server = function(input, output, session){
    Res <- callModule(
      module = brushPlot,
      id = "test",
      plotExpr = reactive({ plotObj }),
      checkExpr = reactive({ TRUE }),
      test = TRUE
    )

    exportTestValues(res = Res())
  }
)
