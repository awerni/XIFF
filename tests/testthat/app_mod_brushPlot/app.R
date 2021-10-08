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

customjs <- readLines(system.file("www/common.js", package = "XIFF"))
startCustomJs <- grep(customjs, pattern = "//// BRUSH PLOT FUNCTIONS START")
endCustomJs <- grep(customjs, pattern = "//// BRUSH PLOT FUNCTIONS END")
customjs <- paste(customjs[startCustomJs:endCustomJs], collapse = "\n")

shinyApp(
  ui = fluidPage(
    tags$head(
      shinyjs::useShinyjs(),
      useXIFF(),
      # NOTE: sometimes phantomJS does not include js files because of
      # unknown reasons (some JS code seems to not be supported by it
      # see https://github.com/mdlama/reprex-shinytest-js-inclusion-bug).
      # Fortunately, for this test only brush plot's functions are required
      # and they works. However, to make that script work, we need to 
      # include these functions separately.
      # So, definitely - this is a hack!
      tags$script(HTML(customjs))
    ),
    column_8(brushPlotUI("test"))
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
