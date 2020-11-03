library(shinytest)

test_that(
  desc = "Brush plot works fine",
  code = {
    app <- ShinyDriver$new("app_mod_brushPlot")
    appState <- app$getAllValues()

    pd <- appState$export[["test-pd"]]
    expect_is(pd$data, "data.frame")
    expect_named(pd$data, c("celllinename", "score", "tumortype"))
    expect_equal(nrow(pd$data), 150)
    expect_equal(pd$xVar, "celllinename")
    expect_equal(pd$yVar, "score")
    expect_null(pd$facetInfo)
    expect_equal(pd$xTrans, "identity")
    expect_equal(pd$yTrans, "sqrt")

    app$findElement("#test-brush_options")$click()

    # Cutoff check
    # app$setInputs("test-score_method_y" = "cutoff")
    app$waitForValue(
      name = "test-cutoff_y",
      iotype = "input"
    )

    # app$setInputs("test-cutoff_action_y" = "lt")
    app$setInputs("test-cutoff_y" = 6)

    app$findElement("#test-brush_button")$click()
    app$waitForValue(
      name = "test-selectionStat",
      iotype = "output",
      ignore = appState$output[["test-selectionStat"]]
    )

    appState <- app$getAllValues()
    res <- appState$export$res
    expect_equal(res$source, "test-plot_brush")
    expect_equal(res$range_x, c(NA, NA))
    expect_equal(res$range_y, c(4.3, 5.9))
    expect_is(res$celllinename, "character")
    expect_length(res$celllinename, 83)

    label <- appState$output[["test-selectionStat"]]
    expect_equal(label, "83 cell lines selected in  Y range: (4.3, 5.9);")

    # Number check
    app$setInputs("test-score_method_y" = "number")
    app$waitForValue(
      name = "test-number_y",
      iotype = "input"
    )

    app$setInputs("test-number_action_y" = "highest")
    app$setInputs("test-number_y" = 19)

    app$findElement("#test-brush_button")$click()
    app$waitForValue(
      name = "test-selectionStat",
      iotype = "output",
      ignore = appState$output[["test-selectionStat"]]
    )

    appState <- app$getAllValues()
    res <- appState$export$res
    expect_equal(res$source, "test-plot_brush")
    expect_equal(res$range_x, c(NA, NA))
    expect_equal(res$range_y, c(6.8, 7.9))
    expect_is(res$celllinename, "character")
    expect_length(res$celllinename, 19)

    label <- appState$output[["test-selectionStat"]]
    expect_equal(label, "19 cell lines selected in  Y range: (6.8, 7.9);")

    # Percentile check
    app$setInputs("test-score_method_y" = "percentile")
    app$waitForValue(
      name = "test-percentile_y",
      iotype = "input"
    )

    app$setInputs("test-percentile_action_y" = "highest")
    app$setInputs("test-percentile_y" = 30)

    app$findElement("#test-brush_button")$click()
    app$waitForValue(
      name = "test-selectionStat",
      iotype = "output",
      ignore = appState$output[["test-selectionStat"]]
    )

    appState <- app$getAllValues()
    res <- appState$export$res
    expect_equal(res$source, "test-plot_brush")
    expect_equal(res$range_x, c(NA, NA))
    expect_equal(res$range_y, c(6.3, 7.9))
    expect_is(res$celllinename, "character")
    expect_length(res$celllinename, 45)

    label <- appState$output[["test-selectionStat"]]
    expect_equal(label, "45 cell lines selected in  Y range: (6.3, 7.9);")
  }
)
