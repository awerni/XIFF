library(shinytest2)

test_that(
  desc = "Brush plot works fine",
  code = {
    app <- AppDriver$new(
      app_dir = "app_mod_brushPlot",
      load_timeout = 10000
    )
    appState <- app$get_values()

    pd <- appState$export[["test-pd"]]
    expect_is(pd$data, "data.frame")
    expect_named(pd$data, c("celllinename", "score", "tumortype"))
    expect_equal(nrow(pd$data), 150)
    expect_equal(pd$xVar, "celllinename")
    expect_equal(pd$yVar, "score")
    expect_null(pd$facetInfo)
    expect_equal(pd$xTrans, "identity")
    expect_equal(pd$yTrans, "sqrt")
    
    app$click(selector = "#test-brush_options")

    # Cutoff check
    # app$set_inputs("test-score_method_y" = "cutoff")
    app$wait_for_value(input = "test-cutoff_y")

    # app$set_inputs("test-cutoff_action_y" = "lt")
    app$set_inputs("test-cutoff_y" = 6)

    app$click(selector = "#test-brush_button")
    app$wait_for_value(
      output = "test-selectionStat",
      ignore = appState$output[["test-selectionStat"]]
    )
    
    appState <- app$get_values()

    res <- appState$export$res
    expect_equal(res$source, "test-plot_brush")
    expect_equal(res$range_x, c(NA, NA))
    expect_equal(res$range_y, c(4.3, 5.9))
    expect_is(res$celllinename, "character")
    expect_length(res$celllinename, 83)

    label <- appState$output[["test-selectionStat"]]
    expect_equal(label, "83 cell lines selected in  Y range: (4.3, 5.9);")

    # Number check
    app$set_inputs("test-score_method_y" = "number")
    app$wait_for_value(input = "test-number_y")

    app$set_inputs("test-number_action_y" = "highest")
    app$set_inputs("test-number_y" = 19)

    app$click(selector = "#test-brush_button")
    app$wait_for_value(
      output = "test-selectionStat",
      ignore = appState$output[["test-selectionStat"]]
    )

    appState <- app$get_values()
    res <- appState$export$res
    expect_equal(res$source, "test-plot_brush")
    expect_equal(res$range_x, c(NA, NA))
    expect_equal(res$range_y, c(6.8, 7.9))
    expect_is(res$celllinename, "character")
    expect_length(res$celllinename, 19)

    label <- appState$output[["test-selectionStat"]]
    expect_equal(label, "19 cell lines selected in  Y range: (6.8, 7.9);")

    # Percentile check
    app$set_inputs("test-score_method_y" = "percentile")
    app$wait_for_value(input = "test-percentile_y")

    app$set_inputs("test-percentile_action_y" = "highest")
    app$set_inputs("test-percentile_y" = 30)

    app$click(selector = "#test-brush_button")
    app$wait_for_value(
      output = "test-selectionStat",
      ignore = appState$output[["test-selectionStat"]]
    )

    appState <- app$get_values()
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
