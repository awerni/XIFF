library(shiny)

test_that(
  desc = "additionalColumns works fine",
  code = {
    testServer(
      app = additionalColumns,
      args = list(
        Table = reactive({ iris }),
        defaultCols = c("Sepal.Width", "Petal.Length")
      ),
      expr = {
        # Initial state
        expect_equal(
          object = Choices(),
          expected = c("Sepal.Length", "Petal.Width", "Species")
        )
        expect_equal(
          object = VisibleCols(),
          expected = c("Sepal.Width", "Petal.Length")
        )

        # Another column selected
        session$setInputs(showCols = "Species")
        expect_equal(
          object = VisibleCols(),
          expected = c("Sepal.Width", "Petal.Length", "Species")
        )
      }
    )
  }
)
