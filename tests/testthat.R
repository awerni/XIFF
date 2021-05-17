library(testthat)
library(XIFF)

if (!shinytest::dependenciesInstalled()){
  shinytest::installDependencies()
}

test_check("XIFF")
