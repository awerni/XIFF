library(testthat)
library(XIFF)

if (!shinytest::dependenciesInstalled()){
  shinytest::installDependencies()
}


# NOTE: phantomJS does not handle newest features in JavaScript.
# In such cases it does not load the JS file without any warning.
# This leads to fail of all the test that depends on `shinytest`. 
# So, for now, when `shinytest` depends on the phantomJS, you need to
# keep this in mind, that your test might fails from that `hidden` reason.
# (for more details check e.g.:
# https://github.com/mdlama/reprex-shinytest-js-inclusion-bug).
test_check("XIFF")
