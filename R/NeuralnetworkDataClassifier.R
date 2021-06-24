#' Random data set for testing the classification models.
#'
#' Random data set for testing the classification models.
#'
#' @format A data frame with 56 rows and 7 variables:
#' \describe{
#'   \item{class}{group to classify}
#'   \item{other columns}{independent variables}
#' }
#' @source geneinfo2db
#' 
#' @examples 
#' 
#' data("train_model_data", package = "XIFF")
#' 
"train_model_data"

#' Create neuralnetwork model info structure for caret classification
#' 
#' @details 
#' 
#' For some reason caret does not support neuralnetwork package for classification task. The code below implements required structure to make it work.
#' For more information of implementing custom ML method in caret please refer to https://topepo.github.io/caret/using-your-own-model-in-train.html
#' 
#' @export
#' 
#' @importFrom NeuralNetTools olden
#' @importFrom neuralnet neuralnet
#' @importFrom magrittr `%>%`
#' 
#' 
#' @examples 
#' library(dplyr)
#' data("train_model_data", package = "XIFF")
#' modelInfo <- modelInfoNeuralNetwork()
#' 
#' # Basic usage
#' param <- data.frame(layer1=  10, layer2 = 10, layer3 = 10)
#' x <- train_model_data %>% select(-class, -celllinename)
#' y <- train_model_data$class
#' 
#' fit <- modelInfo$fit(x = x, y = y, param = param)
#' modelInfo$predict(fit, train_model_data)
#' 
#' # With caret
#' library(caret)
#' xdata <- train_model_data %>% select(ENSG00000169047, ENSG00000188322, class)
#' 
#' fitControl <- caret::trainControl(
#'   method = "none",
#'   savePredictions = "final"
#' )
#' 
#' model <- caret::train(
#'     class ~ ., xdata, method = modelInfoNeuralNetwork(),
#'     trControl = fitControl, tuneGrid = param)
#' predict(model)
#' 
#' modelInfoNeuralNetwork()$varImp(model) # variable importance
#' caret::varImp(model) # it performs some kind of reguralization - and shouldn't be used
#' 
#' predict(model)
#' predict(model, type = "prob")
#' 
modelInfoNeuralNetwork <- function() {
  NeuralNetwork4Classification <- list(
    label = "Neural Network for Classification",
    library = "neuralnet",
    loop = NULL,
    type = 'Classification',
    parameters = data.frame(
          parameter = c('layer1', 'layer2', 'layer3'),
          class = c('numeric', 'numeric', 'numeric'),
          label = c('#Hidden Units in Layer 1', '#Hidden Units in Layer 2', '#Hidden Units in Layer 3')),
    grid = function(x, y, len = NULL, search = "grid") {
      if(search == "grid") {
        out <- data.frame(layer1 = (1:len) + 1, layer2 = (1:len) + 1, layer3 = 0)
      } else {
        out <- data.frame(
          layer1 = sample(2:20, replace = TRUE, size = len),
          layer2 = sample(c(0, 2:20), replace = TRUE, size = len),
          layer3 = sample(c(0, 2:20), replace = TRUE, size = len)
          )
      }
      out
    },
    fit = function(x, y, wts, param, lev, last, classProbs, ...) {
      
      ## Prepare data frame for training
      colNames <- colnames(x)
      dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
      dat$.outcome <- y
      form <- as.formula(paste(".outcome ~",paste(colNames, collapse = "+")))
      
      # Prepare arguments
      layers <- unlist(param)
      layers <- layers[layers != 0]
      
      neuralnet::neuralnet(
        form,
        data = dat,
        hidden = layers,
        act.fct = "logistic",
        linear.output = FALSE,
        ...)
    },
    predict = function(modelFit, newdata, submodels = NULL) {
      
      if(missing(newdata) || is.null(newdata)) {
        newdata <- modelFit$data
      }
      
      newdata <- newdata[, modelFit$model.list$variables, drop = FALSE]
      
      responses <- getNNLabelOrdering(modelFit)
      responses[as.integer(predict(modelFit, newdata = newdata)[,2] > 0.5)+1]
    },
    prob = function(modelFit, newdata, submodels = NULL) {
      
      if(missing(newdata) || is.null(newdata)) {
        newdata <- modelFit$data
      }
      
      responses <- getNNLabelOrdering(modelFit)
      
      newdata <- newdata[, modelFit$model.list$variables, drop = FALSE]
      res <- predict(modelFit, newdata = newdata)
      colnames(res) <- responses
      res
    },
    tags = "Neural Network for Classification",
    sort = function(x) x[order(x$layer1, x$layer2, x$layer3), ],
    varImp = function(object, ...){
      if(inherits(object, "train")) {
        if(is.null(object$finalModel)) {
          stop("Cannot extract finalModel from caret's 'train' object in XIFF::modelInfoNeuralNetwork()$varImp")
        }
        model <- object$finalModel
      } else if(inherits(object, "nn")) {
        model <- object
      } else {
        stop("Model not supported in XIFF::modelInfoNeuralNetwork()$varImp!")
      }
      
      secondResponse <- getNNLabelOrdering(object)[2]
      NeuralNetTools::olden(model, out_var = secondResponse, bar_plot = FALSE)
    }
  )
}

#' Function that fixes the problem with responses ordering in caret model.
#'
#' @param modelFit nn model returned from caret.
#' 
#' @noRd
#' @noMd
#' 
#' @details 
#' 
#' For some reason responses (classes) ordering might be reverted 
#' - in such case using that function fixes the problem.
#'
getNNLabelOrdering <- function(modelFit) {
  
  if(inherits(modelFit, "train")) {
    if(is.null(modelFit$finalModel)) stop("Cannot extract finalModel from caret's 'train' object in XIFF:::getNNLabelOrdering")
    return(getNNLabelOrdering(modelFit$finalModel))
  }
  
  if(!is.null(modelFit$obsLevels)) {
    modelFit$obsLevels
  } else {
    colnames(modelFit$response)
  }
}
