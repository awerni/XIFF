library(XIFF)
library(tidyverse)

XIFF::setDbOptions()

gene_anno <- CLIFF::getGeneAnno()$human
cl_anno <- CLIFF::getCellLineAnno("human")
tt <- levels(cl_anno$tumortype)

TRAILR2_sens <- XIFF::getStashedData("06bd26") %>%
  left_join(cl_anno, by = "celllinename") %>%
  filter(tumortype %in% tt)

cs <- split(TRAILR2_sens$celllinename, TRAILR2_sens$consensus_sensitivity)

TRAILR2_sens_class <-  XIFF::classAssignment(
  cs, positiveClass = "Sensitive"
)
TRAILR2_sens_class


sets <- XIFF::splitTrainingValidationSets(TRAILR2_sens_class, 0.2)
trainingSet <- sets$training
validationSet <- sets$validation

geneSetName <- "WP_APOPTOSIS_MODULATION_AND_SIGNALING"
geneSet <- CLIFF::getGSEAdata("human", "curated")[[geneSetName]]

set.seed(321)
grepFit <- XIFF::buildMachineLearning(
  cs = trainingSet,
  geneSet = geneSet,
  geneAnno = gene_anno,
  method = "GREP", p_validation = 0.2
)

validationData <- getDataForModel(grepFit$validationSet, grepFit)

directResult <- bind_cols(
  validationData %>% select(class, celllinename),
  predict(grepFit, newdata = validationData, type = "prob") %>%
    as.data.frame()
)



grepModel <- XIFF::stripGrepModel(grepFit, gene_anno)

ensg <- grepModel$modelCoefficients %>%
  select(ensg1, ensg2) %>%
  unlist() %>%
  na.omit() %>%
  unique()



rawData <- getRawDataForModel(ensg, grepFit$validationSet$celllinename)

expr_cl <- rawData %>%
  mutate(tpm = 2^score) %>%
  select(-score) %>%
  pivot_wider(names_from = ensg, values_from = tpm) %>% 
  column_to_rownames("celllinename")


# ----------- calc T-GREP-V2 ------------
calcGREP <- function(expr, myModel, epsilon = 15) {
  exprA <- expr + epsilon
  ret <- apply(exprA, 1, function(y) {
    myRatio <- log2(y[as.character(myModel$ensg1)] / y[as.character(myModel$ensg2)])
    myRatio[1] <- 1
    myT <- sum(myRatio * myModel$Coefficient)
    1/(1 + exp(-myT))
  }
  )
  return(ret)
}


res_grep_cl <- data.frame(score = calcGREP(expr_cl, myModel = grepModel$modelCoefficients, grepModel$epsilon)) %>%
  rownames_to_column("celllinename") 


resAll <- left_join(directResult, res_grep_cl)

resAll <- resAll %>% mutate(diff = score - class2)
max(abs(resAll$diff))

resAll
