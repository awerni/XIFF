#' Generate Volcano Plot
#'
#' @param diffExpr differential result expression table
#' @param minuslog10pval 
#' @param minuslog10adjpval 
#' @param log2FC 
#' @param FCside "both sides", "left", "right"
#' @param classLabels 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' if(require("CLIFF")) {
#'    gene_anno <- CLIFF::getGeneAnno("human")
#'    cs <- CLIFF::exampleClassAssigment()
#'    limmaData <- CLIFF::differentialGeneExpression_LimmaVoom(cs, gene_anno)
#'    generateVolcanoPlot(
#'      limmaData,
#'      minuslog10pval = 2,
#'      minuslog10adjpval = 0,
#'      log2FC = 2.5,
#'      FCside = "both sides"
#'    )
#' }
#' 
#' 
generateVolcanoPlot <- function(diffExpr, minuslog10pval, minuslog10adjpval, log2FC, 
                                FCside = "both sides", classLabels = c("class1", "class2")) {
  diffExpr <- diffExpr %>% mutate(
    selected = -log10(P.Value) >= minuslog10pval & -log10(adj.p.val) >= minuslog10adjpval
  )
  
  diffExpr <- if (FCside == "both sides") {
    diffExpr %>% mutate(selected = selected & (logFC <= -log2FC | logFC >= log2FC))
  } else if (FCside == "left") {
    diffExpr %>% mutate(selected = selected & (logFC <= -log2FC))
  } else if (FCside == "right") {
    diffExpr %>% mutate(selected = selected & (logFC >= log2FC))
  }
  
  diffExpr <- diffExpr %>% mutate(myLab = ifelse(selected, symbol, NA))
  X <- diffExpr %>% select(logFC) %>% range()
  
  g <- ggplot(
    data = diffExpr, 
    mapping = aes(
      x = logFC, 
      y = -log10(P.Value), 
      label = myLab, 
      color = selected
    )
  ) + 
    geom_point() +
    xlab("log2 Fold Change") + 
    ylab("-log10 p-value") + 
    commonPlotTheme() +
    scale_color_manual(values = plotColors[2:1]) +
    annotate(
      geom = "text", 
      x = X[1], 
      y = 0, 
      label = paste("higher in", classLabels[1]), 
      hjust = 0
    ) +
    annotate(
      geom = "text", 
      x = X[2], 
      y = 0, 
      label = paste("higher in", classLabels[2]), 
      hjust = 1
    )
  
  if (diffExpr %>% filter(selected) %>% nrow() < 100) {
    g <- g + ggrepel::geom_text_repel(size = 4, show.legend = FALSE, na.rm = TRUE)
  }
  
  g
}


#' Generate GSEA Plot
#'
#' @param diffExResult 
#' @param ensg_geneset 
#' @param rankType "p.valueDir", "logFC"
#' @param classLabels 
#'
#' @return ggplot2 plot
#' @export
#'
#' @examples
#' 
#' if(require("CLIFF")) {
#'    gene_anno <- CLIFF::getGeneAnno("human")
#'    cs <- CLIFF::exampleClassAssigment()
#'    limmaData <- CLIFF::differentialGeneExpression_LimmaVoom(cs, gene_anno)
#'    geneset <- CLIFF::getGSEAdata("human", gene_set = "HALLMARK_P53_PATHWAY")
#'    
#'    generateGSEA_plot(
#'      limmaData,
#'      ensg_geneset = geneset,
#'      rankType = "p.valueDir"
#'    )
#' }
#' 
generateGSEA_plot <- function(diffExResult, ensg_geneset, rankType, classLabels = NULL) {
  
  if(is.null(classLabels) && inherits(diffExResult, "ClassAssigmentInAttribute")) {
    classLabels <- getClassLabelVector(diffExResult)
  } else if(is.null(classLabels)) {
    classLabels <- c("class1", "class2")
  }
  
  ensg_rank <- if (rankType == "logFC") {
    diffExResult %>% 
      arrange(logFC) %>% 
      mutate(in_geneset = ensg %in% unlist(ensg_geneset))
  } else {
    diffExResult %>% 
      mutate(pRank = ifelse(logFC < 0, log(P.Value), -log(P.Value))) %>%
      arrange(pRank) %>% 
      mutate(in_geneset = ensg %in% unlist(ensg_geneset))
  }
  
  stepSize <- 1/table(ensg_rank$in_geneset) * c(-1, 1)
  nESNG <- nrow(ensg_rank)
  ensg_rank <- ensg_rank %>% mutate(step = cumsum(stepSize[as.character(in_geneset)]), n = 1:nESNG)
  
  X <- ensg_rank %>% select(n) %>% range()
  Y <- ensg_rank %>% select(step) %>% range()
  Y <- if (mean(Y) < 0) Y <- Y[1] else Y[2]
  
  p1 <- ggplot(
    data = ensg_rank, 
    mapping = aes(x = n, y = step, color = "red")
  ) + 
    geom_line() + 
    ylab("Enrichment score (ES)") + 
    xlab("") +
    commonPlotTheme("none") +
    geom_hline(
      yintercept = 0, 
      linetype = "dashed"
    ) + 
    ggtitle(names(ensg_geneset)) + 
    coord_cartesian(xlim = c(0, nESNG)) +
    annotate(
      geom = "text", 
      x = X[1], 
      y = Y, 
      label = paste("higher in", classLabels[1]), 
      hjust = 0
    ) +
    annotate(
      geom = "text", 
      x = X[2],
      y = Y, 
      label = paste("higher in", classLabels[2]), 
      hjust = 1
    )
  
  p2 <- ggplot(
    data = ensg_rank, 
    mapping = aes(x = n)
  ) + 
    geom_vline(xintercept = ensg_rank$n[ensg_rank$in_geneset]) + 
    xlab("") +
    coord_cartesian(xlim = c(0, nESNG)) + 
    commonPlotTheme() + 
    theme(
      axis.ticks = element_blank(), 
      axis.text.x = element_blank()
    )
  
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  
  g <- rbind(g1, g2, size = "last")
  g$widths <- grid::unit.pmax(g1$widths, g2$widths)
  
  panels <- g$layout$t[grep("panel", g$layout$name)]
  
  g$heights[panels[1]] <- unit(12, "null")
  g$heights[panels[2]] <- unit( 2, "null")
  
  class(g) <- c("customPlotPrint", class(g))
  g
}

#' @importFrom grid grid.draw
#' @exportS3Method grid.draw ggsurvplot
#' @export
grid.draw.ggsurvplot <- function(x){
  print(x, newpage = FALSE)
}
