atime.results <- readRDS("figure-versions-data.rds")
library(ggplot2)
library(data.table)
param.dt <- rowwiseDT(
  Test=, xmax=, width=, Nlab=,
  "DT[by] fixed in #4558", 1e7, 7, "N = number of rows, 0.9*N = number of groups to summarize over",
  "fread N=cols regression", 1e4, 3, "N = number of columns in CSV file")
for(Test in names(atime.results)){
  param.row <- param.dt[Test, on="Test"]
  tres <- atime.results[[Test]]
  tref <- atime::references_best(tres)
  tpred <- predict(tref)
  prefix <- atime:::test_file_name(Test)
  png(paste0(prefix, ".png"), width=param.row$width, height=3, units="in", res=200)
  gg <- plot(tpred)+
    ggtitle(Test)+
    facet_null()+
    scale_x_log10(param.row$Nlab)+
    scale_y_log10(
      "Computation time (seconds)",
      limits=c(0.002, 0.3))+
    coord_cartesian(
      xlim=c(10, param.row$xmax))
  print(gg)
  dev.off()
}

