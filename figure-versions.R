atime.results <- readRDS("figure-versions-data.rds")
library(ggplot2)
library(data.table)
param.dt <- rowwiseDT(
  Test=, xmax=, width=, Nlab=,
  "DT[by] fixed in #4558", 2e5, 5, "N = number of rows, 0.9*N = groups to summarize over",
  "fread N=cols regression", 1e3, 3, "N = number of columns in CSV file")
for(Test in names(atime.results)){
  param.row <- param.dt[Test, on="Test"]
  tres <- atime.results[[Test]]
  tref <- atime::references_best(tres)
  tpred <- predict(tref)
  prefix <- atime:::test_file_name(Test)
  out.png <- sprintf("figure-versions-%s.png", prefix)
  png(out.png, width=param.row$width, height=3, units="in", res=200)
  meas <- tpred$meas
  msec <- meas[unit=="seconds"]
  pred <- tpred$pred
  one <- pred[1]  
  gg <- ggplot2::ggplot()+
    ggplot2::theme_bw()+
    ggplot2::facet_grid(unit ~ ., scales="free")+
    ggplot2::geom_hline(ggplot2::aes(
      yintercept=unit.value),
      data=one)+
    ggplot2::geom_text(ggplot2::aes(
      0, unit.value, label=sprintf("limit=%s sec.",unit.value)),
      hjust=0,
      size=3,
      vjust=1.2, 
      data=one)+
    ggplot2::geom_ribbon(ggplot2::aes(
      N, ymin=min, ymax=max, fill=expr.name),
      data=msec,
      alpha=0.5)+
    ggplot2::geom_line(ggplot2::aes(
      N, empirical, color=expr.name),
      data=msec)+
    ggplot2::geom_point(ggplot2::aes(
      N, unit.value, color=expr.name),
      data=pred,
      shape=21,
      fill="white")+
    directlabels::geom_dl(ggplot2::aes(
      N, unit.value, 
      label=label,
      color=expr.name),
      data=pred,
      method=list(cex=0.5, directlabels::polygon.method("top",offset.cm=0.5)))+
    ggplot2::theme(legend.position="none")+
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

