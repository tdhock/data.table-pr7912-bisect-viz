data.dir <- file.path("data", system("hostname", intern=TRUE))
src.prefix <- normalizePath("~/R/data.table/.ci/atime/bisect/DT_by__fixed_in__4558_")
if(FALSE){
  (src.dirs <- Sys.glob(paste0(src.prefix, "*")))
  unlink(data.dir, recursive = TRUE)
  dir.create(data.dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(src.dirs, data.dir, recursive = TRUE)
}
(Test.dir.vec <- Sys.glob(file.path(data.dir, "*")))

library(data.table)
version.colors <- c(
  Fast="violet",
  Slow="red",
  HEAD="blue")
details.dt.list <- list()
dot.dt.list <- list()
common.dt.list <- list()
results.dt.list <- list()
for(Test.dir in Test.dir.vec){
  Bisect <- function(dt)dt[
  , Bisect := gsub("-", " ", sub(paste0(".*",basename(src.prefix)), "", Test.dir))
  ][
  , Commit := paste(Bisect, commit)
  ][]
  for(csv in Sys.glob(file.path(Test.dir, "csv", "*"))){
    result.row <- Bisect(fread(csv))
    Age <- function(age)factor(age, c("old","between","new"))
    version.meta <- data.table(
      version=sub("N.", "", grep("^N", names(result.row), value=TRUE)),
      age=Age(c("old","new","between")),
      color=c("violet","red","blue"))
    results.dt.list[[csv]] <- melt( 
      result.row,
      measure.vars=measure(variable, version, sep=".")
    )[version.meta, on="version"]
  }
  for(result.RData in Sys.glob(file.path(Test.dir, "RData", "*"))){
    (objs <- load(result.RData))
    commit <- sub(".RData", "", basename(result.RData))
    details <- Bisect(tres.or.status$measurements[
      names(version.colors),
      on="version",
      data.table(commit, expr.name, version, N, median, min, max)
    ])
    common.dt.list[[paste(Test.dir, commit)]] <- Bisect(details[, .(
      versions=.N
    ), by=N][
      versions==max(versions)
    ][.N, data.table(commit, N)])
    evers <- unique(details[, .(expr.name, version)])
    details.dt.list[[paste(Test.dir, commit)]] <- details
    dot.dt.list[[paste(Test.dir, commit)]] <- Bisect(data.table(
      commit,
      tpred$prediction[evers, on="expr.name"]))
  }
}
(details.dt <- rbindlist(details.dt.list))
(dot.dt <- rbindlist(dot.dt.list))
(common.dt <- rbindlist(common.dt.list))
status.colors <- c(
  "0"="black",
  change="grey",
  "1"="white")
St <- function(s)factor(s, names(status.colors))
(results.dt <- rbindlist(results.dt.list)[, let(
  Status = St(status)
)][])

ref.dt <- fread("refs.csv")
(rank.dt <- unique(rbind(
  results.dt[, .(time, commit)],
  ref.dt[, .(time, commit)]
))[order(time)][, rank := .I][, .(commit, rank)])
results.rank <- rank.dt[results.dt, on="commit"]
HEAD.dt <- results.rank[version=="HEAD"]

HEAD.stats <- dcast(HEAD.dt, rank + commit + time + variable + age ~ ., list(min, median, max, length))
HEAD.stats[value_length>1]
library(animint2)
age.colors <- version.meta[, setNames(color, age)]
ggplot()+
  geom_ribbon(aes(
    rank,
    ymin=value_min,
    ymax=value_max,
    fill=age),
    alpha=0.5,
    data=HEAD.stats)+
  scale_color_manual(values=age.colors)+
  scale_fill_manual(values=age.colors)+
  geom_line(aes(
    rank, value_median, color=age),
    data=HEAD.stats)+
  facet_grid(variable ~ ., scales="free")+
  scale_y_log10()

HEAD.N <- HEAD.dt[variable=="N"]
Bisect.range <- nc::capture_first_df(
  dcast(
    HEAD.N,
    Bisect ~ .,
    list(length, min, max),
    value.var="rank"
  )[order(rank_min, rank_max)][, y := 10^.I],
  Bisect=list(old=".*", "_", new=".*"))
for(age in c("old", "new")){
  set(Bisect.range, j=paste0("rank_", age), value=ref.dt[
    Bisect.range[[age]],
    rank.dt[commit, .(rank), on="commit"],
    on="Rname"])
}
(Bisect.ext <- melt(
  Bisect.range,
  measure.vars=measure(
    value.name,
    age=Age,
    pattern="(.*)_(old|new)")))
change.dt <- setkey(HEAD.N, Bisect, rank)[, .(
  rank_change=rank[[which(diff(status) != 0)]]+0.5
), by=Bisect][Bisect.range, on="Bisect"]
(lim.segs <- results.rank[age!="between", {
  ##browser()
  .(
  med_value=median(value)
  )}, by=.(Bisect, variable, age)][Bisect.range, on="Bisect"])
animint(
  ggplot()+
    theme_bw()+
    geom_rect(aes(
      xmin=rank_old, xmax=rank_new,
      key=1,
      ymin=0, ymax=Inf),
      alpha=0.5,
      color=NA,
      fill="grey",
      showSelected="Bisect",
      data=data.table(Bisect.range)[, panel := NULL])+
    geom_vline(aes(
      xintercept=rank_change,
      key=1),
      color=status.colors[["change"]],
      showSelected=c("Bisect","Status"),
      data=data.table(change.dt)[, let(panel = NULL, Status=St("change"))])+
    geom_segment(aes(
      ifelse(age=="old", rank_old, rank_min), med_value,
      xend=ifelse(age=="old", rank_max, rank_new), yend=med_value,
      key=age,
      color=age),
      showSelected="Bisect",
      data=data.table(lim.segs)[, panel := variable])+
    geom_segment(aes(
      x=rank_old, xend=rank_new,
      y=y, yend=y),
      size=2,
      clickSelects="Bisect",
      data=Bisect.range[, panel := "Bisect"])+
    geom_point(aes(
      rank, y, color=age),
      clickSelects="Bisect",
      data=Bisect.ext[, panel := "Bisect"])+
    geom_segment(aes(
      x=rank_min, xend=rank_max,
      y=y, yend=y),
      clickSelects="Bisect",
      size=5,
      data=Bisect.range[, panel := "Bisect"])+
    geom_point(aes(
      rank_change, y, fill=Status),
      clickSelects="Bisect",
      data=data.table(change.dt)[, let(panel = "Bisect", Status=St("change"))])+
    geom_line(aes(
      rank, value_median, color=age),
      size=1,
      data=HEAD.stats[, panel := variable])+
    geom_point(aes(
      rank, value,
      key=paste(rank, age),
      fill=Status,
      color=age),
      showSelected="Bisect",
      data=data.table(results.rank)[, panel := variable])+
    facet_grid(panel ~ ., scales="free")+
    scale_color_manual(values=age.colors, drop=FALSE)+
    scale_fill_manual(values=status.colors, drop=FALSE)+
    scale_x_continuous(
      "Commit rank over time")+
    scale_y_log10("", labels=function(b){
      if(max(b, na.rm=TRUE)>1e5)rep("", length(b)) else format(b)
    }),
  duration=list(
    Bisect=1000)
)

(label.dt <- results.long[, {
  if(Bisect=="run2b intermediate v1.14.8"){
    fun <- min
    hjust <- 1
  }else{
    fun <- max
    hjust <- 0
  }
  data.table(hjust, .SD[rank==fun(rank)])
}, by=Bisect])
limit.dt <- data.table(seconds=tpred$seconds.limit)

animint(
  title="bisect",
  overview=ggplot()+
    ggtitle("Overview of three git bisect runs, select commit")+
    theme_bw()+
    theme(axis.text.x=element_text(angle=30, hjust=1))+
    theme_animint(width=1000)+
    geom_vline(aes(
      xintercept=rank,
      key=1),
      color="black",
      showSelected="Commit",
      data=results.wide[, .(rank, Commit)])+
    geom_tallrect(aes(
      xmin=rank-0.5, xmax=rank+0.5,
      key=rank,
      fill=Status),
      color="grey",
      alpha=0.5,
      data=results.wide)+
    geom_line(aes(
      rank, value, color=version, group=version),
      data=results.long)+
    geom_point(aes(
      rank, value, color=version, group=version),
      fill=NA,
      data=results.long)+
    facet_grid(
      variable ~ Bisect,
      scales="free_y",
      labeller=label_both)+
    geom_label_aligned(aes(
      rank, value,
      hjust=hjust,
      label=version, color=version),
      data=label.dt,
      alignment="vertical")+
    scale_fill_manual(values=c(
      "0"="grey",
      "1"="white"))+
    scale_y_continuous("")+
    scale_x_continuous("Commit rank over time")+
    scale_color_manual(values=version.colors)+
    geom_tallrect(aes(
      xmin=rank-0.5, xmax=rank+0.5),
      clickSelects="Commit",
      alpha=1,
      alpha_off=0,
      color="black",
      color_off=NA,
      fill=NA,
      data=results.wide),
  details=ggplot()+
    ggtitle("Asymptotic timing results for selected commit")+
    theme_bw()+
    theme(legend.position="none")+
    theme_animint(width=1000)+
    geom_hline(aes(
      yintercept=seconds),
      color="grey",
      data=limit.dt)+
    geom_text(aes(
      0, seconds*1.1,
      label=sprintf(
        "time limit = %s seconds",
        paste(seconds)
      )),
      hjust=0,
      color="grey50",
      data=limit.dt)+
    geom_vline(aes(
      xintercept=N,
      key=1),
      data=common.dt,
      color="grey",
      showSelected="Commit")+
    geom_text(aes(
      N, 0, label=sprintf("largest common N=%s", paste(N)),
      key=1),
      data=common.dt,
      hjust=1,
      color="grey50",
      showSelected="Commit")+
    geom_text(aes(
      0, 0,
      label=commit,
      href=paste0("https://github.com/Rdatatable/data.table/commit/", commit),
      key=1),
      hjust=0,
      vjust=0,
      data=common.dt,
      showSelected="Commit")+
    geom_ribbon(aes(
      N, ymin=min, ymax=max,
      key=version, fill=version, group=version),
      showSelected=c("version","Commit"),
      alpha=0.2,
      color=NA,
      data=details.dt)+
    geom_line(aes(
      N, median,
      key=version, color=version, group=version),
      showSelected=c("version","Commit"),
      data=details.dt)+
    geom_point(aes(
      N, unit.value,
      key=version, color=version),
      showSelected=c("version","Commit"),
      fill="white",
      data=dot.dt)+
    geom_label_aligned(aes(
      N, unit.value, label=version,
      key=version, color=version),
      showSelected=c("version","Commit"),
      alignment="horizontal",
      vjust=-0.1,
      alpha=0.5,
      data=dot.dt)+
    scale_x_log10("N = number of columns in CSV file")+
    scale_color_manual(values=version.colors)+
    scale_fill_manual(values=version.colors)+
    scale_y_log10("Seconds to fread(file.csv)"),
  duration=list(
    Commit=1000)
)

