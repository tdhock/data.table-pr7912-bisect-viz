library(animint2)
library(data.table)
details.dt.list <- list()
dot.dt.list <- list()
common.dt.list <- list()
results.dt.list <- list()
(Bisect.dir.vec <- Sys.glob("data/dinf-thock-02i/bisect-2s-lim0.1/DT_by__fixed_in__4558/*"))
(Bisect.dir.vec <- Sys.glob("data/tdhock-MacBook/bisect-2s-lim0.1-complete/DT_by__fixed_in__4558/*"))
age.colors <- c(
  old="violet",
  between="blue",
  new="red")
Age <- function(age)factor(age, names(age.colors))
for(Bisect.dir in Bisect.dir.vec){
  Bisect <- function(dt)dt[
  , Bisect := basename(Bisect.dir)
  ][]
  Commit <- function(dt)Bisect(dt)[
  , Commit := paste(Bisect, commit)
  ][]
  Test_commits <- fread(file.path(Bisect.dir, "Test_commits.csv"))
  Test_commits_long <- melt(
    Test_commits,
    measure.vars=measure(age=Age, pattern="(old|new)"),
    value.name="version")
  version.meta <- Test_commits_long[, .(
    version=c(version, "HEAD"),
    age=c(age, Age("between")))]
  for(csv in Sys.glob(file.path(Bisect.dir, "csv", "*"))){
    result.row <- Commit(fread(csv))
    results.dt.list[[csv]] <- melt( 
      result.row,
      measure.vars=measure(variable, version, sep=".")
    )[version.meta, on="version"]
  }
  for(result.RData in Sys.glob(file.path(Bisect.dir, "RData", "*"))){
    (objs <- load(result.RData))
    commit <- sub(".RData", "", basename(result.RData))
    details <- Commit(tres.or.status$measurements[, data.table(
      commit, version=expr.name, N, median, min, max)
    ])[version.meta, on="version"]
    (common.dt.list[[paste(Bisect.dir, commit)]] <- Commit(details[, .(
      versions=.N
    ), by=N][
      versions==max(versions)
    ][.N, data.table(commit, N)]))
    details.dt.list[[paste(Bisect.dir, commit)]] <- details
    dot.dt.list[[paste(Bisect.dir, commit)]] <- Commit(tpred$prediction[, data.table(
      commit,
      unit.value,
      version=expr.name,
      N
    )][version.meta, on="version"])
  }
}
(details.dt <- rbindlist(details.dt.list))
(dot.dt <- rbindlist(dot.dt.list))
(common.dt <- rbindlist(common.dt.list))
status.colors <- c(
  "0"="black",
  change="grey50",
  "1"="white")
St <- function(s)factor(s, names(status.colors))
(results.dt <- rbindlist(results.dt.list)[, let(
  Status = St(status)
)][])

timeline.dt <- results.dt[
, .SD[1]
, by=.(Bisect, commit, start, end)
][order(start)][
, bisect := factor(Bisect, unique(Bisect))
][]
to.min <- function(x, start)(x-min(start))/60
gg <- ggplot()+
  theme_bw()+
  geom_segment(aes(
    to.min(start, start), bisect,
    xend=to.min(end, start), yend=bisect),
    size=1,
    data=timeline.dt)+
  geom_point(aes(
    to.min(start, start), bisect),
    shape=1,
    data=timeline.dt)+
  scale_y_discrete(
    "Bisect run (old-new)")+
  scale_x_continuous(
    "Minutes from start of first git bisect (dot at start of each version tested)")
if(FALSE){
  png("viz-PR7912-minutes.png", width=8, height=2, units="in", res=200)
  print(gg)
  dev.off()
}

(ref.dt <- fread("refs.csv"))
(rank.dt <- unique(rbind(
  results.dt[, .(time, commit)],
  ref.dt[Rname %in% results.dt$version, .(time, commit)]
))[order(time)][, rank := .I][, .(commit, rank)])
results.rank <- rank.dt[results.dt, on="commit"]
HEAD.dt <- results.rank[version=="HEAD"]

HEAD.stats <- dcast(HEAD.dt, rank + commit + time + variable + age ~ ., list(min, median, max, length))
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
  facet_grid(variable ~ ., scales="free")

not.HEAD <- rank.dt[
  ref.dt[
    results.rank[version!="HEAD"][, .(Rname=version, variable, value)],
    on="Rname"],
  on="commit"]
med.line.list <- list(not.HEAD, HEAD.dt)
(med.line.dt <- dcast(rbindlist(lapply(
  med.line.list,
  function(DT)DT[, c("rank", "variable", "value")])),
  rank + variable ~ .,
  list(median, length)
  )[, age := Age("between")][])
ggplot()+
  scale_color_manual(values=age.colors)+
  geom_line(aes(
    rank, value_median, color=age),
    data=med.line.dt)+
  facet_grid(variable ~ ., scales="free")

ref.rank <- rank.dt[
  ref.dt,
  on="commit",
  nomatch=0L
][!c("Parent4558","PR7401"), on="Rname"][, y := 5.25-.I*0.3]
ggplot()+
  theme_bw()+
  geom_hline(aes(
    yintercept=rank),
    color="grey",
    data=ref.rank)+
  geom_path(aes(
    value_median, rank),
    data=med.line.dt[variable=="log10_N"])+
  geom_text(aes(
    -Inf, rank, label=Rname,
    vjust=ifelse(grepl("Parent",Rname), 1, 0)),
    data=ref.rank,
    hjust=0,
    color="grey50")

(limit.dt <- data.table(seconds=tpred$seconds.limit))
gg <- ggplot()+
  ggtitle("Recent changes in performance, one test case for R data.table")+
  theme_bw()+
  geom_vline(aes(
    xintercept=rank),
    color="grey",
    data=ref.rank)+
  geom_path(aes(
    rank, 10^value_median),
    data=med.line.dt[variable=="log10_N"])+
  geom_text(aes(
    rank, 10^y, label=sprintf(
      "%s\n%s", Rname, strftime(time, "%Y-%m-%d")),
      hjust=ifelse(grepl("Parent|Fas",Rname), 1, 0)),
    data=ref.rank,
    vjust=0,
    color="grey50")+
  scale_y_log10(sprintf("N = throughput at %s seconds", paste(limit.dt$seconds)))+
  scale_x_continuous("Commit rank over time")
if(FALSE){
  png("viz-PR7912-timeline.png", width=8, height=4, units="in", res=200)
  print(gg)
  dev.off()
}

HEAD.N <- HEAD.dt[variable=="log10_N"]
HEAD.N.stats <- dcast(
  HEAD.N,
  Bisect ~ .,
  list(length, min, max),
  value.var="rank"
)[order(rank_min, rank_max)][, y := .I][]
Bisect.range <- nc::capture_first_df(
  HEAD.N.stats,
  Bisect=list(old=".*", "-", new=".*"))
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
change.dt <- setkey(HEAD.N, Bisect, rank)[, {
  .(
  rank_change=c(rank,rank[.N]+1)[[which(diff(c(0,status,1)) != 0)]]-0.5
)}, by=Bisect][Bisect.range, on="Bisect"]
(lim.segs <- results.rank[age!="between", {
  ##browser()
  .(
  med_value=median(value)
  )}, by=.(Bisect, variable, age)][Bisect.range, on="Bisect"])
(zoom.range <- dcast(
  results.rank,
  Bisect + variable ~ .,
  list(min, max),
  value.var=c("value","rank")))
norm01 <- function(DT)DT[
  zoom.range,
  on=.(Bisect, variable)
][, let(
  rank_rel=rank-rank_min,
  value_norm=(value-value_min)/(value_max-value_min)
)][]
(points.zoom <- norm01(results.rank))
(lines.zoom <- norm01(Bisect.range[med.line.dt, .(
  Bisect, rank=i.rank, variable, value=value_median, age
), on=.(rank_min<=rank, rank_max>=rank), nomatch=0L]))
bg.color <- "grey"
viz <- animint(
  out.dir="viz",
  title="data.table git bisect performance analysis for PR7912, 0.01 sec time limit, N on log10 scale",
  source="https://github.com/tdhock/data.table-pr7912-bisect-viz/blob/main/viz.R",
  bisects=ggplot()+
    ggtitle("Overview of all commits tested, select bisect")+
    theme_bw()+
    theme(legend.position="none")+
    theme_animint(height=700, width=600, rowspan=2)+
    geom_rect(aes(
      xmin=rank_old, xmax=rank_new,
      key=1,
      ymin=-Inf, ymax=Inf),
      alpha=0.3,
      color=NA,
      fill="grey",
      showSelected="Bisect",
      data=data.table(Bisect.range))+
    geom_tallrect(aes(
      xmin=rank-0.5, xmax=rank+0.5,
      key=rank),
      showSelected="Bisect",
      color=bg.color,
      fill=bg.color,
      data=data.table(HEAD.dt)[, panel := variable])+
    geom_vline(aes(
      xintercept=rank_change,
      key=1),
      color=status.colors[["change"]],
      showSelected=c("Bisect","Status"),
      data=data.table(change.dt, Status=St("change")))+
    geom_segment(aes(
      ifelse(age=="old", rank_old, rank_min), med_value,
      xend=ifelse(age=="old", rank_max, rank_new), yend=med_value,
      key=age,
      color=age),
      showSelected=c("age","Bisect"),
      data=data.table(lim.segs)[, panel := variable])+
    geom_segment(aes(
      x=rank_old, xend=rank_new,
      y=y, yend=y),
      size=4,
      clickSelects="Bisect",
      data=data.table(Bisect.range, panel = "Bisect"))+
    geom_text(aes(
      x=(min(rank_old)+max(rank_new))/2, y=0,
      key=1,
      label=Bisect),
      showSelected="Bisect",
      data=data.table(Bisect.range, panel = "Bisect"))+
    geom_point(aes(
      rank, y, color=age),
      clickSelects=c("age","Bisect"),
      size=4,
      data=data.table(Bisect.ext, panel = "Bisect"))+
    geom_segment(aes(
      x=rank_min, xend=rank_max,
      y=y, yend=y),
      clickSelects="Bisect",
      size=7,
      data=data.table(Bisect.range, panel = "Bisect"))+
    geom_point(aes(
      rank_change, y, fill=Status),
      clickSelects="Bisect",
      showSelected="Status",
      size=5,
      data=data.table(change.dt, panel = "Bisect", Status=St("change")))+
    geom_line(aes(
      rank, value_median, color=age),
      size=1,
      showSelected="age",
      data=data.table(med.line.dt)[, panel := variable])+
    geom_point(aes(
      rank, value,
      key=paste(rank, age),
      fill=Status,
      color=age),
      size=3,
      showSelected=c("Status", "age","Bisect"),
      data=data.table(results.rank)[, panel := variable])+
    geom_tallrect(aes(
      xmin=rank-0.5, xmax=rank+0.5,
      key=rank),
      clickSelects="Commit",
      showSelected="Bisect",
      alpha=1,
      alpha_off=0,
      color="black",
      color_off=NA,
      fill=NA,
      data=data.table(HEAD.dt)[, panel := variable])+
    facet_grid(panel ~ ., scales="free")+
    scale_color_manual(values=age.colors, drop=FALSE)+
    scale_fill_manual(values=status.colors, drop=FALSE)+
    scale_y_continuous("")+
    scale_x_continuous(
      "Commit rank over time"),
  details=ggplot()+
    ggtitle("Asymptotic timing results for selected commit")+
    theme_bw()+
    theme(legend.position="none")+
    theme_animint(height=300, width=600, last_in_row=TRUE)+
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
    geom_ribbon(aes(
      N, ymin=min, ymax=max,
      key=age, fill=age, group=age),
      showSelected=c("age","Commit"),
      alpha=0.2,
      color=NA,
      data=details.dt)+
    geom_line(aes(
      N, median,
      key=age, color=age, group=age),
      showSelected=c("age","Commit"),
      data=details.dt)+
    geom_point(aes(
      N, unit.value,
      key=age, color=age),
      showSelected=c("age","Commit"),
      fill="white",
      size=3,
      data=dot.dt)+
    geom_label_aligned(aes(
      N, unit.value, label=age,
      key=age, color=age),
      showSelected=c("age","Commit"),
      alignment="horizontal",
      vjust=-0.1,
      alpha=0.5,
      data=dot.dt)+
    geom_text(aes(
      0, 0,
      label=substring(commit, 1, 8),
      href=paste0("https://github.com/Rdatatable/data.table/commit/", commit),
      key=1),
      hjust=0,
      vjust=0,
      data=common.dt,
      showSelected="Commit")+
    scale_x_log10("N = data size")+
    scale_color_manual(values=age.colors)+
    scale_fill_manual(values=age.colors)+
    scale_y_log10("Computation time (seconds)"),
  zoom=ggplot()+
    ggtitle("Zoom to bisect, relative values")+
    theme_bw()+
    theme_animint(width=550)+
    geom_tallrect(aes(
      xmin=rank_rel-0.5, xmax=rank_rel+0.5,
      key=rank_rel),
      showSelected="Bisect",
      color=bg.color,
      fill=bg.color,
      data=points.zoom[version=="HEAD"])+
    geom_line(aes(
      rank_rel, value_norm, color=age,
      key=1),
      size=1,
      showSelected="Bisect",
      data=lines.zoom)+
    geom_point(aes(
      rank_rel, value_norm,
      key=paste(rank, age),
      fill=Status,
      color=age),
      size=3,
      showSelected="Bisect",
      data=points.zoom)+
    coord_cartesian(ylim=c(0,1))+
    facet_grid(variable ~ ., scales="free")+
    scale_color_manual(values=age.colors, drop=FALSE)+
    scale_fill_manual(values=status.colors, drop=FALSE)+
    scale_y_continuous("")+
    ## scale_y_log10("", labels=function(b){
    ##   if(max(b, na.rm=TRUE)>1e5)rep("", length(b)) else format(b)
    ## }),
    geom_tallrect(aes(
      xmin=rank_rel-0.5, xmax=rank_rel+0.5,
      key=rank_rel),
      clickSelects="Commit",
      showSelected="Bisect",
      alpha=1,
      color="black",
      color_off=NA,
      fill=NA,
      data=points.zoom[version=="HEAD"])+
    scale_x_continuous(
      "Commit rank relative to bisect"),
  duration=list(
    Commit=1000,
    Bisect=1000)
)
viz

if(FALSE){
  animint2pages(viz, "2026-10-13-bisect-performance-pr7912", chromote_sleep_seconds=3)
  animint2pages(viz, "2026-10-13-bisect-performance-pr7912-log10-lim0.01sec", chromote_sleep_seconds=3)
}
