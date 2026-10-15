library(ggplot2)
library(data.table)
details.dt.list <- list()
dot.dt.list <- list()
common.dt.list <- list()
results.dt.list <- list()
Bisect.dir.vec <- Sys.glob("data/dinf-thock-02i/bisect-2s-lim0.1-install/DT_by__fixed_in__4558/*")
age.colors <- c(
  old="violet",
  between="blue",
  new="red")
print(Bisect.dir.vec)
Age <- function(age)factor(age, names(age.colors))
for(Bisect.dir in Bisect.dir.vec){
  run.dir <- dirname(dirname(Bisect.dir))
  host.dir <- dirname(run.dir)
  Bisect <- function(dt)dt[, let(
    Bisect = basename(Bisect.dir),
    run = sub(".*-", "", run.dir),
    CPU_year = ifelse(
      grepl("dinf", basename(host.dir)),
      "2025", "2010")
  )][]
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
    results.dt.list[[csv]] <- Bisect(melt( 
      result.row,
      measure.vars=measure(variable, version, sep=".")
    )[version.meta, on="version"])
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
(results.dt <- rbindlist(results.dt.list, fill=TRUE, use.names = TRUE)[, let(
  Status = St(status)
)][])

min.common.N <- min(common.dt$N)
min.common.N <- 2^8
min.common.N <- 2^16
min.common.N <- 2^12
(ref.dt <- fread("refs.csv"))
(rank.dt <- unique(rbind(
  results.dt[, .(time, commit)],
  ref.dt[Rname %in% results.dt$version, .(time, commit)]
))[order(time)][, rank := .I][, .(commit, rank)])
details.common.N <- rank.dt[
  details.dt[N==min.common.N & version=="HEAD"],
  on="commit"]
stats.common.N <- dcast(
  details.common.N,
  CPU_year + run + rank ~ .,
  list(median, length),
  value.var="median")
results.rank <- rank.dt[results.dt, on="commit"]
HEAD.dt <- results.rank[version=="HEAD"]
HEAD.stats <- dcast(HEAD.dt, rank + commit + time + variable + age ~ ., list(min, median, max, length))
not.HEAD <- rank.dt[
  ref.dt[
    results.rank[version!="HEAD"][, .(Bisect, CPU_year, run, Rname=version, variable, value)],
    on="Rname"],
  on="commit"]
med.line.list <- list(not.HEAD, HEAD.dt)
(med.line.dt <- dcast(rbindlist(lapply(
  med.line.list,
  function(DT)DT[, c("Bisect", "CPU_year", "run", "rank", "variable", "value")])),
  Bisect + CPU_year + run + rank + variable ~ .,
  list(median, length)
  )[, age := Age("between")][])
ref.rank <- rank.dt[
  ref.dt,
  on="commit",
  nomatch=0L
]
(limit.dt <- data.table(seconds=tpred$seconds.limit))
med.points.N <- med.line.dt[variable=="log10_N"]

HEAD.N <- HEAD.dt[variable=="log10_N"]
HEAD.N.stats <- dcast(
  HEAD.N,
  Bisect ~ .,
  list(length, min, max),
  value.var="rank"
)[order(rank_min, rank_max)][]
Bisect.range <- nc::capture_first_df(
  HEAD.N.stats,
  Bisect=list(Rname_old=".*", "-", Rname_new=".*"))
for(age in c("old", "new")){
  set(Bisect.range, j=paste0("rank_", age), value=ref.dt[
    Bisect.range[[paste0("Rname_",age)]],
    rank.dt[commit, .(rank), on="commit"],
    on="Rname"])
}
(Bisect.ext <- melt(
  Bisect.range,
  measure.vars=measure(
    value.name,
    age=Age,
    pattern="(.*)_(old|new)")))

(bisect.dt <- rowwiseDT(
  Rname_old=, Rname_new=, label=, expected=, stage=,
  "PR4164", "Parent7401", "PR5463", "PR4558", "discovery",
  "PR4558", "Fast", "PR7401", "", "discovery",
  "Before", "Parent4558", "PR4164", "", "discovery",
  "PR4558", "Parent5463", "PR4491", "", "final",
  "PR4164", "Fast", "PR7401", "", "final",
  "PR4491", "Parent7401", "PR5463", "", "final",
  "PR4164", "Parent4491", "PR4558", "", "final"
)[
  Bisect.range, on=.(Rname_old, Rname_new)
][
  order(rank_old, rank_new)
][
, ord := seq_along(Bisect), by=stage
][
, y := 1300000*0.7^ord
][order(stage,-y)])
ci.refs <- c("Before", "PR4164", "PR4558", "Fast")
orig.points <- med.points.N[
  ref.rank[Rname %in% ci.refs],
  on="rank"]
show.points <- rbind(
  data.table(stage="discovery", orig.points)[, Bisect := "previous stage"],
  med.points.N[bisect.dt, on="Bisect"][stage!="final"][, let(Bisect="previous stage", stage="final")],
  med.points.N[bisect.dt, on="Bisect"],
  data.table(stage="CI", orig.points)[, Bisect := "previous stage"],
  fill=TRUE, use.names=TRUE)
vline.dt <- melt(bisect.dt, measure.vars=measure(value.name, age, pattern="(.*)_(old|new)"), value.name="Rname")
##> dput(scales::hue_pal(c(0,360), 100, 65, 0, 1)(7))
## dput(sort(bisect.dt$Bisect))
Bisect.colors <- c(
  "previous stage"="black",
  "Before-Parent4558"="#00BE72",
  "PR4164-Fast"="#D79000",
  "PR4164-Parent4491"="#6C9AFF", 
  "PR4164-Parent7401"="#FF6C91",
  "PR4491-Parent7401"="#00BCD6",
  "PR4558-Fast"="#81AD00",
  "PR4558-Parent5463"="#EE67EC")
(Bisect.sizes <- setNames(
  ifelse(names(Bisect.colors)=="previous stage", 2, 1),
  names(Bisect.colors)))
gg <- ggplot()+
  ggtitle("Discovering changes in performance, one test case for R data.table")+
  theme_bw()+
  scale_color_manual(
    "Computed in",
    values=Bisect.colors,
    limits=c("previous stage",bisect.dt$Bisect))+
  scale_size_manual(
    "Computed in",
    values=Bisect.sizes,
    limits=c("previous stage",bisect.dt$Bisect))+
  geom_vline(aes(
    xintercept=rank),
    color="grey",
    data=vline.dt)+
  geom_segment(aes(
    rank_old, y,
    color=Bisect,
    xend=rank_new, yend=y),
    linewidth=2,
    data=bisect.dt)+
  geom_text(aes(
    40, 1e6, label="Segments show bisect old/new commits"),
    size=3,
    data=data.frame(stage=c("discovery","final")))+
  geom_point(aes(
    rank, 10^value_median, color=Bisect, size=Bisect),
    data=show.points)+
  directlabels::geom_dl(aes(
    rank_new, 2e4, label=Rname_new),
    color="grey",
    method=list(cex=0.7, "bottom.polygons"),
    data=bisect.dt)+
  directlabels::geom_dl(aes(
    rank_old, 2e6, label=Rname_old),
    color="grey",
    method=list(cex=0.7, "top.polygons"),
    data=bisect.dt)+
  scale_y_log10(
    sprintf("N = throughput at %s seconds", paste(limit.dt$seconds)),
    limits=c(1e4, 4e6))+
  scale_x_continuous("Commit rank over time")+
  facet_grid(stage ~ ., labeller=label_both)
png("figure-story-PR7912.png", width=8, height=4, units="in", res=200)
print(gg)
dev.off()

