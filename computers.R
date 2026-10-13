library(ggplot2)
library(data.table)
details.dt.list <- list()
dot.dt.list <- list()
common.dt.list <- list()
results.dt.list <- list()
(Bisect.dir.vec <- Sys.glob("data/*/bisect-2s-lim0.1/DT_by__fixed_in__4558/*"))
age.colors <- c(
  old="violet",
  between="blue",
  new="red")
Age <- function(age)factor(age, names(age.colors))
for(Bisect.dir in Bisect.dir.vec){
  Bisect <- function(dt)dt[, let(
    Bisect = basename(Bisect.dir),
    host_year = ifelse(
      grepl("dinf", basename(dirname(dirname(dirname(Bisect.dir))))),
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
(results.dt <- rbindlist(results.dt.list)[, let(
  Status = St(status)
)][])

to.min <- function(x, start)(x-min(start))/60
timeline.dt <- results.dt[
, .SD[1]
, by=.(Bisect, commit, start, end)
][order(start)][
, bisect := factor(Bisect, unique(Bisect))
][, let(
  rstart=to.min(start, start),
  rend=to.min(start, end)
), by=host_year][]
gg <- ggplot()+
  theme_bw()+
  geom_segment(aes(
    rstart, bisect,
    xend=rend, yend=bisect),
    size=1,
    data=timeline.dt)+
  geom_point(aes(
    rstart, bisect),
    shape=1,
    data=timeline.dt)+
  scale_y_discrete(
    "Bisect run (old-new)")+
  scale_x_continuous(
    "Minutes from start of first git bisect (dot at start of each version tested)")+
  facet_grid(host_year ~ ., labeller=label_both)
png("computers-PR7912-minutes.png", width=8, height=3, units="in", res=200)
print(gg)
dev.off()


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
    results.rank[version!="HEAD"][, .(host_year, Rname=version, variable, value)],
    on="Rname"],
  on="commit"]
med.line.list <- list(not.HEAD, HEAD.dt)
(med.line.dt <- dcast(rbindlist(lapply(
  med.line.list,
  function(DT)DT[, c("host_year", "rank", "variable", "value")])),
  host_year + rank + variable ~ .,
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
][!c("Parent4558","PR7401"), on="Rname"][, y := 6.2-.I*0.3]
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
    rank, 10^value_median, color=host_year),
    data=med.line.dt[variable=="log10_N"])+
  geom_text(aes(
    rank, 10^y, label=sprintf(
      "%s\n%s", Rname, strftime(time, "%Y-%m-%d")),
      hjust=ifelse(grepl("Parent|Fas",Rname), 1, 0)),
    data=ref.rank,
    vjust=0,
    size=3,
    color="grey50")+
  scale_y_log10(sprintf("N = throughput at %s seconds", paste(limit.dt$seconds)))+
  scale_x_continuous("Commit rank over time")
(dl <- directlabels::direct.label(gg, "right.polygons"))
png("computers-PR7912-timeline.png", width=8, height=4, units="in", res=200)
print(dl)
dev.off()
