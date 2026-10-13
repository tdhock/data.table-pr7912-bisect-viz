library(data.table)
src.dir <- normalizePath("~/R/data.table/.ci/atime/bisect-2s-lim0.1/DT_by__fixed_in__4558")
(data.dir <- file.path("data", system("hostname", intern=TRUE), basename(dirname(src.dir))))
unlink(data.dir, recursive = TRUE)
dir.create(data.dir, showWarnings = FALSE, recursive = TRUE)
file.copy(src.dir, data.dir, recursive = TRUE)
ref.vec <- c(
  Before = "166e0bab317e9d9f4f1844997a315e6482ab5fd9",#a few click of parent up from first commit in PR4164 gets to https://github.com/Rdatatable/data.table/commit/166e0bab317e9d9f4f1844997a315e6482ab5fd9
  PR4164="4aadde8f5a51cd7c8f3889964e7280432ec65bbc", # Merge commit of the PR that causes the regression.
  Parent4558="6f360be0b2a6cf425f6df751ca9a99ec5d35ed93", #parent of PR4558 (https://github.com/Rdatatable/data.table/commit/ba32f3cba38ec270587e395f6e6c26a80be36be6)
  PR4558 = "ba32f3cba38ec270587e395f6e6c26a80be36be6", # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/4558) that fixes the regression
  Parent4491="3fa8b20435d33b3d4b5c26fd9b0ac14c10b98800", # PR4491 is bisect result of PR4164 and Parent5463.
  PR4491="85adf09e3463838d547977ae9bc75e3b37f9cbaf",
  Parent5463="052f8da6", # PR5463 is bisect result of PR4164 and Parent7401.
  PR5463="19b7866112614db53eb3e909c097407d91cd6738",
  Parent7401="20b463c9592c07d05ad2e89a25f84f0b998d6154", # master parent of the PR7401 which caused the recent speedup.
  PR7401="0216983c51e03e3f61d5e6f08f4ba0c42cceb22c",
  Fast="master")
(ref.dt <- data.table(
  Rname=names(ref.vec),
  ref=ref.vec
)[
, gert::git_log(ref, 1, repo="~/R/data.table")
, by=.(Rname, ref)][, .(Rname, commit, time)])
fwrite(ref.dt, "refs.csv")
