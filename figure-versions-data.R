edit.data.table = function(old.Package, new.Package, sha, new.pkg.path) {
  pkg_find_replace <- function(glob, FIND, REPLACE) {
    atime::glob_find_replace(file.path(new.pkg.path, glob), FIND, REPLACE)
  }
  Package_regex <- gsub(".", "([_.]?)", old.Package, fixed = TRUE)
  Package_ <- gsub(".", "\\1", old.Package, fixed = TRUE)
  new.Package_ <- paste0(Package_, "\\1", sha)
  pkg_find_replace(
    "DESCRIPTION",
    paste0("Package:\\s+", old.Package),
    paste("Package:", new.Package))
  pkg_find_replace(
    file.path("src", "Makevars.*in"),
    Package_regex,
    new.Package_)
  pkg_find_replace(
    file.path("R", "onLoad.R"),
    Package_regex,
    new.Package_)
  pkg_find_replace(
    file.path("src", "init.c"),
    paste0("R_init_", Package_regex),
    paste0("R_init_", gsub("[.]", "_", new.Package_)))
  # require C<23 for empty prototype declarations to work, #7689
  descfile = file.path(new.pkg.path, "DESCRIPTION")
  desc = as.data.frame(read.dcf(descfile))
  desc$SystemRequirements = paste(
    c(desc$SystemRequirements, "USE_C99"),
    collapse = "; ")
  write.dcf(desc, descfile)
  # allow compilation on new R versions where 'Calloc' is not defined
  pkg_find_replace(
    file.path("src", "*.c"),
    "\\b(Calloc|Free|Realloc)\\b",
    "R_\\1")
  pkg_find_replace(
    "NAMESPACE",
    sprintf('useDynLib\\("?%s"?', Package_regex),
    paste0('useDynLib(', new.Package_))
  pkg_find_replace(
    file.path("src", "Makevars.*in"),
    "@PKG_CFLAGS@", "@PKG_CFLAGS@ -DSTRING_PTR_RO=STRING_PTR_RO")
  backports = c(
    "src/data.table.h" = '
        #include <Rversion.h>
        #if R_VERSION >= R_Version(4, 6, 0)
        // backports.c
        void SETLENGTH(SEXP x, R_xlen_t n);
        R_xlen_t TRUELENGTH(SEXP x);
        void SET_TRUELENGTH(SEXP x, R_xlen_t n);
        void SET_GROWABLE_BIT(SEXP);
        int LEVELS(SEXP);
        int NAMED(SEXP);
        #define REFCNT(x) NAMED(x)
        SEXP ATTRIB(SEXP);
        void SET_ATTRIB(SEXP, SEXP);
        int OBJECT(SEXP);
        void SET_OBJECT(SEXP, int);
        #define isFrame(x) isDataFrame(x)
        #define GetOption(x, none) GetOption1(x)
        #undef findVar // Rf_ mapping remains
        #define findVar(sym, env) R_getVar(sym, env, FALSE)
        #define STRING_PTR(x) ((SEXP *)STRING_PTR_RO(x))
        int IS_S4_OBJECT(SEXP);
        void SET_S4_OBJECT(SEXP);
        void UNSET_S4_OBJECT(SEXP);
        void SET_TYPEOF(SEXP, int);
        #define VECTOR_ELT(x, i) VECTOR_ELT_(x, i)
        SEXP VECTOR_ELT_(SEXP, R_xlen_t);
        #define VECTOR_PTR(x) ((SEXP*)DATAPTR_RO(x))
        #define DATAPTR(x) ((void*)DATAPTR_RO(x))
        #endif
      ',
    "src/backports.c" = '
        #include "data.table.h"
        #if R_VERSION >= R_Version(4, 6, 0)
        #define NAMED_BITS 16
        struct sxpinfo_struct {
          SEXPTYPE type      :  TYPE_BITS; // in Rinternals.h
          unsigned int scalar:  1;
          unsigned int obj   :  1;
          unsigned int alt   :  1;
          unsigned int gp    : 16;
          unsigned int mark  :  1;
          unsigned int debug :  1;
          unsigned int trace :  1;
          unsigned int spare :  1;
          unsigned int gcgen :  1;
          unsigned int gccls :  3;
          unsigned int named : NAMED_BITS;
          unsigned int extra : 32 - NAMED_BITS;
        };
        struct vecsxp_struct {
          R_xlen_t length;
          R_xlen_t truelength;
        };
        typedef struct VECTOR_SEXPREC {
          struct sxpinfo_struct sxpinfo;
          SEXP attrib;
          SEXP gengc_next_node, gengc_prev_node;
          struct vecsxp_struct vecsxp;
        } *VECSEXP;
        void SETLENGTH(SEXP x, R_xlen_t n) {
          ((VECSEXP)x)->vecsxp.length = n;
        }
        R_xlen_t TRUELENGTH(SEXP x) {
          return ((VECSEXP)x)->vecsxp.truelength;
        }
        void SET_TRUELENGTH(SEXP x, R_xlen_t n) {
          ((VECSEXP)x)->vecsxp.truelength = n;
        }
        void SET_GROWABLE_BIT(SEXP x) {
          ((VECSEXP)x)->sxpinfo.gp |= 0x20;
        }
        int LEVELS(SEXP x) {
          return ((VECSEXP)x)->sxpinfo.gp;
        }
        int NAMED(SEXP x) {
          return ((VECSEXP)x)->sxpinfo.named;
        }
        int OBJECT(SEXP x) {
          return ((VECSEXP)x)->sxpinfo.obj;
        }
        void SET_OBJECT(SEXP x, int o) {
          ((VECSEXP)x)->sxpinfo.obj = o;
        }
        SEXP ATTRIB(SEXP x) {
          return ((VECSEXP)x)->attrib;
        }
        void SET_ATTRIB(SEXP x, SEXP att) {
          ((VECSEXP)x)->attrib = att;
        }
        #define S4_OBJECT (1<<4)
        int IS_S4_OBJECT(SEXP x) {
          return ((VECSEXP)x)->sxpinfo.gp & S4_OBJECT;
        }
        void SET_S4_OBJECT(SEXP x) {
          ((VECSEXP)x)->sxpinfo.gp |= S4_OBJECT;
        }
        void UNSET_S4_OBJECT(SEXP x) {
          ((VECSEXP)x)->sxpinfo.gp &= ~S4_OBJECT;
        }
        void SET_TYPEOF(SEXP x, int type) {
          ((VECSEXP)x)->sxpinfo.type = type;
        }
        SEXP VECTOR_ELT_(SEXP x, R_xlen_t i) {
          return ALTREP(x) ? (VECTOR_ELT)(x, i) : ((SEXP*)DATAPTR_RO(x))[i];
        }
        #endif
      ')
  for (n in names(backports)) {
    f = file(file.path(new.pkg.path, n), "a")
    writeLines(backports[[n]], f)
    close(f)
  }
}

tlist <- list(
  "DT[by] fixed in #4558" = atime::atime_test(
    N=2^seq(1, 30),
    seconds.limit=0.1,
    setup = {
      N9 <- as.integer(N * 0.9)
      d <- data.table(
        id = sample(c(seq.int(N9), sample(N9, N-N9, TRUE))),
        v1 = sample(5L, N, TRUE),
        v2 = sample(5L, N, TRUE)
      )
    },
    Before = "166e0bab317e9d9f4f1844997a315e6482ab5fd9",#a few click of parent up from first commit in PR4164 gets to https://github.com/Rdatatable/data.table/commit/166e0bab317e9d9f4f1844997a315e6482ab5fd9
    PR4164="4aadde8f5a51cd7c8f3889964e7280432ec65bbc", # Merge commit of the PR that causes the regression.
    ##Parent4558="6f360be0b2a6cf425f6df751ca9a99ec5d35ed93", #parent of PR4558 (https://github.com/Rdatatable/data.table/commit/ba32f3cba38ec270587e395f6e6c26a80be36be6)
    PR4558 = "ba32f3cba38ec270587e395f6e6c26a80be36be6", # Merge commit of the PR (https://github.com/Rdatatable/data.table/pull/4558) that fixes the regression
    ##Parent4491="3fa8b20435d33b3d4b5c26fd9b0ac14c10b98800", # PR4491 is bisect result of PR4164 and Parent5463.
    PR4491="85adf09e3463838d547977ae9bc75e3b37f9cbaf",
    ##Parent5463="052f8da6", # PR5463 is bisect result of PR4164 and Parent7401.
    PR5463="19b7866112614db53eb3e909c097407d91cd6738",
    ##Parent7401="20b463c9592c07d05ad2e89a25f84f0b998d6154", # master parent of the PR7401 which caused the recent speedup.
    PR7401="0216983c51e03e3f61d5e6f08f4ba0c42cceb22c",
    ##Fast="a3eeb4c0caec51afd66bdf9bbc16817e12a47a40", #master 10 Sept 2026
    expr = data.table:::`[.data.table`(d, , max(v1) - min(v2), by = id)),
  "fread N=cols regression" = atime::atime_test(
    N = unique(as.integer(2^seq(1, 20, by=0.25))), # finer N grid for git bisect.
    setup={
      set.seed(1)
      n.rows <- 100
      input.vec <- rnorm(n.rows*N)
      input.mat <- matrix(input.vec, n.rows, N)
      input.dt <- data.table(input.mat)
      input.csv <- tempfile()
      data.table::fwrite(input.dt, input.csv)
    },
    seconds.limit=0.1,
    Fast="1.14.8",
    PR7375="59f966cf6839a0494e42ed1ab98616eea03dcb36", #fread slowdown added comment.char arg
    ##Parent7370="1685a3b47d48f323afeae589545f7cbb7717ec28",
    PR7370="df7fa8071c6b818d24e47cc1a84a6dc0491550d6", #fread slowdown sep detection single column quoted
    ##Slow="1.18.6",
    expr=data.table::fread(input.csv, showProgress = FALSE, nThread=1)))
atime.results <- list()
for(Test in names(tlist)){
  some.args <- tlist[[Test]]
  targs <- c(
    some.args,
    pkg.edit.fun=edit.data.table,
    pkg.path="~/R/data.table")
  atime.results[[Test]] <- do.call(atime::atime_versions, targs)
}
plot(atime.results[[1]])
plot(atime.results[[2]])
saveRDS(atime.results, "figure-versions-data.rds")
