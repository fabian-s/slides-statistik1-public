# usage: Rscript slides/render-intro.R <file.Rmd>  (run from repo root)
args <- commandArgs(trailingOnly = TRUE)
setwd(file.path(
  dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))),
  "."
))
for (f in args) rmarkdown::render(f, quiet = TRUE)
