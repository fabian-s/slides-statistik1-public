libs <- c(
  "ggplot2",
  "ggrepel",
  "grid",
  "gridExtra",
  "rayshader",
  "patchwork",
  "viridisLite",
  "colorspace",
  "dslabs",
  "socviz",
  "datasauRus",
  "dplyr",
  "tidyr",
  "exams",
  "here",
  "remotes"
)

libs2 <- c(
  "ggthemes",
  "GGally",
  "cowplot",
  "plotROC",
  "maps",
  "mapproj",
  "hexbin",
  "viridis",
  "reshape2",
  "formatR",
  "mvtnorm",
  "glmnet",
  "VGAM",
  "gridGraphics"
)

libs_gh <- c("fabian-s/energy") # CRAN version needs GSL which is a pain to install so I forked it and removed the GSL dep.

for (l in libs) {
  inst.p <- installed.packages()
  if (!(l %in% inst.p)) {
    install.packages(l)
  }
}

for (l in libs2) {
  inst.p <- installed.packages()
  if (!(l %in% inst.p)) {
    install.packages(l)
  }
}

for (l in libs_gh) {
  inst.p <- installed.packages()
  if (!(l %in% inst.p)) {
    remotes::install_github(l)
  }
}


lapply(X = c(libs, libs2), FUN = require, character.only = TRUE)
