# install all packages:
# source("setup_install_all_packages.R") # in same directory

knitr::knit_hooks$set(size = function(before, options, envir) {
  if (before) {
    paste0("\\", options$size)
  } else {
    "\\normalsize"
  }
})
knitr::opts_chunk$set(echo = TRUE, size = "scriptsize", cache = TRUE,
                      background = "white", tidy = TRUE, fig.width = 8, fig.height = 6, 
                      out.width = ".9\\textwidth", 
                      out.height = ".8\\textheight",
                      fig.align= "center")

library(tidyverse)
library(purrr)

library(ggplot2)
library(patchwork)
# library(rayshader); library("ggrgl") #3d ggplots, pre-rendered bc too slow
library(ggrepel)
library(ggthemes)
library(GGally)
library(gridExtra)
library(cowplot)


# devtools::install_github("sachsmc/plotROC")
library(plotROC) 
library(maps)
library(mapproj)
library(hexbin)
library(colorspace)
library(viridisLite)
library(viridis)
options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)
scale_colour_discrete <- scale_colour_viridis_d
scale_fill_discrete <- scale_fill_viridis_d
options(digits = 2)

library(reshape2)
library(formatR)
library(datasauRus) #modern anscombe quartet
library(dslabs) #gapminder data
library(socviz)

library(MASS)
library(mvtnorm)
library(glmnet)
library(mgcv) # Pima Indian Diabetes model
library(energy) #distance corr
library(VGAM) #betabinom

##  def-parametverteilung
make_dist_data <- function(name, grid, params) {
  dens <- get(paste0("d",name), mode = "function")
  cdf <- get(paste0("p",name), mode = "function")
  data <- purrr::map2(params, seq_along(params), ~ {
    tibble(
      name = name,
      x = grid, 
      `f(x)` = do.call(dens, append(.x, list(x = grid))),
      `F(x)` = do.call(cdf, append(.x, list(q = grid))),
      param = as.character(.y),
      param_value = list(.x))
  }) |> bind_rows()
  data
}

make_dist_plot <- function(data, discrete) {
  clr_scale <- colorspace::scale_color_discrete_qualitative("Dark 3")
  
  dgeom <- if (discrete) geom_point else geom_line
  dens <-  ggplot(data) + dgeom(aes(x = x, y = `f(x)`, col = param)) +
    theme(legend.position = "none") + clr_scale
  
  pgeom <- if (discrete) geom_step else geom_line
  cdf   <-  ggplot(data) + pgeom(aes(x = x, y = `F(x)`, col = param)) +
    theme(legend.position = "none") + clr_scale + ylim(c(0, 1))
  
  if (discrete) {
    dens <- dens + 
      geom_segment(aes(x = x, xend = x, y = 0, yend =`f(x)`, col = param), alpha = .5, lwd = .2) + 
      scale_x_continuous()
    cdf <- cdf + geom_point(aes(x = x, y = `F(x)`, col = param), size = .5)
  }
  list(dens =  dens, cdf =  cdf)
}

distplot <- function(name, grid, params, discrete = TRUE) {
  data <- make_dist_data(name, grid, params)
  make_dist_plot(data, discrete)
}
if (FALSE) {
  p <- distplot("norm", seq(-3, 3, l = 105), 
                params = list(list(mean = 0, sd = 1), 
                              list(mean = 1, sd = 2)), 
                discrete = FALSE)
  
  p <- distplot("geom", 0:30, 
                params = list(list(prob = 0.1), 
                              list(prob = .8), 
                              list(prob = .4)), 
                discrete = TRUE)
  p[[1]] + p[[2]]
}


