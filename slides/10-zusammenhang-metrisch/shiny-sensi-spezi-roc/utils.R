library(tidyverse)
library(plotROC) # via devtools::install_github("sachsmc/plotROC")
library(grid)
library(gridExtra)
library(gtable)
library(patchwork)

# create data for ROC viz
# n_pos, n_neg: no. of cases, controls
# mean_diff, sd_pos, sd_neg: mean difference and group-wise standard deviations 
#   of diagnostic score
# cutoff: threshold used for classification by diagnostic score
make_roc_data <- function(n_pos= 50, n_neg = 200, 
                          mean_diff = 2, sd_pos = 1, sd_neg = 1,
                          cutoff = 1.7, seed = NULL) {
  if (!is.null(seed)) set.seed(as.integer(seed))
  data <- tibble(Score = c(scale(rnorm(n_pos)) * sd_pos + mean_diff,
                           scale(rnorm(n_neg)) * sd_neg),
                 Status = ordered(c(rep("krank", n_pos), 
                                    rep("gesund", n_neg)), 
                                  c("gesund", "krank")))  %>%
    mutate(Prognose = ordered(c("<\"gesund\">", "<\"krank\">"))[(Score > cutoff) + 1],
           Ergebnis = ordered(paste0(Prognose, Status))) %>%
    mutate(Ergebnis = 
             recode_factor(Ergebnis, 
                           `<"gesund">gesund` = "wahr negativ",
                           `<"gesund">krank` = "falsch negativ",
                           `<"krank">gesund` = "falsch positiv",
                           `<"krank">krank` = "wahr positiv"),
           #make sure all levels are always present:
           Ergebnis = ordered(Ergebnis, 
                              levels = c("wahr negativ", "falsch negativ", 
                                         "falsch positiv", "wahr positiv")),
           Prognose = recode_factor(Prognose, 
                                    `<krank>` = "Diagnose: <\"krank\">",
                                    `<gesund>` = "Diagnose: <\"gesund\">",
                                    .ordered = TRUE))
  data
}

# ROC curve viz
roc_plot <- function(data, cutoff, roclabels = 3.5, th_label_size = 4) {
  
  table1 <- with(data, table(Prognose, Status))
  table2 <- t(t(c(Sensitivität = prop.table(table1, margin = 2)[2,2],
                  Spezifität = prop.table(table1, margin = 2)[1,1],
                  `pos. prädiktiver Wert` = 
                    prop.table(table1, margin = 1)[2,2],
                  `neg. prädiktiver Wert` = 
                    prop.table(table1, margin = 1)[1,1])))
  
  p1 <- ggplot(data, aes(y = Score, x = Status)) +
    coord_flip(clip = 'off', xlim = c(0.5, 2.5)) +
    geom_hline(yintercept = cutoff, col = "gold", size = 1) +
    geom_boxplot(outlier.size = 0, outlier.color = NA) +
    geom_point(aes(shape = Ergebnis, col = Ergebnis),
               position = position_dodge2(w = .15), alpha = .7) +
    scale_color_manual("Ergebnis", 
                       values = rev(c(`wahr negativ` = alpha("#CA0020", .6),
                                  `falsch negativ` = alpha("#F4A582", .6),
                                  `falsch positiv` = alpha("#92C5DE", .6),
                                  `wahr positiv` = alpha("#0571B0", .6)))) +
    scale_x_discrete(limits = rev(levels(data$Status))) +
    scale_shape_manual("Ergebnis", values = rev(c(`wahr negativ` = 16,
                                      `falsch negativ` = 17,
                                      `falsch positiv` = 16,
                                      `wahr positiv` = 17))) +
    annotate(y = cutoff, x = 0, label = "Schwellenwert", geom = "text",
             vjust = 1.3, hjust = 0.5, col = "gold", size = th_label_size) +
    
    theme(legend.position = "right") + labs(subtitle = "Daten") +
    guides(color = guide_legend(reverse = TRUE),
           shape = guide_legend(reverse = TRUE)) 

  gt1 <- tableGrob(format(table1, digits = 2), theme = ttheme_minimal())
  gt2 <- tableGrob(format(table2, digits = 2), theme = ttheme_minimal())
  gt <- gtable(widths = unit(1, 'null'), heights = unit(c(1, 2), 'null')) %>% 
    gtable_add_grob(gt1, 1, 1) %>% 
    gtable_add_grob(gt2, 2, 1)
  
  data$Status_num <- 1 * (data$Status == "krank")
  p2 <- ggplot(data, aes(d = Status_num, m = Score)) +
    geom_roc(pointsize = 0, n.cuts = 4, labelsize = roclabels) +
    labs(x = "1-Spezifität (falsch positiv)", 
         y = "Sensitivität (wahr positiv)") +
    annotate(x = 1 - table2[2, 1], y = table2[1, 1], 
             col = "gold", size = 3, geom = "point") +
    geom_abline(slope = 1, intercept = 0) +
    labs(subtitle = "ROC Kurve") 
 
  p1 / (p2 + gt)
}
