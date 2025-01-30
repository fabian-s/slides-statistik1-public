#' Experiment: how do OR and K* behave for changing data size, marginal imbalance, odds and odds ratio?

library(tidyverse)
source("chisquare-titanic.R", echo = FALSE)

# generate random 2x2 table for given sample size <n>, marginal distribution over <rows>,
# <odds> for y=1 vs y=2 | x=1 and odds ratio <or>.
# (sets h11 to expected count under independence and computes others based on that)
generate_2x2_table <- function(n = 100, f_x1 = .5, odds_x1 = 2, or = 1) {
  h_x1 <- f_x1 * n #h_1.
  h_11 <- h_x1 / (1 + 1/odds_x1)
  h_12 <- h_x1 - h_11
  odds_x2 <- 1/or * odds_x1
  h_x2 <- n - h_x1 
  h_21 <- h_x2 / (1 + 1/odds_x2)
  h_22 <- h_x2 - h_21
  
  matrix(c(h_11, h_21, h_12, h_22), 2, 2) |> round()
}

settings <- expand.grid(
  n = c(100, 1000, 10000, 1000000),
  f_x1 = seq(.05, .95, by = .05),
  odds = seq(1, 19, by = 2),
  or = c(1, 1.25, 1.5, 2, (2:7)^2)
) 

or_vs_kstar <- settings |> rowwise() |>  
  mutate(
    table = list(
      generate_2x2_table(n, f_x1 = f_x1, odds = odds, or = or)
    ),
    observed_or = odds_ratio(table),
    Kstar = chisq_stats(table, print = FALSE)["K*"]
  ) 

#sanity check: are we generating the right data? specified and observed OR 
#should agree for sufficiently large samples / balanced data...
ggplot(or_vs_kstar,
       aes(x = or, y = observed_or, color = ordered(odds), group = odds)) +
  geom_abline(color = "darkgrey") +
  geom_line() + 
  facet_grid(n ~ f_x1, labeller = label_both)

# now for the actual experiment:
ggplot(or_vs_kstar,
       aes(x = observed_or, y = Kstar, color = ordered(odds), group = odds)) +
  geom_line() + 
  facet_grid(n ~ f_x1, labeller = label_both) + 
  scale_x_log10() +
  labs(x = expression("observed OR"~gamma), 
       y = expression(K^"*"))
