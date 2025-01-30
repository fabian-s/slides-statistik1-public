library(DescTools)

# compute chi-square distance and (corrected) contingency coefficient
# arguments: 
#   table: a 2D table of absolute frequencies
#   print_stuff: boolean flag -- print intermediate results to console or not
# output: 
#  double(3) containing the 3 statistics for the table (rounded, 2 decimals)
chisq_stats <- function(table, print_stuff = TRUE){
  # check args:
  stopifnot(
    inherits(table, "matrix"),
    !any(is.na(table)),
    length(dim(table)) == 2
  )
  
  sample_size <- sum(table)   # n 
  row_freqs <- rowSums(table) # h_i., i = 1,...,k 
  col_freqs <- colSums(table) # h_.j, j = 1,...,m
  expected_freqs <- outer(row_freqs, col_freqs) /  sample_size  #~h_ij 
  
  chisq_dist <- 
    {(table - expected_freqs)^2 / expected_freqs} |>  # (h_ij - ~h_ij)^2/~h_ij
    sum()
  contingency_coef <- sqrt(chisq_dist / (chisq_dist + sample_size)) # K
  
  table_size <- min(nrow(table), ncol(table))
  correction_factor <-  
    ((table_size - 1) / table_size) |> 
    sqrt() # sqrt((min(k, m) - 1)/min(k, m))  
  corrected_contingency_coef <- contingency_coef / correction_factor #K*
  
  if (print_stuff) {
    cat("\n    h_i.:\n")
    print(row_freqs)
    cat("\n    h_.j:\n")
    print(col_freqs)
    cat("\n   h_ij:\n")
    print(table)
    cat("\n   expected ~h_ij under independence:\n") 
    print(round(expected_freqs, 2))
    cat("\n")
  }
  
  c("chisq" = round(chisq_dist, 2), 
    "K"     = round(contingency_coef, 2), 
    "K*"    = round(corrected_contingency_coef, 2))
}

# compute odds ratio
# arguments: 
#   table: a 2x2 table of absolute frequencies
# output: double(1) containing odds ratio
odds_ratio <- function(table) {
  stopifnot(all.equal(dim(table), c(2, 2)))
  
  # (h_11 * h_22) / (h_21 * h_12)
  (table[1,1] * table[2,2])/(table[2,1] * table[1,2]) 
}

# -----------------------------------------------------------------------------

#' ## Example: Sex and Class and Survival on the `Titanic`

#' Titanic data:
str(Titanic)

#' marginal freq.s:  survival ~ class
surv_class <- apply(Titanic, MAR = c(4, 1), FUN = sum)
chisq_stats(surv_class)

#' verify that we implemented K* correctly by comparing with a reference
#' implementation from the DescTools package:
DescTools::ContCoef(surv_class, correct = TRUE) # approx 0.4 -- yay!

#' marginal freq.s:  survival ~ sex
surv_sex <- apply(Titanic, MAR = c(4, 2), FUN = sum)
chisq_stats(surv_sex)

#' $\implies$ 
#' stronger marginal dependency between sex and survival than between sex and class

# -----------------------------------------------------------------------------

#' What if we condition on class, i.e.:  
#' look at strength of dependency between sex and survival for each class separately.  
#' First we need the marginal conditional freq.s for  survival ~ sex | class:
(surv_sex_class <- apply(Titanic, MAR = c(4, 2, 1), FUN = sum))

#' Visualize this complicated 3D joint distribution:
mosaicplot(~ Class + Sex + Survived , data = Titanic, main = "Titanic", color = TRUE)

chisq_stats(surv_sex_class[, , "1st"], print = FALSE)
chisq_stats(surv_sex_class[, , "2nd"], print = FALSE)
chisq_stats(surv_sex_class[, , "3rd"], print = FALSE)
chisq_stats(surv_sex_class[, , "Crew"], print = FALSE)
#' $\implies$
#' sex-survival dependency ordered by decreasing strength as measured by $K^*$:  
#' 2nd > 1st > 3rd > Crew

odds_ratio(surv_sex_class[, , "1st"])
odds_ratio(surv_sex_class[, , "2nd"])
odds_ratio(surv_sex_class[, , "3rd"])
odds_ratio(surv_sex_class[, , "Crew"])
#' $\implies$
#' sex-survival dependency ordered by decreasing strength as measured by OR $\gamma$:  
#' 1st > 2nd > Crew > 3rd (!)
#' 
#' What's going on, why the discrepancy between OR $\gamma$ and $K^*$?
#' 
#' - $\chi^2$-distance and derived quantities $K$, $K^*$ are affected by
#' the marginal distributions of the sample: The more unbalanced the marginal
#' distributions are, the higher the observed OR needs to be to reach the same
#' value for $K^*$ as for a more balanced sample. In a nutshell, strong
#' deviations from expected counts under independence in subpopulations with
#' small counts might just be due to chance and receive relatively less weight, by
#' construction of the $\chi^2$- statistic. 
#' - OR, in contrast, simply reports the observed relative difference in the odds.
