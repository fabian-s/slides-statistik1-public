(observed <- matrix(c(19, 43, 18, 20), 2, 2))

mosaicplot(t(observed))

(expected <- tcrossprod(rowSums(observed), 
                        colSums(observed)) / 100) 

(observed - expected)^2

(chisq <- sum( (observed - expected)^2 / expected ))

(k <- sqrt(chisq / (chisq + sum(observed))))

(k_star <- k / sqrt(1/2))

stats::chisq.test(observed, correct  =FALSE)

DescTools::ContCoef(observed)

