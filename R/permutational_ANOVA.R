#' Permutational ANOVA of the high housing cost burden percentage vs the most represented race for each county
#' @description
#' The function performs a permutational ANOVA of the data set SVI_2020_US_county according to the formula EP_HBURD ~ F_RACE.
#' The test is
#' H0: tau1 = tau2 = tau3 = 0 i.e. no group specific effect
#' H1: (H0)^c i.e there are at least two different populations
#'
#' @param data SVI_2020_US_county dataset filtered on the F_RACE label
#' @param B number of permutations
#' @returns a list with the T0 statistic, the permutational sample and the pvalue of the test
#'
#' @importFrom dplyr pull
#' @importFrom stats aov
#' @export
#'

permutational_ANOVA <- function(data, B = 1000){

  n <- data |>
    nrow()

  response <- data |>
    pull(EP_HBURD)

  grouping <- data |>
    pull(F_RACE)

  fit <- aov(EP_HBURD ~ F_RACE, data = data)
  T0 <- summary(fit)[[1]][1,4]


  T_stat <- numeric(B)

  for(perm in 1:B){
    # Permutation:
    permutation <- sample(1:n)
    response_perm <- response[permutation]
    fit_perm <- aov(response_perm ~ grouping)

    # Test statistic:
    T_stat[perm] <- summary(fit_perm)[[1]][1,4]
  }


  # p-value
  p_val <- sum(T_stat>=T0)/B
  p_val

  return(list("T0" = T0,
              "T_permutational_distribution" = T_stat,
              "Pvalue" = p_val))

}
