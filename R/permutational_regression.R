#' Permutational F-type test for regression of the high housing cost burden percentage vs the most represented race for each county
#' @description
#' The function performs a permutational F-type test for the linear fit according to the formula EP_HBURD ~ EP_WHITE, EP_AFAM, EP_HISP, EP_ASIAN, EP_NHPI of the data set SVI_2020_US_county.
#' The test is
#' H0: beta1 = beta2 = ... = beta5 = 0 i.e. no group specific effect
#' H1: (H0)^c i.e there is at least one nonzero parameter
#'
#' @param data SVI_2020_US_county dataset filtered on the F_RACE label
#' @param B number of permutations
#' @returns a list with the T0 statistic, the permutational sample and the pvalue of the test
#'
#' @importFrom dplyr pull mutate
#' @importFrom stats lm
#' @export
#'

permutational_regression <- function(data, B = 1000){


  regression <- lm(EP_HBURD ~
                     EP_WHITE +
                     EP_AFAM +
                     EP_HISP +
                     EP_ASIAN +
                     EP_NHPI, data = data)
  n <- data |>
    nrow()

  response <- data |>
    pull(EP_HBURD)


  T0_glob <- summary(regression)$f[1]


  T_H0glob <- numeric(B)

  for(perm in 1:B){

    train_copy <- data

    permutation <- sample(n)

    response_perm <- response[permutation]

    train_copy <- data |>
      mutate(response_perm = response_perm)

    T_H0glob[perm] <- summary(
      lm(response_perm ~
           EP_WHITE +
           EP_AFAM +
           EP_HISP +
           EP_ASIAN +
           EP_NHPI, data = train_copy))$f[1]

  }


  # p-value
  p_val <- sum(T_H0glob>=T0_glob)/B
  p_val

  return(list("T0" = T0_glob,
              "T_permutational_distribution" = T_H0glob,
              "Pvalue" = p_val))

}
