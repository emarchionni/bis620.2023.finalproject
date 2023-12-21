#' Histogram of the high housing cost burden percentage vs the most represented race for each county
#' @description
#' The function returns histograms of the data set SVI_2020_US_county according to the formula EP_HBURD ~ F_RACE
#'
#' @param data SVI_2020_US_county data set filtered on the F_RACE label
#' @returns A ggplot object with the histogram
#'
#' @importFrom ggplot2 ggplot geom_histogram aes theme_minimal
#' @importFrom utils data
#' @export

histogram_F_RACE <- function(data) {
  ggplot(data, aes(x = EP_HBURD)) +
    geom_histogram() +
    theme_minimal()
}
