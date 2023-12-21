#' Boxplot of the high housing cost burden percentage vs the most represented race for each county
#' @description
#' The function returns boxplots of the dataset SVI_2020_US_county according to the formula EP_HBURD ~ F_RACE
#'
#' @param data SVI_2020_US_county dataset
#' @returns A ggplot object with the boxplot
#'
#' @importFrom ggplot2 ggplot geom_boxplot aes theme_classic
#' @importFrom utils data
#' @export

boxplot_F_RACE <- function(data) {
  ggplot(data, aes(x = F_RACE, y = EP_HBURD, fill = F_RACE)) +
    geom_boxplot() +
    theme_classic()
}


