################################################################################
#
#'
#' Function to calculate the design effect of a stepped wedge cluster randomised
#' study.
#'
#' @param k Number of steps in the stepped wedge study
#' @param b Number of baseline measurements
#' @param t Number of measurements after each step
#' @param icc Intracluster correlation coefficient (\code{ICC})
#' @param size cluster size
#'
#' @return A numeric value of the design effect of a stepped wedge cluster
#'     ranomised study
#'
#' @examples
#'
#'   get_deff_swedge(k = 2, b = 1, t = 1, icc = 0.034, size = 192)
#'
#' @export
#'
#
################################################################################

get_deff_swedge <- function(k, b, t, icc, size) {

  deff <- ((1 + icc * (k * t * size + b * size - 1)) / (1 + icc * (0.5 * (k * t * size) + b * size - 1))) * ((3 * (1 - icc)) / ((2 * t) * (k - 1 / k)))

  return(deff)
}


################################################################################
#
#'
#' Function to calculate sample size requirements for a stepped wedge cluster
#' randomised study.
#'
#' @param z1 The z-score indicating level of significance (confidence interval).
#'     Default at 1.96 for 95\% confidence interval.
#' @param z2 desired power. Default at 0.84 for 80\% power.
#' @param p1 Expected proportion for control group.
#' @param p2 Expected proportion for intervention group
#' @param deff Design effect of a stepped wedge cluster randomised study
#'
#' @return A numeric value of sample size needed for a stepped wedge cluster
#'     ranomised study
#'
#' @examples
#'
#'  get_ss_swedge(z1 = 1.96, z2 = 0.84,
#'                p1 = 0.2, p2 = 0.15,
#'                deff = get_deff_swedge(k = 2, b = 1, t = 1,
#'                                       icc = 0.034, size = 192))
#'
#' @export
#'
#
################################################################################

get_ss_swedge <- function(z1 = 1.96, z2 = 0.84, p1, p2, deff) {

  n1 <- ((z1 + z2) ^ 2) * ((p1 * (1 - p1)) + (p2 * (1 - p2))) / (p1 - p2) ^ 2

  n <- n1 * 2 * deff

  return(n)
}
