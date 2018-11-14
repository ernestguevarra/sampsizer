################################################################################
#
#' get_k_roc
#'
#' Function to calculate ratio of cases to non-cases in the sample for an ROC
#' analysis
#'
#' @param p Prevalence or proportion of cases in the relevant population
#'
#' @result A numeric value of the ratio of cases to non-cases in the sample
#'
#' @examples
#'
#'   get_k_roc(p = 0.5)
#'
#' @source Obuchowski NA. Fundamentals of clinical research for radiologists:
#'   ROC Analysis. American Journal of Roentgenology. 2005;184.
#'
#' @export
#'
#
################################################################################

get_k_roc <- function(p) {
  k <- (1 - p) / p
  return(k)
}


################################################################################
#
#' get_a_roc
#'
#' Function to calculate the A binormal distribution parameter
#'
#' @param AUC Area under the ROC curve. Ranges from greater than 0.5 to 1.
#'
#' @result A numeric value of the A binormal distribution parameter
#'
#' @examples
#'
#'   get_a_roc(AUC = 0.6)
#'
#' @source Obuchowski NA. Fundamentals of clinical research for radiologists:
#'   ROC Analysis. American Journal of Roentgenology. 2005;184.
#'
#' @export
#'
#
################################################################################

get_a_roc <- function(AUC) {
  ## Check that AUC is greater than 0.5 and up to 1
  if(AUC <= 0.5 | AUC >= 1) {
    stop("Area under the ROC curve (AOC) must be greater than 0.5 and not greater than 1.", call. = TRUE)
  }
  a <- qnorm(1 - AUC) * 1.414
  return(a)
}


################################################################################
#
#' get_vf_roc
#'
#' Function to calculate variance function (VF) of the area under the curve
#' (AUC) for the received operator characteristic (ROC) surve sample size
#' calculations
#'
#' @param A Binormal distribution parameter
#' @param p Prevalence or proportion of cases in the relevant population. Used
#'     to calculate ratio \code{k} of cases to non-cases in the relevant
#'     population
#'
#' @examples
#'
#'   get_vf_roc(A = 2, p = 0.5)
#'
#' @source Obuchowski NA. Fundamentals of clinical research for radiologists:
#'   ROC Analysis. American Journal of Roentgenology. 2005;184.
#'
#' @export
#'
#
################################################################################

get_vf_roc <- function(A, p) {
  VF <- (0.0099 * exp(-A * A/2)) * ((5 * (A ^ 2) + 8) + (((A ^ 2) + 8) / get_k_roc(p)))
  return(VF)
}


################################################################################
#
#' get_nc_roc
#'
#' Function to calculate the number of cases needed for an ROC analysis for a
#' single test
#'
#' @param z The z-score/z-value for a 95\% confidence interval. Typical vaues
#'     for \code{z} are 1.645 for a 90\% confidence interval, 1.75 for a 92\%
#'     confidence interval, 1.96 for a 95\% confidence interval and 2.05 for a
#'     96\% confidence interval. Default is 1.96.
#' @param precision The numeric value of the desired precision. Default is set
#'     at ±5\% (0.05).
#' @param vf Variance function of the area under the curve (AUC) for the
#'     received operator characteristic (ROC) surve sample size calculations
#'
#' @examples
#'
#'   get_nc_roc(z = 1.96, precision = 0.05, vf = 0.00991024)
#'
#' @source Obuchowski NA. Fundamentals of clinical research for radiologists:
#'   ROC Analysis. American Journal of Roentgenology. 2005;184.
#'
#' @export
#'
#
################################################################################

get_nc_roc <- function(z = 1.96, precision = 0.05, vf) {
  nc <- ((z ^ 2) * vf) / (precision ^ 2)
  return(nc)
}


################################################################################
#
#' get_n_roc
#'
#' Function to calculate total sample size for an ROC analysis for a single
#' test
#'
#' @param z The z-score/z-value for a 95\% confidence interval. Typical vaues
#'     for \code{z} are 1.645 for a 90\% confidence interval, 1.75 for a 92\%
#'     confidence interval, 1.96 for a 95\% confidence interval and 2.05 for a
#'     96\% confidence interval. Default is 1.96.
#' @param precision The numeric value of the desired precision. Default is set
#'     at ±5\% (0.05).
#' @param vf Variance function of the area under the curve (AUC) for the
#'     received operator characteristic (ROC) surve sample size calculations
#' @param p Prevalence or proportion of cases in the relevant population. Used
#'     to calculate ratio \code{k} of cases to non-cases in the relevant
#'     population
#'
#' @examples
#'
#'   get_n_roc(z = 1.96, precision = 0.05, vf = 0.00991024, p = 0.2)
#'
#' @source Obuchowski NA. Fundamentals of clinical research for radiologists:
#'   ROC Analysis. American Journal of Roentgenology. 2005;184.
#'
#' @export
#'
#
################################################################################

get_n_roc <- function(z = 1.96, precision = 0.05, vf, p) {
  n <- get_nc_roc(z = z, precision = precision, vf = vf) * (1 + get_k_roc(p = p))
  return(n)
}
