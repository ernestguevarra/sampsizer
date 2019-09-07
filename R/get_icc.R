################################################################################
#
#'
#' Function to calculate the intracluster correlation coefficient of an
#' indicator collected from random cluster survey (RCS). This is a wrapper of
#' the \code{deff()} function in the \code{Hmisc} package.
#'
#' @param x variable to calculate ICC from
#' @param cluster variable identifying the clusters or groupings of the variable
#'
#' @return A vector with named elements n (total number of non-missing observations),
#'     \code{clusters} (number of clusters after deleting missing data),
#'     \code{rho} (intra-cluster correlation), and \code{deff} (design effect).
#'
#' @examples
#'   x <- sample(1:2, size = 25, replace = TRUE)
#'   cluster <- c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5), rep(5, 5))
#'   get_icc(x = x, cluster = cluster)
#'
#' @export
#'
#
################################################################################

get_icc <- function(x, cluster) {
  icc <- Hmisc::deff(y = x, cluster = cluster)
  return(icc)
}
