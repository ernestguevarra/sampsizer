################################################################################
#
#' get_deff
#'
#' Function to calculate design effect (\code{DEFF}) given intracluster
#' correlation coefficient (\code{ICC}) and cluster size.
#'
#' @param x variable to calculate \code{ICC} from
#' @param cluster variable identifying the clusters or groupings of the variable
#' @param size number of samples within a cluster
#'
#' @return A numeric value for design effect (DEFF)
#'
#' @examples
#'   #
#'
#' @export
#'   x <- sample(1:2, size = 25, replace = TRUE)
#'   cluster <- c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5), rep(5, 5))
#'   get_deff(x = x, cluster = cluster, size = 10)
#'
#
################################################################################

get_deff <- function(x, cluster, size) {
  icc <- get_icc(x, cluster)

  deff <- 1 + (size - 1) * icc

  return(deff)
}
