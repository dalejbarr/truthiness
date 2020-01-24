#' Simulate ordinal responses choices from log odds
#'
#' @param eta Predicted response tendency or tendencies on log odds scale.
#' @param thresh Cut-points (thresholds).
#'
#' @return A vector of the same length as \code{eta} with simulated
#'   integer response values, one for each \code{eta} value.
#'
#' @examples
#' # N=10 with eta = 0 and 6 point scale from N&E
#' eta2resp(rep(0, 10), clmm_maximal$alpha)
#' 
#' # N=10 with eta = 0 and 7 point scale
#' eta2resp(rep(0, 10), alpha_6_to_7(clmm_maximal$alpha))
#'
#' # N=10 with eta = 4 and 6 point scale from N&E
#' eta2resp(rep(4, 10), clmm_maximal$alpha)
#' 
#' # N=10 with eta = 4 and 7 point scale
#' eta2resp(rep(4, 10), alpha_6_to_7(clmm_maximal$alpha))
#'
#' @export
eta2resp <- function(eta, thresh) {
  sapply(eta, function(.x) {
    probs <- c(1 / (1 + exp(-(thresh - .x))), 1L)
    min(which(probs > stats::runif(1)))
  })
}

#' Convert threshold values from six to seven point scale
#'
#' @param thresh Log-odds thresholds from a cumulative logit model fit. Should be a five-element vector.
#' 
#' @return A six-element vector representing the thresholds on a seven-point scale.
#' 
#' @details The basic algorithm is to copy the top and bottom thresholds and then to shrink the spaces between the thresholds on the six-point scale to 80% of their size. The left over space is allocated to the middle category on the seven-point scale.
#' 
#' @examples
#' clmm_maximal$alpha # original thresholds
#' alpha_6_to_7(clmm_maximal$alpha) 
#' @export
alpha_6_to_7 <- function(thresh = truthiness::clmm_maximal$alpha) {
  newthresh <- double(6)
  names(newthresh) <- paste(1:6, 2:7, sep = "|")

  ## fix the endpoints
  newthresh[c(1, 6)] <- thresh[c(1, 5)]

  newthresh[2] <- newthresh[1] + (thresh[2] - thresh[1]) * .8
  newthresh[3] <- newthresh[2] + (thresh[3] - thresh[2]) * .8

  newthresh[5] <- newthresh[6] - (thresh[5] - thresh[4]) * .8
  newthresh[4] <- newthresh[5] - (thresh[4] - thresh[3]) * .8
  newthresh
}

