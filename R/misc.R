#' Warn About Simulated Data
#'
#' Check whether the data in \code{subdir} is simulated data and
#' generate a warning to include in an R Markdown document.
#' 
#' @param subdir Subdirectory with the anonymized data.
#'
#' @export
warn <- function(subdir) {
  if (truthiness::check_fake(subdir)) {
    extra_text <-
      case_when(basename(subdir) == "all_null" ~ "**This document demonstrates a null main effect and null interaction.**",
                basename(subdir) == "main_effect" ~ "**This document demonstrates a significant main effect and null interaction.**",
                basename(subdir) == "interaction" ~ "**This document demonstrates a significant main effect and significant interaction.**",
                TRUE ~ "")
    paste("\n<div class=\"warn\">",
          "*WARNING! Results in this document are based on **simulated** data*.",
          "This document will be re-compiled and results will be updated after data collection.\n",
          extra_text,
          "</div>\n", sep = "\n")
  }
}

#' Compile and Display Codebook and Materials
#'
#' Compile and display the codebook for anonymized data and stimulus
#' materials.
#'
#' @param show_stim Whether to include the stimulus materials.
#'
#' @param browse Whether to open the codebook in a browser. Otherwise, it just prints the filename.
#'
#' @return Path to the codebook file.
#'
#' @export
codebook <- function(show_stim = TRUE, browse = TRUE) {
  tmp <- tempfile(fileext = ".html")
  res <- rmarkdown::render(system.file("codebook.Rmd", package = "truthiness"),
                           output_file = basename(tmp),
                           output_dir = dirname(tmp),
                           knit_root_dir = dirname(tmp),
                           params = list(show_stim = show_stim),
                           quiet = TRUE)
  if (browse) {
    browseURL(res)
  } else {
    message("Compiled codebook to '", res, "'.")
  }
  invisible(res)
}

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

