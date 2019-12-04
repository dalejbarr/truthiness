#' Data from Nadarevic & Erdfelder (2014), Experiment 1
#'
#' A dataset containing the trial data.
#'
#' @details The raw data file from which the dataset is derived is available using \code{system.file("data-raw", "Nadarevic_Erdfelder_2014_Exp1.csv", package = "truthiness")}, and the script for pre-processing the data can be assessed with \code{system.file("data-raw", "Nadarevic_Erdfelder_2014_Exp1.R")}.
#'
#' @seealso \code{link{NE_items}}
#' 
#' @format A data frame with 14,950 observations on 5 variables:
#' \describe{
#'   \item{subj_id}{Unique subject identifier.}
#'   \item{item_id}{Unique stimulus (statement) identifier.}
#'   \item{repetition}{Whether the statement was repeated (\code{old}) or not (\code{new}).}
#'   \item{delay}{Testing interval, ten minutes (\code{10m}) or one week (\code{1w}) after initial exposure.}
#'   \item{trating}{Truth rating on a six-point scale, with higher values corresponding to greater perceived truth.}
#' }
"NE_exp1"

#' Item information from Nadarevic & Erdfelder (2014)
#'
#' A dataset describing the statements used as stimuli.
#'
#' @format A data frame with 176 rows and 4 variables:
#' \describe{
#'   \item{item_id}{Unique stimulus (statement) identifier.}
#'   \item{statement}{Statement (in German).}
#'   \item{set}{Which set the statement belong to, used for counterbalancing.}
#'   \item{status}{Actual truth of the statement.}
#' }
"NE_items"

#' Fitted cumulative mixed model for Nadarevic & Erdfelder data.
#'
#' @format An object of class "clmm", resulting from a call to the \code{clmm} function in the ordinal package.
#' @seealso \code{\link{NE_exp1}}
"clmm_maximal"
