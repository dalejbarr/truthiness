globalVariables(c("clmm_maximal", "NE_exp1", "NE_items",
                "presentation_lists", "stimulus_conditions",
                "stimulus_materials", "prolific_headers"))

#' Data from Nadarevic & Erdfelder (2014), Experiment 1
#'
#' A dataset containing the trial data.
#'
#' @details The raw data file from which the dataset is derived is available using \code{system.file("data-raw", "Nadarevic_Erdfelder_2014_Exp1.csv", package = "truthiness")}, and the script for pre-processing the data can be assessed with \code{system.file("data-raw", "Nadarevic_Erdfelder_2014_Exp1.R")}. Note that to reduce the size of the stored object, elements \code{gfList}, \code{L}, and \code{condVar} have been removed from the fitted model data.
#'
#' @seealso \code{link{NE_items}}
#' 
#' @format A data frame with 14,950 observations on 7 variables:
#' \describe{
#'   \item{subj_id}{Unique subject identifier.}
#'   \item{item_id}{Unique stimulus (statement) identifier.}
#'   \item{repetition}{Whether the statement was repeated (\code{old}) or not (\code{new}).}
#'   \item{delay}{Testing interval, ten minutes (\code{10m}) or one week (\code{1w}) after initial exposure.}
#'   \item{trating}{Truth rating on a six-point scale, with higher values corresponding to greater perceived truth.}
#'   \item{R}{Deviation-coded numerical predictor for \code{repetition}, with \code{old} = .5 and \code{new} = -.5.}
#'   \item{D}{Deviation-coded numerical predictor for \code{delay}, with \code{10m} = .5 and \code{1w} = -.5.}
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
#' 
#' @details The object is the result of the function call
#' \code{ordinal::clmm(trating ~ R * D + (R * D | subj_id) +
#'                                       (R * D | item_id), NE_exp1)}.
#'
#' The fitted model is stored as an independent object in the package
#' because the fitting process is too slow to allow it to be
#' re-created whenever it is needed.
#' 
#' @seealso \code{\link{NE_exp1}}
"clmm_maximal"

#' Lists for counterbalanced stimulus presentation.
#'
#' @format A data frame (tibble) with 1,536 rows and 6 variables:
#' \describe{
#' 
#'   \item{phase_id}{Phase (1-4) in which the stimulus will be presented.}
#' 
#'   \item{list_id}{Unique identifier of stimulus presentation list.}
#' 
#'   \item{task}{Which task the stimulus is presented in (rate
#'   interest or truth).}
#' 
#'   \item{stim_id}{Unique identifier of the stimulus.}
#'
#'   \item{task_id}{Unique identifier of the task/stimulus combination.}
#'
#'   \item{order}{Presentation order within the phase.}
#' }
"presentation_lists"

#' Lookup table identifying stimulus conditions across lists.
#'
#' @format A data frame (tibble) with 1,024 rows and 4 variables:
#' \describe{
#' 
#'   \item{list_id}{Unique identifier of stimulus presentation list.}
#' 
#'   \item{stim_id}{Unique identifier of the stimulus.}
#'
#'   \item{actual_truth}{Actual truth or falsity of the statement.}
#' 
#'   \item{repetition}{Whether the statement was repeated or novel.}
#' 
#'   \item{interval}{Interval (phase) in which the truth-rating was
#'   performed (immediate = 1, 1 day = 2, 1 week = 3, 1 month = 4).}
#' 
#' }
"stimulus_conditions"

#' Stimulus materials (statements) used in the experiment.
#'
#' @format A data frame (tibble) with 128 rows and three variables:
#' \describe{
#'   \item{stim_id}{Unique identifier of the stimulus.}
#'   \item{actual_truth}{Whether the statement is actually true.}
#'   \item{statement}{The statement.}
#' }
"stimulus_materials"
