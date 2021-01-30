globalVariables(c("clmm_maximal", "NE_exp1", "NE_items",
                  "presentation_lists", "stimulus_conditions",
                  "stimulus_categories",
                  "stimulus_materials", "prolific_headers"))

#' Data from Nadarevic & Erdfelder 2014
#'
#' A dataset containing the trial data from Experiment 1 of Nadarevic & Erdfelder (2014).
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

#' Item Information from Nadarevic & Erdfelder 2014
#'
#' A dataset describing the statements used as stimuli in Experiment 1 of Nadarevic & Erdfelder (2014).
#'
#' @format A data frame with 176 rows and 4 variables:
#' \describe{
#'   \item{item_id}{Unique stimulus (statement) identifier.}
#'   \item{statement}{Statement (in German).}
#'   \item{set}{Which set the statement belong to, used for counterbalancing.}
#'   \item{status}{Actual truth of the statement.}
#' }
"NE_items"

#' Fitted Cumulative Link Mixed Model for Nadarevic & Erdfelder Data
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

#' Lists for Counterbalanced Stimulus Presentation
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

#' Lookup Table Identifying Stimulus Conditions Across Lists
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

#' Statements Used in the Truth Trajectory Study
#'
#' @format A data frame (tibble) with 128 rows and three variables:
#' \describe{
#'   \item{stim_id}{Unique identifier of the stimulus.}
#'   \item{actual_truth}{Whether the statement is actually true.}
#'   \item{statement}{The statement.}
#' }
"stimulus_materials"

#' Target Categories for Statements in the Truth Trajectory Study
#'
#' A dataset representing the category or categories a stimulus belongs to for the categorization task.
#'
#' @format A data frame with 170 observations on 3 variable:
#' \describe{
#'   \item{stim_id}{Unique identifier of the stimulus.}
#'   \item{choice}{Category number, allows for more than one correct categorization.}
#'   \item{category}{Name of category.}
#' }
"stimulus_categories"

#' Session Information from Truth Trajectory Study
#'
#' A dataset representing information about each participant who took part in the study.
#'
#' @details The logical variable keep is a boolean AND of the
#'   variables `chk_noduplicates`, `chk_consent_all`, `chk_native`,
#'   `chk_nocheat`, `chk_dur_all`, `chk_noflatline`, `chk_anydata`,
#'   and `chk_notmanex`.
#' 
#' @format A data frame (tibble) with 631 observations on 17 variables:
#' \describe{
#'   \item{ID}{Participant identifier.}
#'   \item{list_id}{Stimulus list identifier.}
#'   \item{Age}{Age of participant in years.}
#'   \item{Gender}{Gender of participant.}
#'   \item{Nationality}{Nationality of participant.}
#'   \item{NativeLang}{Native language of participant.}
#'   \item{keep}{Logical value, whether to keep (TRUE) or exclude (FALSE) participant.}
#'   \item{excl_phase}{Phase in which exclusion occurred (or NA).}
#'   \item{excl_reason}{Reason for exclusion (or NA).}
#'   \item{chk_noduplicates}{Whether there were no duplicated sessions.}
#'   \item{chk_consent_all}{Whether participant gave consent for all phases.}
#'   \item{chk_native}{Whether participant is a native speaker of English.}
#'   \item{chk_nocheat}{Whether participant never looked up answers.}
#'   \item{chk_dur_all}{Whether all phase durations were within an acceptable range.}
#'   \item{chk_noflatline}{Whether the participant did not produce 'flatline' responses.}
#'   \item{chk_anydata}{Whether there is any ratings data for this participant.}
#'   \item{chk_notmanex}{Whether the participant is not manually excluded.}
#' }
"sessions"

#' Phase Information from Truth Trajectory Study
#'
#' A dataset representing information about each phase for each participant.
#'
#' @details The logical variable keep is a boolean AND of the
#'   variables `chk_consent`, `chk_finished`, `chk_notmanex`,
#'   and the variable `keep` from the `sessions` table.
#' 
#' @format A data frame (tibble) with 2,291 observations on 9 variables:
#' \describe{
#'   \item{ID}{Participant identifier.}
#'   \item{phase_id}{Phase number (1-4).}
#'   \item{duration_secs}{Duration of the phase in seconds.}
#'   \item{keep}{Logical value, whether to keep (TRUE) or exclude (FALSE) participant.}
#'   \item{p_excl_reason}{Reason phase is excluded (or NA).}
#'   \item{chk_consent}{Whether participant gave consent for this phase.}
#'   \item{chk_finished}{Whether participant completed the rating task.}
#'   \item{chk_notmanex}{Whether the phase was not manually excluded.}
#'   \item{reason_for_manual_exclusion}{Reason for exclusion or NA.}
#' }
"phases"

#' Statement Ratings from Truth Trajectory Study
#'
#' A dataset representing truth ratings for each statement by each
#' participant across phases. Ratings were on a 1-7 scale.
#'
#' @format A data frame (tibble) with 72,282 observations on 4 variables:
#' \describe{
#'   \item{ID}{Participant identifier.}
#'   \item{phase_id}{Phase number (1-4).}
#'   \item{stim_id}{Stimulus identifier.}
#'   \item{trating}{Rating.}
#' }
"ratings"

#' Category Judgments from Truth Trajectory Study
#'
#' A dataset representing category judgments for each statement by
#' each participant during the exposure phase (phase 1).
#'
#' @format A data frame (tibble) with 39,406 observations on 3 variables:
#' \describe{
#'   \item{ID}{Participant identifier.}
#'   \item{stim_id}{Stimulus identifier.}
#'   \item{category}{Category.}
#' }
"cjudgments"
