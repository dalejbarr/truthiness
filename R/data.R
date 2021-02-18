globalVariables(c("clmm_maximal", "NE_exp1", "NE_items",
                  "presentation_lists", "stimulus_conditions",
                  "stimulus_categories",
                  "stimulus_materials", "prolific_headers"))

#' Data from Experiment 1 of Nadarevic and Erdfelder
#'
#' A dataset containing truth ratings from Experiment 1 of
#' \insertCite{Nadarevic2014;textual}{truthiness}.
#'
#' @source The source data is from
#'   \insertCite{Nadarevic2014;textual}{truthiness}, which is freely
#'   available for download from \url{https://osf.io/eut35/}. The data
#'   included here has been reorganized for statistical modeling.
#'
#' @seealso \code{link{NE_items}}
#' 
#' @format A data frame with 14,950 observations on 7 variables:
#' \describe{
#'   \item{\code{subj_id}}{Unique subject identifier.}
#' 
#'   \item{\code{item_id}}{Unique stimulus (statement) identifier.}
#' 
#'   \item{\code{repetition}}{Whether the statement was repeated (\code{old})
#'   or not (\code{new}).}
#' 
#'   \item{\code{delay}}{Testing interval, ten minutes (\code{10m}) or one
#'   week (\code{1w}) after initial exposure.}
#' 
#'   \item{\code{trating}}{Truth rating on a six-point scale, with higher
#'   values corresponding to greater perceived truth.}
#' 
#'   \item{\code{R}}{Deviation-coded numerical predictor for
#'   \code{repetition}, with \code{old} = .5 and \code{new} = -.5.}
#' 
#'   \item{\code{D}}{Deviation-coded numerical predictor for \code{delay},
#'   with \code{10m} = .5 and \code{1w} = -.5.}
#' 
#' }
#' @references
#'   \insertAllCited{}
"NE_exp1"

#' Stimulus Information from Experiment 1 of Nadarevic and Erdfelder
#'
#' A dataset describing the statements used as stimuli in Experiment 1 of
#' \insertCite{Nadarevic2014;textual}{truthiness}.
#'
#' @format A data frame with 176 rows and 4 variables:
#' \describe{
#'   \item{\code{item_id}}{Unique stimulus (statement) identifier.}
#'   \item{\code{statement}}{Statement (in German).}
#'   \item{\code{set}}{Which set the statement belong to, used for counterbalancing.}
#'   \item{\code{status}}{Actual truth of the statement.}
#' }
#' @references
#'   \insertAllCited{}
"NE_items"

#' Fitted Cumulative Link Mixed Model for Nadarevic and Erdfelder Data
#'
#' @format An object of class "clmm", resulting from a call to the
#' \code{\link[ordinal]{clmm}} function.
#' 
#' @details The object is the result of the function call
#' 
#' \code{ordinal::clmm(trating ~ R * D + (R * D | subj_id) +
#'                                       (R * D | item_id), NE_exp1)}.
#'
#' The fitted model is stored as an independent object in the package
#' because the fitting process is too slow to allow it to be
#' re-created whenever it is needed.
#' 
#' @seealso \code{\link{NE_exp1}}
"clmm_maximal"

#' Design of the Longitudinal Illusory Truth Study
#'
#' A collection of four data frames in tidy format that contain
#' information about stimuli and experimental design for
#' \insertCite{Henderson_Simons_Barr_2021;textual}{truthiness}.
#'
#' @details Each data frame contains a subset of the following 11 variables:
#' 
#' \describe{
#'   \item{\code{stim_id}}{Unique identifier of the stimulus.}
#'
#'   \item{\code{statement}}{The statement.}
#'
#'   \item{\code{actual_truth}}{Actual truth or falsity of the statement.}
#'
#'   \item{\code{choice}}{Category number; allows for more than one correct categorization.}
#' 
#'   \item{\code{category}}{Name of category.}
#'
#'   \item{\code{list_id}}{Unique identifier of stimulus presentation list.}
#'
#'   \item{\code{repetition}}{Whether the statement was repeated or novel.}
#' 
#'   \item{\code{interval}}{Interval (phase) in which the truth-rating was
#'   performed (immediate = 1, 1 day = 2, 1 week = 3, 1 month = 4).}
#'
#'   \item{\code{phase_id}}{Phase (1-4) in which the stimulus will be presented.}
#' 
#'   \item{\code{task}}{Which task the stimulus is presented in (categorize
#'   or rate truth).}
#'
#'   \item{\code{task_id}}{Unique identifier of the task/stimulus combination.}
#' }
#'
#' The \code{stimulus_materials} data frame lists all 128 trivia
#' statements used in the study. These statements were adapted from
#' \insertCite{Nadarevic2014;textual}{truthiness} and
#' \insertCite{Dekeersmaecker2019;textual}{truthiness}, who in turn
#' adapted their materials from those original compiled by
#' \insertCite{Unkelbach_Rom_2017;textual}{truthiness}. Each stimulus is
#' given a unique identifier, \code{stim_id}, that appears across
#' related tables.
#'
#' The \code{stimulus_categories} data frame gives information about
#' which category each stimulus statement belongs to. Note that each
#' statement can belong to more than one category simultaneously.
#'
#' The \code{stimulus_conditions} data frame provides a 'lookup' table
#' that associates each stimulus (\code{stim_id}) from each
#' presentation list (\code{list_id}) with its experimental conditions
#' (\code{repetition}, and \code{interval}).
#'
#' There were eight separate presentation lists used in the study for
#' counterbalancing purposes. These lists are provided in
#' the \code{presentation_lists} data frame.
#'
#' @name truth_trajectory_design
#' @references
#'   \insertAllCited{}
NULL

#' @rdname truth_trajectory_design
"stimulus_materials"

#' @rdname truth_trajectory_design
"stimulus_categories"

#' @rdname truth_trajectory_design
"stimulus_conditions"

#' @rdname truth_trajectory_design
"presentation_lists"

#' Data from the Longitudinal Illusory Truth Study
#' 
#' A collection of four data frames representing the anonymized
#' longitudinal data in tidy format from
#' \insertCite{Henderson_Simons_Barr_2021;textual}{truthiness}.
#'
#' @details Each data frame contains a subset of the following variables:
#' 
#' \describe{
#'   \item{\code{ID}}{Participant identifier.}
#' 
#'   \item{\code{list_id}}{Stimulus list identifier.}
#'
#'   \item{\code{phase_id}}{Phase number (1-4).}
#'
#'   \item{\code{stim_id}}{Stimulus identifier.}
#'
#'   \item{\code{Age}}{Age of participant in years.}
#' 
#'   \item{\code{Gender}}{Gender of participant.}
#' 
#'   \item{\code{Nationality}}{Nationality of participant.}
#' 
#'   \item{\code{NativeLang}}{Native language of participant.}
#'
#'   \item{\code{duration_secs}}{Duration of the phase in seconds.}
#' 
#'   \item{\code{category}}{Category the participant selected for this statement.}
#' 
#'   \item{\code{trating}}{Truth rating on a seven-point scale, 1=Definitely
#'   False, 7=Definitely True.}
#' 
#'   \item{\code{excl_phase}}{Phase in which participant was excluded (\code{NA}
#'   if never excluded).}
#' 
#'   \item{\code{excl_reason}}{Reason for participant exclusion.}
#'
#'   \item{\code{p_excl_reason}}{Reason for phase exclusion.}
#' 
#'   \item{\code{chk_anydata}}{Whether there is ratings data for at least one
#'   phase for this participant after phase-level exclusions.}
#' 
#'   \item{\code{chk_consent_all}}{Whether participant gave consent for all
#'   phases.}
#' 
#'   \item{\code{chk_consent}}{Whether participant gave consent for this phase.}
#'
#'   \item{\code{chk_dur_all}}{Whether all phase durations for this
#'   participant were within an acceptable range.}
#' 
#'   \item{\code{chk_finished}}{Whether participant completed the rating task
#'   for this phase.}
#' 
#'   \item{\code{chk_native}}{Whether participant is a native speaker of
#'   English.}
#' 
#'   \item{\code{chk_nocheat}}{Whether participant never looked up answers.}
#' 
#'   \item{\code{chk_noduplicates}}{Whether there were no duplicated
#'   sessions.}
#' 
#'   \item{\code{chk_noflatline}}{Whether the participant did not produce
#'   'flatline' responses.}
#' 
#'   \item{\code{chk_notmanex}}{Whether the participant (or phase) is not
#'   manually excluded.}
#' 
#'   \item{\code{keep}}{Logical value, whether to keep (TRUE) or exclude
#'   (FALSE) participant (or phase data); this is a boolean "and" of
#'   all of the exclusion criteria (\code{chk_*} variables) for that participant (or
#'   phase).}
#' }
#'
#' @details
#'
#' The \code{sessions} data frame contains information about the 631
#' participants who were recruited to the study. The \code{chk_*}
#' variables are logical variables representing exclusion
#' criteria. The variable \code{keep} is a boolean "AND" of these
#' criteria, and thus has a value of \code{TRUE} for participants who
#' are to be included and \code{FALSE} for those who are to be
#' excluded.
#'
#' The \code{phases} data frame contains data from the 2,282 phases
#' that were initiated by participants. Each participant who was not
#' excluded during data collection had the opportunity to complete up
#' to four phases of data collection taking place (1) immediately
#' after the exposure phase; (2) one day after exposure; (3) one week
#' after exposure; and (4) one month after exposure. The \code{chk_*}
#' variables in this data frame represent exclusion criteria, and
#' \code{keep} is a boolean "AND" of those criteria along with the
#' \code{keep} variable from the \code{sessions} table. In other
#' words, to apply the full set of participant-level and phase-level
#' exclusion criteria for the study, simply include those rows in
#' \code{phases} where \code{keep} is set to \code{TRUE}, and join
#' this table to the others in the set; see the example below.
#'
#' The \code{cjudgments} table contains 39,406 category judgments that
#' were produced in the exposure phase (phase 1) of the study.
#' 
#' The \code{ratings} data frame contains 72,215 truth ratings of the
#' stimulus statements used in the study. Ratings were on a 1-7 scale
#' (1 = definitely false; 7 = definitely true).
#'
#' @name truth_trajectory_data
#' @references
#'   \insertAllCited{}
#' @examples
#' library(dplyr)
#'
#' ## apply exclusions and merge with ratings data
#' ratings_incl <- phases %>%
#'   filter(keep) %>%                                        # apply exclusions
#'   inner_join(sessions %>% select(ID, list_id), "ID") %>%  # get list ID
#'   inner_join(ratings, c("ID", "phase_id"))
#' 
#' ## look up conditions and calculate cell means
#' ratings_incl %>%
#'   inner_join(stimulus_conditions, c("list_id", "stim_id")) %>% # lookup condition
#'   group_by(repetition, interval) %>%
#'   summarize(rating_mean = mean(trating),
#'             rating_sd = sd(trating),
#'             N = n()) %>%
#'   ungroup()
NULL

#' @rdname truth_trajectory_data
"sessions"

#' @rdname truth_trajectory_data
"phases"

#' @rdname truth_trajectory_data
"cjudgments"

#' @rdname truth_trajectory_data
"ratings"

#' Fitted Models from the Longitudinal Illusory Truth Study
#'
#' Fitted models from the pre-registered analysis of
#'   \insertCite{Henderson_Simons_Barr_2021;textual}{truthiness},
#'   which have been stored as an objects in the package because the
#'   fitting process is too slow to allow them to be re-created when
#'   needed.
#' 
#' @format This object is a named list with six elements, with each
#'   element representing a fitted model object of class "clmm",
#'   resulting from a call to the \code{\link[ordinal]{clmm}}
#'   function. The named elements are:
#'
#' \describe{
#' 
#'   \item{\code{main_base}}{Base model for testing the main effect; 
#'   model formula is \code{mod1 <- T ~ R + I1 + I2 + I3 + R:I1 + R:I2 + R:I3 + (R | subj_id) + (R | stim_id)}.}
#'
#'   \item{\code{main_comp}}{Comparison model for testing the main
#'     effect; model formula identical to \code{main_base} except the
#'     fixed effect \code{R} has been excluded.}
#' 
#'   \item{\code{ix_base}}{Base model for testing the
#'     repetition-by-interval interaction; Model formula is \code{mod3
#'     <- T ~ R + I1 + I2 + I3 + R:I1 + R:I2 + R:I3 + (R:I1 + R:I2 +
#'     R:I3 | subj_id) + (R:I1 + R:I2 + R:I3 | stim_id)}.}
#'
#'   \item{\code{ix_comp}}{Comparison model for testing the
#'   interaction; Model formula is identical to \code{ix_base} except
#'   for exclusion of the fixed effects terms \code{R:I1},
#'   \code{R:I2}, and \code{R:I3}.}
#' 
#'   \item{\code{ix2}}{Same as \code{ix_base} except predictors
#'   included as factors rather than numerical predictors to enable
#'   use of functions from the \code{emmeans} package (for equivalence
#'   and follow-up tests).}
#'
#'   \item{\code{main2}}{Same as \code{main_base} except predictors
#'   included as factors rather than numerical predictors to enable
#'   use of functions from the \code{emmeans} package (for equivalence
#'   and follow-up tests).}
#' }
#' 
#' @examples
#' library(ordinal)
#'
#' ## print model information
#' summary(truth_trajectory_models$ix_base)
#'
#' ## likelihood ratio test, testing repetition-by-interval interaction
#' anova(truth_trajectory_models$ix_base,
#'       truth_trajectory_models$ix_comp)
"truth_trajectory_models"
