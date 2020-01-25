#' Pre-process and Anonymize Response Data
#'
#' @param inpath Path to the directory containing raw response files.
#'
#' @param outpath Path to the directory where anonymized data will be saved.
#'
#' @param overwrite Whether to overwrite the anonymized data.
#'
#' @return Path where the files were written (\code{outpath}).
#'
#' @details Loads in the data from the raw response files and writes
#'   out non-anonymized, pre-processed versions to the current working
#'   directory, as well as anonymized versions to the directory
#'   specified by \code{outpath}. The anonymized output files have the
#'   following structure.
#'
#' \code{ANON_sessions.csv}: A table with one row for each participant
#' and four columns:
#' 
#' \describe{
#'   \item{ID}{Unique participant identifier.}
#'   \item{list_id}{Identifier of stimulus list.}
#'   \item{chk_native}{Whether the participant was a native English speaker.}
#'   \item{chk_nocheat}{Whether the participant did not look up answers.}
#' }
#'
#' \code{ANON_phases.csv}: A table with one row for each participant
#' and five columns:
#' 
#' \describe{
#'   \item{ID}{Unique participant identifier.}
#'   \item{phase_id}{Identifier of phase (1-4).}
#'   \item{duration_secs}{Duration of phase in seconds.}
#'   \item{chk_finished}{Whether the participant completed the phase.}
#'   \item{chk_dur}{Whether the phase was completed within a reasonable duration.}
#' }
#'
#' \code{ANON_interest.csv}: A table with one row for each interest
#' rating and four columns:
#'
#' \describe{
#'   \item{ID}{Unique participant identifier.}
#'   \item{stim_id}{Unique stimulus identifier.}
#'   \item{order}{Order of presentation.}
#'   \item{irating}{Interest rating (0-10).}
#' }
#'
#' \code{ANON_ratings.csv}: A table with one row for each truth rating
#' and five columns.
#'
#' \describe{
#'   \item{ID}{Unique participant indentifier.}
#'   \item{phase_id}{Identifier of phase (1-4).}
#'   \item{stim_id}{Unique stimulus identifier.}
#'   \item{order}{Order of presentation.}
#'   \item{trating}{Truth rating (1-7).}
#' }
#' 
#' @export
preprocess <- function(inpath,
                       outpath = paste0("anon-", Sys.Date()),
                       overwrite = FALSE) {

  private_sess_fname <- "NOT_ANONYMIZED_sessions.rds"
  private_phase_fname <- "NOT_ANONYMIZED_phases.rds"
  outpath <- sub("/$", "", outpath) # no trailing slash
  if (dir.exists(outpath)) {
    if (overwrite) {
      unlink(outpath, TRUE, TRUE)
    } else {
      stop("output directory '", outpath, "' exists and overwrite = FALSE")
    }
  }
  dir.create(outpath)
  if (file.exists(private_sess_fname) && !overwrite)
    stop("File '", private_sess_fname, "' exists and overwrite = FALSE")
  if (file.exists(private_phase_fname) && !overwrite)
    stop("File '", private_phase_fname, "' exists and overwrite = FALSE")
  
  sess <- import_sessions(inpath)
  phase <- import_phase_info(inpath)
  ratings <- import_tratings(inpath)
  interest <- import_iratings(inpath)

  ## destroy data from non-consenting participants

  sess_consent <- sess %>%
    dplyr::filter(grepl("^Yes", ConsentAll, ignore.case = TRUE))

  if (nrow(sess) - nrow(sess_consent)) {
    message("Eliminated data from ",
            nrow(sess) - nrow(sess_consent),
            " participants who did not consent to the full study.")
  }
  
  phase2 <- dplyr::semi_join(phase, sess_consent, "PID")

  phase_consent <- phase2 %>%
    dplyr::filter(grepl("^Yes", Consent, ignore.case = TRUE))

  if (nrow(phase2) - nrow(phase_consent)) {
    message("Eliminated data from ",
            nrow(phase2) - nrow(phase_consent),
            " phases where consent was not provided.")
  }

  ## remove non-native speakers
  sess_native <- sess_consent %>%
    dplyr::mutate(chk_native = grepl("English", nativelang, ignore.case = TRUE))

  phase_finished <- phase_consent %>%
    dplyr::mutate(chk_finished = Finished == "TRUE")

  ## find anyone who looked up answers; remove them from all phases
  cheaters <- phase_finished %>%
    dplyr::filter(grepl("^Yes", cheat, ignore.case = TRUE)) %>%
    dplyr::distinct(PID) %>%
    dplyr::mutate(chk_nocheat = FALSE)

  sess_keep <- sess_native %>%
    dplyr::left_join(cheaters, "PID") %>%
    tidyr::replace_na(list(chk_nocheat = TRUE))

  dur_cutoffs <-
    tibble::tibble(
              phase_id = factor(1:4, levels = 1:4),
              min_dur = c(3L * 60L, rep(1L * 60L, 3)),
              max_dur = c(40L * 60L, rep(30L * 60L, 3)))

  phase_keep <- phase_finished %>%
    dplyr::inner_join(dur_cutoffs, "phase_id") %>%
    dplyr::mutate(chk_dur = (as.integer(`Duration (in seconds)`) >= min_dur) &
             (as.integer(`Duration (in seconds)`) <= max_dur)) %>%
    dplyr::select(-min_dur, -max_dur)

  ## use phase_keep and sess_keep

  ## now anonymize
  sess_keep[["ID"]] <- sprintf("S%04d", sample(seq_len(nrow(sess_consent))))
  share_cols <- c("list_id", "chk_native", "chk_nocheat")
  sess_private <- sess_keep[, c("PID", "ID",
                                setdiff(names(sess_keep),
                                        c("PID", "ID", "ConsentAll", share_cols)))]
  sess_share <- sess_keep[, c("ID", share_cols)] %>%
    dplyr::arrange(ID)

  pshare_cols <- c("phase_id", "Duration (in seconds)", "chk_finished", "chk_dur")
  pkeep <- phase_keep %>%
    dplyr::inner_join(sess_private[, c("ID", "PID")], "PID")

  phase_share <- pkeep[, c("ID", pshare_cols)] %>%
    dplyr::arrange(ID, phase_id)
  colnames(phase_share)[colnames(phase_share) == "Duration (in seconds)"] <-
    "duration_secs"

  phase_private <- pkeep[, c("PID", "ID", "phase_id",
                             setdiff(names(phase_keep),
                                     c("PID", "ID", "phase_id", "list_id",
                                       "Duration (in seconds)",
                                       pshare_cols)))]

  ratings2 <- ratings %>%
    dplyr::group_by(PID, phase_id) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::semi_join(sess_consent, "PID") %>%
    dplyr::semi_join(phase_consent, c("PID", "phase_id")) %>%
    dplyr::inner_join(phase_private[, c("PID", "ID", "phase_id")],
               c("PID", "phase_id"))

  ratings_share <-
    ratings2[, c("ID", "phase_id", "stim_id", "order", "trating")] %>%
    dplyr::arrange(ID, phase_id, stim_id)

  interest2 <- interest %>%
    dplyr::group_by(PID) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::semi_join(sess_consent, "PID") %>%
    dplyr::semi_join(phase_consent %>% dplyr::filter(phase_id == 1L), "PID") %>%
    dplyr::inner_join(phase_private %>% dplyr::filter(phase_id == 1L) %>%
                      dplyr::select("PID", "ID"), "PID")

  interest_share <- interest2[, c("ID", "stim_id", "order", "irating")] %>%
    dplyr::arrange(ID, stim_id)

  saveRDS(sess_private, private_sess_fname)
  message("Wrote non-anonymized pre-processed session data to '",
          private_sess_fname, "'")
  saveRDS(phase_private, private_phase_fname)
  message("Wrote non-anonymized pre-processed phase data to '",
          private_phase_fname, "'")  
  readr::write_csv(sess_share, file.path(outpath, "ANON_sessions.csv"))
  readr::write_csv(phase_share, file.path(outpath, "ANON_phases.csv"))
  readr::write_csv(ratings_share, file.path(outpath, "ANON_ratings.csv"))
  readr::write_csv(interest_share, file.path(outpath, "ANON_interest.csv"))
  message("Wrote anonymized data to files ",
          "ANON_sessions.csv, ANON_phases.csv, ANON_ratings.csv, and ",
          "ANON_interest.csv in subdirectory '", outpath, "'")
  invisible(outpath)
}
