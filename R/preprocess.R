get_varnames <- function(path) {
  ## read in the header row from a CSV file
  df <- readr::read_csv(readLines(path)[-c(2:3)])
  names(df)
}

scrape_cols <- function(path, cols) {
  ## read in specified columns from the CSV file
  df <- readr::read_csv(readLines(path)[-c(2:3)],
                        col_types = readr::cols(.default = readr::col_character()))
  df[, cols]
}

#' Read Interest Ratings From Response File
#'
#' Reads in the interest ratings from a single response file.
#'
#' @param path Path to the file.
#'
#' @return A long table with the interest ratings.
#'
#' @export
read_iratings <- function(path) {
  if (!grepl(rfiles_iregex, basename(path))) {
    stop("Filename '", path,
	 "' not recognized as file that contains interest ratings")
  }
  cnames <- grep("^IN[0-9]{3}$", get_varnames(path), value = TRUE)
  idat <- scrape_cols(path, c("PID", cnames))
  df <- tidyr::pivot_longer(idat, -PID, "stim_code", values_to = "irating")
  valid <- grepl("^[0-9]{1,2}", df[["irating"]])
  if (!all(valid)) {
    stop("invalid interest rating in file'", basename(path),
         "' at line(s) ",
         paste(which(!valid), collapse = ", "))
  }
  df[["stim_id"]] <- as.factor(as.integer(sub("^IN", "", df[["stim_code"]])))
  df[["irating"]] <- sub("^([0-9]{1,2})\\s.+", "\\1", df[["irating"]])
  df[["irating"]] <- as.integer(df[["irating"]])
  df
}

#' Read Truth Ratings From Response File
#'
#' Reads in the truth ratings from a single response file.
#'
#' @param path Path to the file.
#'
#' @return A long table with the truth ratings.
#'
#' @export
read_tratings <- function(path) {
  if (!grepl(rfiles_regex, basename(path))) {
    stop("Filename '", path,
	 "' not recognized as file that contains truth ratings")
  }
  cnames <- grep("^TR[0-9]{3}$", get_varnames(path), value = TRUE)
  idat <- scrape_cols(path, c("PID", cnames))
  df <- tidyr::pivot_longer(idat, -PID, "stim_code", values_to = "trating")
  valid <- grepl("^[1-7]{1}.*", df[["trating"]])
  if (!all(valid)) {
    stop("invalid truth rating in file '", basename(path),
         "' at line(s) ",
         paste(which(!valid), collapse = ", "))
  }
  df[["stim_id"]] <- as.factor(as.integer(sub("^TR", "", df[["stim_code"]])))
  df[["trating"]] <- sub("^([0-9]{1}).*", "\\1", df[["trating"]])
  df[["trating"]] <- as.integer(df[["trating"]])
  df
}

#' Read Session Information From Response File
#'
#' Read in session information from a single response file.
#'
#' @param path Path to the file.
#'
#' @return A table with session information.
#' 
#' @export
read_sessions <- function(path) {
  vnames <- get_varnames(path)
  tnames <- grep("^TR[0-9]{3}$", vnames, value = TRUE)
  inames <- grep("^IN[0-9]{3}$", vnames, value = TRUE)
  idat <- scrape_cols(path, setdiff(vnames, c(tnames, inames)))
  idat[, c("PID", setdiff(names(idat), "PID"))]
}

#' Validate Filenames in Directory
#'
#' Make sure all the files needed for the analysis are present in a
#' given directory.
#'
#' @param path Path to the files.
#'
#' @details Output files from the study must match the pattern
#'   \code{PXLY.csv} where X is phase number (1-4) and Y is list
#'   number (1-8).
#' 
#' @export
validate_filenames <- function(path) {
  if (!dir.exists(path)) stop("directory '", path, "' does not exist")
  allfiles <- dir(path, "^[Pp][1-4][Ll][1-8]\\.[Cc][Ss][Vv]$")
  files_needed <- paste0(rep(paste0("P", 1:4), each = 8), paste0("L", 1:8))
  stripped <- sub("\\.[Cc][Ss][Vv]$", "", allfiles)
  names(stripped) <- allfiles
  if (!setequal(files_needed, stripped)) {
    extra <- setdiff(stripped, files_needed)
    probs1 <-
      if (length(extra) > 0L) {
        paste0("Filenames must be PXLY.csv (X in 1:4, Y in 1:8); ",
               "violations: ",
               paste(paste0(names(stripped)[stripped %in% extra], ".csv"),
                     collapse = ", "),
               ". ")
      } else {
        ""
      }
    missing <- setdiff(files_needed, stripped)
    probs2 <-
      if (length(missing) > 0L) {
        paste0("Missing the following files: ",
               paste(paste0(missing, ".csv"), collapse = ", "))
      } else {
        ""
      }
    stop(probs1, probs2)
  }
  invisible(TRUE)
}

#' Import Interest Ratings From Multiple Files
#'
#' @param path Directory containing response files.
#'
#' @return A table with the interest ratings data.
#' 
#' @export
import_iratings <- function(path) {
  ifiles <- dir(sub("/$", "", path), rfiles_iregex, full.names = TRUE)
  df <- tibble::tibble(fname = ifiles)
  df[["data"]] <- purrr::map(df[["fname"]], read_iratings)
  tidyr::unnest(df, c(data))[, c("PID", "stim_id", "irating")]
}

#' Import Truth Ratings From Multiple Files
#'
#' @param path Directory containing response files.
#'
#' @return A table with the interest ratings data.
#' 
#' @export
import_tratings <- function(path) {
  ifiles <- dir(sub("/$", "", path), rfiles_regex, full.names = TRUE)
  phase_id <- factor(as.integer(substr(basename(ifiles), 2, 2)), levels = 1:4)
  df <- tibble::tibble(fname = ifiles,
                       phase_id = phase_id)
  df[["data"]] <- purrr::map(df[["fname"]], read_tratings)
  tidyr::unnest(df, c(data))[, c("PID", "phase_id", "stim_id", "trating")]
}

#' Import Session Information From Multiple Files
#'
#' @param path Directory containing response files.
#'
#' @return A table with session information.
#'
#' @export
import_sessions <- function(path) {
  ifiles <- dir(sub("/$", "", path), rfiles_regex, full.names = TRUE)
  list_id <- factor(as.integer(substr(basename(ifiles), 4, 4)), levels = 1:8)
  phase_id <- factor(as.integer(substr(basename(ifiles), 2, 2)), levels = 1:4)
  df <- tibble::tibble(fname = ifiles,
                       list_id = list_id,
                       phase_id = phase_id)
  df[["data"]] <- purrr::map(df[["fname"]], read_sessions)
  ## lets figure out the columns that only appear in a single session
  cnames <- purrr::map(df[["data"]], names)
  by_phase <- split(cnames, df[["phase_id"]])
  pdat <- split(df, df[["phase_id"]])
  pcols <- purrr::map(by_phase, function(x) {unique(unlist(x))})
  ## get the columns unique to each phase
  pdat2 <- list()

  for (i in 1:4) {
    others <- setdiff(1:4, i)
    unique_cols <-
      c("PID",
        setdiff(pcols[[i]],
                union(union(pcols[[others[1]]], pcols[[others[2]]]),
                      pcols[[others[3]]])))
    pdat[[i]][["data2"]] <- purrr::map(pdat[[i]][["data"]],
                                       function(x) x[, unique_cols])
    pdat2[[i]] <- tidyr::unnest(pdat[[i]][, c("list_id", "data2")], c("data2"))    
  }

  df2 <- dplyr::full_join(dplyr::full_join(pdat2[[1]], pdat2[[2]],
                                           c("PID", "list_id")),
                          dplyr::full_join(pdat2[[3]], pdat2[[4]],
                                           c("PID", "list_id")),
                          c("PID", "list_id"))
  df2[, c("PID", "list_id", setdiff(names(df2), c("PID", "list_id")))]
}

#' Import Phase Information From Multiple Files
#'
#' @param path Directory containing response files.
#'
#' @return A table with phase information.
#'
#' @export
import_phase_info <- function(path) {
  ifiles <- dir(sub("/$", "", path), rfiles_regex, full.names = TRUE)
  list_id <- factor(as.integer(substr(basename(ifiles), 4, 4)), levels = 1:8)
  phase_id <- factor(as.integer(substr(basename(ifiles), 2, 2)), levels = 1:4)
  df <- tibble::tibble(fname = ifiles,
                       list_id = list_id,
                       phase_id = phase_id)
  df[["data"]] <- purrr::map(df[["fname"]], read_sessions)
  ## lets figure out the columns that only appear in a single session
  cnames <- purrr::map(df[["data"]], names)
  by_phase <- split(cnames, df[["phase_id"]])
  pdat <- split(df, df[["phase_id"]])
  pcols <- purrr::map(by_phase, function(x) {unique(unlist(x))})
  ## get the columns common to all phases
  common_cols <- intersect(intersect(pcols[[1]], pcols[[2]]),
                           intersect(pcols[[3]], pcols[[4]]))
  pdat2 <- list()
  for (i in 1:4) {
    pdat[[i]][["data2"]] <- purrr::map(pdat[[i]][["data"]],
                                       function(x) x[, common_cols])
    pdat2[[i]] <- tidyr::unnest(pdat[[i]][, c("list_id", "phase_id", "data2")],
                                c("data2"))
  }
  df2 <- dplyr::bind_rows(pdat2[[1]], pdat2[[2]], pdat2[[3]], pdat2[[4]])
  df2[, c("PID", "list_id", "phase_id",
          setdiff(names(df2), c("PID", "list_id", "phase_id")))]
}

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
#' and ten columns:
#' 
#' \describe{
#'   \item{ID}{Unique participant identifier.}
#'   \item{list_id}{Identifier of stimulus list.}
#'   \item{age}{Age of the participant in years.}
#'   \item{gender}{Gender of the participant.}
#'   \item{nationality}{Nationality of the participant.}
#'   \item{nativelang}{Native language of the participant.}
#'   \item{chk_native}{Whether the participant was a native English speaker.}
#'   \item{chk_nocheat}{Whether the participant did not look up answers.}
#'   \item{chk_dur_all}{Whether the participant completed all phases
#'   within a reasonable time-frame.}
#'   \item{chk_flatline}{Whether the participant did not produce
#'   'flatline' responses during any phase of the study.}
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
#'   \item{chk_dur_phase}{Whether the phase was completed within a reasonable duration.}
#' }
#'
#' \code{ANON_interest.csv}: A table with one row for each interest
#' rating and three columns:
#'
#' \describe{
#'   \item{ID}{Unique participant identifier.}
#'   \item{stim_id}{Unique stimulus identifier.}
#'   \item{irating}{Interest rating (0-10).}
#' }
#'
#' \code{ANON_ratings.csv}: A table with one row for each truth rating
#' and four columns.
#'
#' \describe{
#'   \item{ID}{Unique participant indentifier.}
#'   \item{phase_id}{Identifier of phase (1-4).}
#'   \item{stim_id}{Unique stimulus identifier.}
#'   \item{trating}{Truth rating (1-7).}
#' }
#' 
#' @export
preprocess <- function(inpath,
                       outpath = paste0("anon-", Sys.Date()),
                       overwrite = FALSE) {

  private_sess_fname <- "NOT_ANONYMIZED_sessions.rds"
  private_phase_fname <- "NOT_ANONYMIZED_phases.rds"
  outpath <- normalize_path(outpath) # no trailing slash
  if (dir.exists(outpath)) {
    if (overwrite) {
      unlink(outpath, TRUE, TRUE)
    } else {
      stop("output directory '", outpath, "' exists and overwrite = FALSE")
    }
  }
  dir.create(outpath)
  if (check_fake(inpath)) {flag_fake(outpath)}
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
    dplyr::mutate(chk_native = grepl("English", nativelang,
                                     ignore.case = TRUE))

  ## phase-level exclusion: was the phase completed?
  phase_finished <- phase_consent %>%
    dplyr::mutate(chk_finished = Finished == "TRUE")

  ## identify anyone who looked up answers; remove them from all phases
  cheaters <- phase_finished %>%
    dplyr::filter(grepl("^Yes", cheat, ignore.case = TRUE)) %>%
    dplyr::distinct(PID) %>%
    dplyr::mutate(chk_nocheat = FALSE)

  sess_cheat <- sess_native %>%
    dplyr::left_join(cheaters, "PID") %>%
    tidyr::replace_na(list(chk_nocheat = TRUE))

  dur_cutoffs <-
    tibble::tibble(
              phase_id = factor(1:4, levels = 1:4),
              min_dur = c(3L * 60L, rep(1L * 60L, 3)),
              max_dur = c(40L * 60L, rep(30L * 60L, 3)))
  
  ## identify participants who were too fast or too slow on *any* phase
  phase_keep <- phase_finished %>%
    dplyr::inner_join(dur_cutoffs, "phase_id") %>%
    dplyr::mutate(chk_dur_phase =
                    (as.integer(`Duration (in seconds)`) >= min_dur) &
                    (as.integer(`Duration (in seconds)`) <= max_dur)) %>%
    dplyr::select(-min_dur, -max_dur)

  sess_dur <- sess_cheat %>%
    dplyr::left_join(phase_keep %>%
                     dplyr::filter(!chk_dur_phase) %>%
                     dplyr::distinct(PID) %>%
                     dplyr::mutate(chk_dur_all = FALSE), "PID") %>%
    tidyr::replace_na(list(chk_dur_all = TRUE))

  ## now find any flatliners
  ## interest scores
  ispt <- split(interest[["irating"]], interest[["PID"]])
  res <- sapply(ispt, function(.x) {length(unique(.x)) == 1L})
  flat_interest <- names(res)[res]
    
  ## truth ratings
  tspt <- split(ratings[["trating"]],
                list(ratings[["PID"]], ratings[["phase_id"]]), sep = ",")
  res <- sapply(tspt, function(.x) {length(unique(.x)) == 1L})
  flat_truth <- unique(sapply(names(res)[res],
                              function(.x) {strsplit(.x, ",")[[1]][1]},
                              USE.NAMES = FALSE))
  flatliners <- union(flat_interest, flat_truth)

  sess_keep <- sess_dur %>%
    dplyr::mutate(chk_flatline = !(PID %in% flatliners))

  ## done with participant level + phase level exclusions
  
  ## use phase_keep and sess_keep

  ## now anonymize
  sess_keep[["ID"]] <- sprintf("S%04d", sample(seq_len(nrow(sess_consent))))
  share_cols <- c("list_id",
                  "age", "gender", "nationality", "nativelang",
                  "chk_native", "chk_nocheat", "chk_dur_all", "chk_flatline")
  sess_private <- sess_keep[, c("PID", "ID",
                                setdiff(names(sess_keep),
                                        c("PID", "ID", "ConsentAll",
                                          share_cols)))]
  sess_share <- sess_keep[, c("ID", share_cols)] %>%
    dplyr::arrange(ID)

  pshare_cols <- c("phase_id", "Duration (in seconds)", "chk_finished",
                   "chk_dur_phase")
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
    dplyr::ungroup() %>%
    dplyr::semi_join(sess_consent, "PID") %>%
    dplyr::semi_join(phase_consent, c("PID", "phase_id")) %>%
    dplyr::inner_join(phase_private[, c("PID", "ID", "phase_id")],
               c("PID", "phase_id"))

  ratings_share <-
    ratings2[, c("ID", "phase_id", "stim_id", "trating")] %>%
    dplyr::arrange(ID, phase_id, stim_id)

  interest2 <- interest %>%
    dplyr::group_by(PID) %>%
    dplyr::ungroup() %>%
    dplyr::semi_join(sess_consent, "PID") %>%
    dplyr::semi_join(phase_consent %>%
                     dplyr::filter(phase_id == 1L), "PID") %>%
    dplyr::inner_join(phase_private %>% dplyr::filter(phase_id == 1L) %>%
                      dplyr::select("PID", "ID"), "PID")

  interest_share <- interest2[, c("ID", "stim_id", "irating")] %>%
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
  par_exclude <- tibble::tibble(ID = character(0), reason = character(0))
  phs_exclude <- tibble::tibble(ID = character(0), phase_id = integer(0),
                                reason = character(0))
  readr::write_csv(par_exclude, file.path(outpath,
                                          "exclude_participants.csv"))
  readr::write_csv(phs_exclude, file.path(outpath,
                                          "exclude_phases.csv"))
  message("\nEdit the files 'exclude_participants.csv' and 'exclude_phases.csv' to manually\n", "exclude participants and phases.")
  invisible(outpath)
}
