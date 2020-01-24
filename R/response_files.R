make_response_file <- function(data, segment_id, subj_data, idata, path) {
  id <- as.integer(strsplit(segment_id, "\\.")[[1]])
  names(id) <- c("L", "P")
  pad_string <- "xxxx"
  dur_id <- paste0("dur_", id[["P"]])
  wide_data <- tidyr::pivot_wider(data,
                                  names_from = "task_id",
                                  values_from = "trating")
  jtib <- wide_data[, "subj_id"]
  jtib[["StartDate"]] <-
    jtib[["EndDate"]] <-
    jtib[["Status"]] <-
    jtib[["Progress"]] <-
    jtib[["RecordedDate"]] <-
    jtib[["ResponseId"]] <-
    jtib[["DistributionChannel"]] <-
    jtib[["UserLanguage"]] <-
    pad_string

  rtib <- wide_data[, "subj_id"]
  rtib[["age"]] <-
    rtib[["gender"]] <-
    rtib[["nationality"]] <-
    rtib[["comments"]] <-
    pad_string

  extra <- 
    dplyr::inner_join(
             wide_data[, "subj_id"],
             subj_data[, c("subj_id", dur_id, "Finished",
                           "ConsentAll", "Consent", "PID",
                           "cheat", "repeater", "nativelang",
                           "Q1", "Q2_1", "Q2_2", "Q2_3", "Q2_4", "Q2_5")],
             "subj_id")
  names(extra)[names(extra) == dur_id] <- "Duration (in seconds)"

  plists <- truthiness::presentation_lists
  plists2 <-
    plists[plists[["task"]] == "interest" &
	   as.integer(plists[["list_id"]]) == id[["L"]],]
  plists3 <- tidyr::crossing(jtib[, "subj_id"],
                             plists2[, c("task_id")])
  plists3[["score"]] <- sample(
    c("0 Not at all interesting",
      1:9,
      "10 Completely interesting"), nrow(plists3), TRUE)
  this_idata <-
    tidyr::pivot_wider(plists3,
                       names_from = "task_id",
                       values_from = "score")
  tord <- as.character(plists2[["task_id"]])
  
  ftbl <- dplyr::inner_join(jtib, extra, "subj_id") %>%
    dplyr::inner_join(wide_data, "subj_id") %>%
    dplyr::left_join(this_idata[, c("subj_id", tord)], "subj_id") %>%
    dplyr::inner_join(rtib, "subj_id")
  ftbl[["PROLIFIC_PID"]] <- ftbl[["PID"]]

  wide_cnames <- setdiff(colnames(wide_data),
                         c("list_id", "phase_id", "subj_id"))
  inames <- setdiff(colnames(this_idata), "subj_id")

  ## repeaters
  rtodo <- which(ftbl[["repeater"]])
  for (i in rtodo) {
    ftbl[i, wide_cnames] <- factor(rep(sample(1:7, 1), length(wide_cnames)),
                                   levels = 1:7, ordered = TRUE)
  }

  tt <- respfile_headers[["head_cols"]]
  zz <- respfile_headers[["tail_cols"]]
  if (id[["P"]] == 1L) {
    cc <- which(tt == "Consent")
    ee <- which(zz == "comments")
    cols_left <- c(tt[1:(cc - 1L)], "ConsentAll", tt[cc:length(tt)])
    cols_mid <- c(inames, wide_cnames)
    cols_right <- c(zz[1:(ee - 1L)], "nativelang", zz[ee:length(zz)])
  } else if (id[["P"]] == 4L) {
    dd <- which(zz == "comments")
    cols_left <- tt
    cols_mid <- wide_cnames
    cols_right <- c(zz[dd:(length(zz) - 1L)],
                    "Q1", "Q2_1", "Q2_2", "Q2_3", "Q2_4", "Q2_5",
                    zz[length(zz)])
  } else {
    dd <- which(zz == "comments")
    cols_left <- tt
    cols_mid <- wide_cnames
    cols_right <- zz[dd:length(zz)]
  }

  odata <- ftbl[, c(cols_left, cols_mid, cols_right)]
  fname <- file.path(path, sprintf("P%dL%d.csv", id[["P"]], id[["L"]]))

  ## now create the header lines of the file
  xx <- as.matrix(respfile_headers$left_chunk)
  consentTxt <- as.character(xx[2, 11])
  r1 <- c(cols_left, seq_along(cols_mid), cols_right)
  dr1 <- do.call(data.frame,
                 c(as.list(r1), list(stringsAsFactors = FALSE))) %>%
    tibble::as_tibble()

  dr2 <- do.call(data.frame,
                 c(as.list(colnames(odata),
                           list(stringsAsFactors = FALSE))))

  st <- tibble::tibble(stim_id = factor(
                         as.integer(substr(cols_mid, 3, 5)),
                         levels = levels(
                           truthiness::stimulus_materials[["stim_id"]])))

  st2 <- dplyr::inner_join(st, truthiness::stimulus_materials, "stim_id")
  r3 <- c(cols_left,
          paste0(seq_along(st2[["statement"]]), ".    ",
                 st2[["statement"]]),
          cols_right)
  r3[r3 %in% c("PID", "PROLIFIC_PID")] <- "Please enter your Prolific ID:"
  r3[r3 == "Q1"] <- "What questions do you think researchers in this area should focus on"
  r3[r3 %in% c("Q2_1", "Q2_2", "Q2_3", "Q2_4", "Q2_5")] <-
    "Below are five"
  dr3 <- do.call(data.frame,
                 c(as.list(r3),
                   list(stringsAsFactors = FALSE)))
  r4 <- paste0("{\"ImportId\":", "\"", colnames(odata), "\"}")
  dr4 <- do.call(data.frame,
                 c(as.list(r4),
                   list(stringsAsFactors = FALSE)))

  ## readr::write_csv(dr1, fname, col_names = FALSE, append = FALSE)
  readr::write_csv(dr2, fname, col_names = FALSE, append = FALSE)
  readr::write_csv(dr3, fname, col_names = FALSE, append = TRUE)
  readr::write_csv(dr4,
                   fname, col_names = FALSE, append = TRUE)
  readr::write_csv(odata, fname, col_names = FALSE, append = TRUE)

  return(fname)
}

#' Simulate response data files from the rating study
#'
#' @param nsubj Number of subjects; must be a multiple of 8.
#'
#' @param path Path to subdirectory where files should be stored.
#'
#' @param overwrite Whether to overwrite the subdirectory.
#'
#' @param p_too_fast Probability that the respondent completed the
#'   task faster than the cutoff time ('Duration (in seconds)' less
#'   than \code{duration_range_1[1]} for Phase 1, less than
#'   \code{duration_range_all[1]} for all other phases).
#'
#' @param p_too_slow Probability that the respondent completed the
#'   task slower than the cutoff time ('Duration (in seconds)' greater
#'   than \code{duration_range_1[2]} for Phase 1, greater than
#'   \code{duration_range_all[2]} for all other phases).
#'
#' @param p_incomplete Probability that the respondent failed to complete the task ('Finished' = FALSE).
#'
#' @param p_cheat Probability that the respondent looked up answers
#'   ('cheat' = "Yes...")
#'
#' @param p_no_consent_all Probability the respondent refused consent
#'   to the full study.
#'
#' @param p_no_consent_phase Probability the respondent refused
#'   consent to a phase of the study.
#'
#' @param p_nonnative Probability the respondent is not a native
#'   English speaker.
#' 
#' @param p_repeater Probability that the respondent just pressed the
#'   same key over and over for at least one phase.
#'
#' @param duration_range_1 Two-element vector giving the range of
#'   acceptable task durations for Phase 1.
#'
#' @param duration_range_all Two-element vector giving the range of
#'   acceptable task durations for Phases 2, 3, and 4.
#'
#' @details Simulates response data and writes a set of CSV files out
#'   to \code{path} in Qualtrics format. The file names are of the
#'   format \code{PXLY.csv}, where X is the phase number (1-4) and Y
#'   is the list number (1-8). So P2L6.csv is the file for phase 2 of
#'   list 6.
#' 
#' @return A character vector with the names of the data files.
#' @export
simulate_resp_files <- function(nsubj,
                                path,
                                overwrite = FALSE,
                                p_too_fast = .01,
                                p_too_slow = .01,
                                p_incomplete = .01,
                                p_cheat = .01,
                                p_no_consent_all = .01,
                                p_no_consent_phase = .01,
                                p_nonnative = .01,
                                p_repeater = .01,
                                duration_range_1 = c(180, 2400),
                                duration_range_all = c(60, 1800)) {

  list_id <- subj_id <- phase_id <- task_id <- trating <- NULL
  
  if (dir.exists(path)) {
    if (!overwrite)
      stop("subdirectory '", path, "' already exists and overwrite = FALSE")
    unlink("path", TRUE, TRUE)
  }
  dir.create(path, FALSE)

  PIDs <- replicate(nsubj, {
    paste0(sample(c(LETTERS, letters), 24L),
           collapse = "")
  })

  p1_qlower <- qnorm(p_too_fast)
  p1_qupper <- qnorm(p_too_slow, lower.tail = FALSE)
  sd_span <- p1_qupper - p1_qlower
  p1_sd <- (duration_range_1[2] - duration_range_1[1]) /
    sd_span
  p1_mean <- duration_range_1[1] + (-p1_qlower * p1_sd)

  all_sd <- (duration_range_all[2] - duration_range_all[1]) /
    sd_span
  all_mean <- duration_range_all[1] + (-p1_qlower * all_sd)
  
  all_mean = (duration_range_all[2] - duration_range_all[1]) / 2 +
    duration_range_all[1]
  all_sd = (duration_range_all[2] - duration_range_1[1]) /
    (2 * qnorm(.99))

  dur_p1 <- as.integer(rnorm(nsubj, p1_mean, p1_sd))
  dur_p1 <- dplyr::if_else(dur_p1 < 0, 0L, dur_p1)

  dur_all <- replicate(3, {
    .ff <- as.integer(rnorm(nsubj, all_mean, all_sd))
    dplyr::if_else(.ff < 0, 0L, .ff)
  })

  ## open-ended question
  oeq <- replicate(rbinom(1, nsubj, .1), 
    "blah blah 'blah'; blah, blah, \"blah\""
  )
  
  ## generate participant info
  pids <- tibble::tibble(
                    subj_id = factor(seq_len(nsubj)),
                    PID = PIDs,
                    dur_1 = dur_p1,
                    dur_2 = dur_all[, 1],
                    dur_3 = dur_all[, 2],
                    dur_4 = dur_all[, 3],
                    Finished = sample(c(TRUE, FALSE),
                                      nsubj, TRUE,
                                      c(1 - p_incomplete,
                                        p_incomplete)),
                    cheat = sample(c(
                      "No, I didn't look any answers up",
                      "Yes, I looked up answer(s)"),
                      nsubj, TRUE, c(1 - p_cheat, p_cheat)),
                    ConsentAll = sample(c(
                      "Yes, I will take part in all 4 phases of the study.",
                      "No, I do not wish to take part in the study."), nsubj, TRUE,
                      c(1 - p_no_consent_all, p_no_consent_all)),
                    Consent = sample(c(
                      "Yes, I confirm that I am over 18 years old, have read and understood the information provided, and consent to participate in this research.",
                      "No, I do not consent to participate in this research."), nsubj, TRUE,
                      c(1 - p_no_consent_phase,
                        p_no_consent_phase)),
                    repeater = sample(c(TRUE, FALSE),
                                      nsubj, TRUE,
                                      c(p_repeater, 1 - p_repeater)),
                    nativelang = sample(c("English", "something else"),
                                        nsubj, TRUE,
                                        c(1 - p_nonnative, p_nonnative)),
                    Q1 = sample(c(oeq, rep("", nsubj - length(oeq)))),
                    Q2_1 = sample(1:5, nsubj, TRUE),
                    Q2_2 = sample(1:5, nsubj, TRUE),
                    Q2_3 = sample(1:5, nsubj, TRUE),
                    Q2_4 = sample(1:5, nsubj, TRUE),
                    Q2_5 = sample(1:5, nsubj, TRUE))

  tpres <-
    presentation_lists[presentation_lists[["task"]] == "truth",
                       c("phase_id", "list_id", "stim_id",
                         "task_id", "order")]

  dat <- gen_data(nsubj) %>%
    dplyr::inner_join(pids, "subj_id")
  dat[["trating"]] <- as.character(dat[["trating"]])
  dat[["trating"]] <-
    dplyr::if_else(dat[["trating"]] == "1",
                   "1 Definitely false", dat[["trating"]])
  dat[["trating"]] <-
    dplyr::if_else(dat[["trating"]] == "7",
                   "7 Definitely true", dat[["trating"]])

  df1 <- dplyr::inner_join(dat, tpres,
                           c("list_id", "stim_id")) %>%
    dplyr::arrange(list_id, subj_id, phase_id, order) %>%
    dplyr::select(list_id, subj_id, phase_id, task_id, trating)

  df1 <- split(df1, list(df1[["list_id"]], df1[["phase_id"]]))

  ## make interest rating data
  irate <- dat[dat[["repetition"]] == "repeated",
               c("subj_id", "list_id", "stim_id")]
  irate[["trating"]] <- sample(c("0 Not at all interesting", 1:9,
                                 "10 Completely interesting"),
                               nrow(irate), TRUE)
  irate[["task"]] <- sprintf("IN%03d", irate[["stim_id"]])

  ilists <-
    split(irate[, c("subj_id", "task", "trating")],
          list(irate[["list_id"]])) %>%
    purrr::map(~ tidyr::pivot_wider(.x,
                                    names_from = "task",
                                    values_from = "trating"))

  invisible(purrr::map2_chr(df1, names(df1), make_response_file,
                            pids, ilists, path))
}

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
  df <- tidyr::pivot_longer(idat, -PID, "stim_id", values_to = "irating")
  valid <- grepl("^[0-9]{1,2}", df[["irating"]])
  if (!all(valid)) {
    stop("invalid interest rating in file'", basename(path),
         "' at line(s) ",
         paste(which(!valid), collapse = ", "))
  }
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
  df <- tidyr::pivot_longer(idat, -PID, "stim_id", values_to = "trating")
  valid <- grepl("^[1-7]{1}.*", df[["trating"]])
  if (!all(valid)) {
    stop("invalid truth rating in file '", basename(path),
         "' at line(s) ",
         paste(which(!valid), collapse = ", "))
  }
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
