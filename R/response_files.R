#' Get Rid of Trailing Slash
#'
#' Remove extra trailing slash from file path.
#' 
#' @param path Directory name.
#' 
#' @return Directory name without a trailing slash.
#' 
#' @export
normalize_path <- function(path) {
  sub("/$", "", path)
}

#' Flag Subdirectory as Having Simulated Data
#' 
#' @param path Path to subdirectory.
#'
#' @details This function tags data in a subdirectory as simulated so
#'   that it is not confused with genuine data. When an analysis
#'   report is compiled against data from that subdirectory, the
#'   report will contain a warning that the data is not real.
#'
#' @return No return value, called only for its side effect.
#' 
#' @export
flag_fake <- function(path) {
  writeLines(" ",
             file.path(normalize_path(path), ".fake"))
}

#' Is Data Flagged As Simulated?
#'
#' Check whether the data in the subdirectory is flagged as simulated.
#'
#' @param path Name of the subdirectory.
#'
#' @return \code{TRUE} if the data in the subdirectory is tagged as
#'   simulated, \code{FALSE} otherwise.
#'
#' @export
check_fake <- function(path) {
  file.exists(file.path(normalize_path(path), ".fake"))
}

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
    ## rtib[["gender"]] <-
    rtib[["nationality"]] <-
    rtib[["comments"]] <-
    pad_string

  rtib[["gender"]] <- sample(c("Female", "Male",
                               "Gender Variant", "Not Reported"),
                             nrow(wide_data), TRUE, prob = c(.45, .45, .08, .02))
  
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
    plists[plists[["task"]] == "categorization" &
	   as.integer(plists[["list_id"]]) == id[["L"]],]
  plists3 <- tidyr::crossing(jtib[, "subj_id"],
                             plists2[, c("task_id")])
  plists3[["score"]] <- sample(
    levels(truthiness::stimulus_categories[["category"]]),
    nrow(plists3), TRUE)
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
    ftbl[i, wide_cnames] <- as.character(sample(1:7, 1))
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

#' Simulate Response Data Files From Longitudinal Illusory Truth Study
#'
#' @importFrom stats rbinom
#' 
#' @param nsubj Number of subjects; must be a multiple of 8.
#'
#' @param phase_eff A four-element vector giving the size of the
#'   illusory truth effect at each of the four phases (on the log odds
#'   scale). Use \code{rep(0, 4)} for testing Type I error rate. A
#'   value of .14 gives an effect of approximately 1/10 of a scale
#'   point.
#'
#' @param path Path to subdirectory where resulting files will be
#'   stored; will be created if it does not exist.
#'
#' @param overwrite Whether to overwrite the subdirectory if it
#'   exists.
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
#'   list 6. When we ran a pilot study, we discovered that the data
#'   files had a somewhat different structure from this, but we
#'   nevertheless opted to retain this function rather than rewriting
#'   it to match the new format.
#' 
#' @return A character vector with the names of the data files.
#'
#' @examples
#' td <- tempdir()
#' simulate_resp_files(40, path = td, overwrite = TRUE)
#' dir(td) # show the response files
#' unlink(td, TRUE, TRUE) # cleanup
#' 
#' @export
simulate_resp_files <- function(nsubj,
                                phase_eff = c(0, 0, 0, 0),
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

  path <- normalize_path(path)
  
  if (dir.exists(path)) {
    if (!overwrite)
      stop("subdirectory '", path, "' already exists and overwrite = FALSE")
    unlink(path, TRUE, TRUE)
  }
  dir.create(path, FALSE)
  flag_fake(path)
  
  ## create a file '.fake' to flag that the data are simulated
  writeLines(" ", file.path(path, ".fake"))

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

  plists <- truthiness::presentation_lists
  tpres <-
    plists[plists[["task"]] == "truth",
                       c("phase_id", "list_id", "stim_id",
                         "task_id")]

  dat <- gen_data(nsubj, phase_eff) %>%
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
    dplyr::arrange(list_id, subj_id, phase_id, task_id) %>%
    dplyr::select(list_id, subj_id, phase_id, task_id, trating)

  df1 <- split(df1, list(df1[["list_id"]], df1[["phase_id"]]))

  ## make categorization data
  irate <- dat[dat[["repetition"]] == "repeated",
               c("subj_id", "list_id", "stim_id")]
  irate[["trating"]] <- sample(levels(
    truthiness::stimulus_categories[["category"]]),
    nrow(irate), TRUE)
  irate[["task"]] <- sprintf("CJ%03d", irate[["stim_id"]])

  ilists <-
    split(irate[, c("subj_id", "task", "trating")],
          list(irate[["list_id"]])) %>%
    purrr::map(~ tidyr::pivot_wider(.x,
                                    names_from = "task",
                                    values_from = "trating"))

  invisible(purrr::map2_chr(df1, names(df1), make_response_file,
                            pids, ilists, path))
}

#' Simulate Guessing During the Categorization Task
#'
#' Run simulations tabulating the number of correct guesses assuming a
#' participant is just guessing during the categorization task. This
#' can be used to estimate a chance baseline on the 64 categorization
#' trials.
#' 
#' @param nruns Number of simulation runs.
#'
#' @return A vector of length \code{nruns} with the number of correct guesses.
#'
#' @examples
#' n_correct <- simulate_category_guess(1000)
#' hist(n_correct)
#' mean(n_correct)
#' 
#' @export
simulate_category_guess <- function(nruns = 10000) {
  simcorr <- replicate(nruns, {
    categories <- levels(truthiness::stimulus_categories[["category"]])
    rtbl <- data.frame(
      stim_id = sort(unique(truthiness::stimulus_categories[["stim_id"]])))
    rtbl[["response"]] <- sample(categories, nrow(rtbl) / 2, TRUE)
    rtbl_chk <- dplyr::left_join(rtbl, stimulus_categories,
                                 c("stim_id", "response" = "category"))
    sum(!is.na(rtbl_chk[["choice"]]))
  })
  simcorr
}
