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

#' Read Category Judgments From Response File
#'
#' Reads in the interest ratings from a single response file.
#'
#' @param path Path to the file.
#'
#' @return A long table with the interest ratings.
#'
#' @export
read_cjudgments <- function(path) {

  if (!grepl(rfiles_iregex, basename(path))) {
    stop("Filename '", path,
	 "' not recognized as file that contains category judgments")
  }
  cnames <- grep("^CJ[0-9]{3}$", get_varnames(path), value = TRUE)
  idat <- scrape_cols(path, c("PID", cnames))
  df <- tidyr::pivot_longer(idat, -PID, "stim_code", values_to = "category")
  valid <- sapply(df[["category"]], function(.x) {
    .x %in% levels(truthiness::stimulus_categories[["category"]])
  }, USE.NAMES = FALSE)
  if (!all(valid)) {
    stop("invalid category judgment in file'", basename(path),
         "' at line(s) ",
         paste(which(!valid), collapse = ", "))
  }
  df[["stim_id"]] <- as.factor(as.integer(sub("^CJ", "", df[["stim_code"]])))
  df[["category"]] <- factor(df[["category"]],
                             levels = levels(truthiness::stimulus_categories[["category"]]))
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
  inames <- grep("^CJ[0-9]{3}$", vnames, value = TRUE)
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

#' Import Category Judgments From Multiple Files
#'
#' @param path Directory containing response files.
#'
#' @return A table with the category judgment data.
#' 
#' @export
import_cjudgments <- function(path) {
  ifiles <- dir(sub("/$", "", path), rfiles_iregex, full.names = TRUE)
  df <- tibble::tibble(fname = ifiles)
  df[["data"]] <- purrr::map(df[["fname"]], read_cjudgments)
  tidyr::unnest(df, c(data))[, c("PID", "stim_id", "category")]
}

#' Import Truth Ratings From Multiple Files
#'
#' @param path Directory containing response files.
#'
#' @return A table with the truth rating data.
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

  ## only do the below if we have data from all phases
  ## if (length(ifiles) == 32L) {
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
      unique(c("PID",
               setdiff(pcols[[i]],
                       union(union(pcols[[others[1]]], pcols[[others[2]]]),
                             pcols[[others[3]]]))))
    pdat[[i]][["data2"]] <- purrr::map(pdat[[i]][["data"]],
                                       function(x) x[, unique_cols])
    pdat2[[i]] <- tidyr::unnest(pdat[[i]][, c("list_id", "data2")], c("data2"))    
  }

  has_data <- purrr::map_lgl(pdat2,
                             ~ !(is.null(.x[["PID"]]) | is.null(.x[["list_id"]])))

  df2 <- purrr::reduce(pdat2[has_data], dplyr::full_join, by = c("PID", "list_id"))
  
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
  has_data <- purrr::map_lgl(pcols, ~ (length(.x) > 0L))
  common_cols <- purrr::reduce(pcols[has_data], intersect)
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
#' @param path Path to the directory containing raw response files.
#'
#' @param outpath Path to the directory where anonymized data will be saved.
#'
#' @param report Filename of the HTML preprocessing report.
#' 
#' @return Path where the files were written (\code{outpath}).
#'
#' @details Loads in the data from the raw response files and writes
#'   out non-anonymized, pre-processed versions to the current working
#'   directory, as well as anonymized versions to the directory
#'   specified by \code{outpath}. The structure of the files is
#'   described by the function \code{\link{codebook}}.
#' 
#' @export
preprocess <- function(path,
                       outpath = paste0("anon-", Sys.Date()),
                       report = paste0(basename(outpath),
                                       "-preprocessing.html")) {
  path <- normalize_path(path)
  outpath <- normalize_path(outpath)
  if (!dir.exists(path)) {stop("directory '", path, "' does not exist")}
  tf <- tempfile(fileext = ".Rmd")
  infile <- rmarkdown::draft(tf, "illusory-truth-preprocessing", "truthiness",
                             FALSE, FALSE)
  message("Pre-processing data in '", path, "'")
  ofile <- rmarkdown::render(infile,
                             output_file = file.path(getwd(), report),
                             knit_root_dir = getwd(),
                             output_dir = NULL,
                             params = list(subdir = path,
                                           anondir = outpath))
  file.copy(ofile, report)
  invisible(report)  
}
