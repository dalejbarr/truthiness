#' Identify data files
#'
#' @param path Path to data files.
#'
#' @details Looks for files matching the regular expression
#'   \code{^[Pp][1-4]\\.[Cc][Ss][Vv]$} and performs basic
#'   error-checking.
#'
#' @return A character vector with the paths to the files.
#' @export
locate_data_files <- function(path, full.names = TRUE) {
  if (!dir.exists(path)) {
    stop("path '", path, "' does not exist")
  }
  res <- sort(dir(sub("/$", "", path), "^[Pp][1-4]\\.[Cc][Ss][Vv]$",
                  full.names = full.names))
  if (length(res) == 0L) {
    stop("No data files of the form 'P1.csv' found in path '", path, ", '.")
  } else if (length(res) > 4L) {
    stop("Number of data files of the form 'P1.csv' (", length(res), ") ",
         "exceeds number of phases (4):\n",
         paste0("'", paste(res, collapse = "', '"), "'"))
  }

  ## check for gaps in the sequence
  phases <- as.integer(sub(".*[Pp]([1-4])\\.[Cc][Ss][Vv]$", "\\1", res))
  if (length(phases) != max(phases)) {
    missing_files <- paste0("P", setdiff(seq_len(max(phases)), phases), ".csv")
    stop("missing ",
         if (length(missing_files) == 1L) "file '" else "files '",
         paste0(paste(missing_files, collapse = "', '"), "'"))
  }
  
  res
}

get_varnames <- function(path) {
  ## read in the header row from a CSV file
  df <- suppressWarnings({readr::read_csv(readLines(path)[-c(2:3)])})
  names(df)
}

scrape_cols <- function(path, cols) {
  ## read in specified columns from the CSV file
  df <- suppressWarnings({readr::read_csv(readLines(path)[-c(2:3)],
                                          col_types = readr::cols(.default = readr::col_character()))})
  df[, cols]
}

scrape_cols <- function(path, cols) {
  ## read in specified columns from the CSV file
  df <- suppressWarnings({readr::read_csv(readLines(path)[-c(2:3)],
                                          col_types = readr::cols(.default = readr::col_character()))})
  df[, cols]
}

#' Read Category Judgments From Raw Data File
#'
#' Reads in the interest ratings from a single response file.
#'
#' @param path Path to the file.
#'
#' @return A long table with the interest ratings.
#'
#' @export
read_cjudgments <- function(path) {
  if (!grepl("^[Pp]1\\.[Cc][Ss][Vv]$", basename(path))) {
    stop("Filename '", path,
	 "' not recognized as file that contains category judgments")
  }
  newpath <- prolific_cleanup(path)
  cnames <- grep("^CJ[0-9]{3}_?", get_varnames(newpath), value = TRUE)
  idat <- scrape_cols(newpath, c("PROLIFIC_PID", cnames))
  names(idat)[1] <- "PID"
  df_0 <- tidyr::pivot_longer(idat, -PID, "stim_code", values_to = "category")
  df <- df_0[!is.na(df_0[["category"]]), ]
  df[["stim_code"]] <- sub("_[1-9]$", "", df[["stim_code"]])
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

#' Read Category Judgments From Simulated Data File
#'
#' Reads in the interest ratings from a single response file.
#'
#' @param path Path to the file.
#'
#' @return A long table with the interest ratings.
#'
#' @export
read_cjudgments_simulated <- function(path) {
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

#' Read Truth Ratings From Raw Data
#'
#' Reads in the truth ratings from a single response file.
#'
#' @param path Path to the file.
#'
#' @return A long table with the truth ratings.
#'
#' @export
read_tratings <- function(path) {
  if (!grepl("^[Pp][1-4]\\.[Cc][Ss][Vv]$", basename(path))) {
    stop("Filename '", path,
	 "' not recognized as file that contains truth ratings")
  }
  newpath <- prolific_cleanup(path)
  cnames <- grep("^TR[0-9]{3}_?", get_varnames(newpath), value = TRUE)
  idat <- scrape_cols(newpath, c("PROLIFIC_PID", cnames))
  names(idat)[1] <- "PID"
  df_0 <- tidyr::pivot_longer(idat, -PID, "stim_code", values_to = "trating")
  df <- df_0[!is.na(df_0[["trating"]]), ]
  df[["stim_code"]] <- sub("_[1-9]{1}$", "", df[["stim_code"]])
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

#' Read Truth Ratings From Simulated Data
#'
#' Reads in the truth ratings from a single response file.
#'
#' @param path Path to the file.
#'
#' @return A long table with the truth ratings.
#'
#' @export
read_tratings_simulated <- function(path) {
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

## Clean up line-breaks from prolific file (replace with spaces)
## Prolific seems to use 0x0D 0x0A AS line breaks
## but can also have breaks within a field that are not treated as new lines
## creating an UTTER NIGHTMARE for import
## returns a string with the cleaned data
prolific_cleanup <- function(filename) {
  b1 <- readr::read_file(filename)
  b2 <- gsub("\r\n", "ZzOogaBoogazZ", b1) # replace \r\n with special code
  b3 <- gsub("\n", " ", b2) # replace \n with spaces
  b4 <- gsub("ZzOogaBoogazZ", "\n", b3) # replace code with \n
  tf <- tempfile(fileext = ".csv")
  readr::write_file(b4, tf)
  tf
}

#' Read Session Information From Simulated Response File
#'
#' Read in session information from a single response file.
#'
#' @param path Path to the file.
#'
#' @return A table with session information.
#' 
#' @export
read_sessions_simulated <- function(path) {
  vnames <- get_varnames(path)
  tnames <- grep("^TR[0-9]{3}$", vnames, value = TRUE)
  inames <- grep("^CJ[0-9]{3}$", vnames, value = TRUE)
  idat <- scrape_cols(path, setdiff(vnames, c(tnames, inames)))
  idat[, c("PID", setdiff(names(idat), "PID"))]
}


#' Read Session Information From Simulated Response File
#'
#' Read in session information from a single response file.
#'
#' @param path Path to the file.
#'
#' @return A table with session information.
#' 
#' @export
read_sessions <- function(path) {
  newpath <- prolific_cleanup(path)
  vnames <- get_varnames(newpath)
  tnames <- grep("^TR[0-9]{3}.*$", vnames, value = TRUE)
  inames <- grep("^CJ[0-9]{3}.*$", vnames, value = TRUE)
  idat <- scrape_cols(newpath, setdiff(vnames, c(tnames, inames)))
  idat2 <- idat[, c("PROLIFIC_PID", setdiff(names(idat),
                                            c("PID", "PROLIFIC_PID")))]
  ## check for weird format of phase 1 file
  if (length(intersect(names(idat2), paste("Group", 1:8, sep = "_"))) == 8L) {
    cx <- grep("^Group_[1-8]", names(idat2))
    idat2[["Group"]] <- character(nrow(idat2))
    for (i in seq_len(nrow(idat2))) {
      not_na <- which(!is.na(idat2[i, cx]))
      idat2[["Group"]][[i]] <- sub("^Group_", "", names(idat2)[cx][not_na])
    }
  }
  names(idat2)[1] <- "PID"
  names(idat2) <- sub("^Group$", "list_id", names(idat2))
  idat3 <- idat2[,
                 setdiff(names(idat2), paste("Group", 1:8, sep = "_"))]
  idat3[, c("PID", "list_id", setdiff(names(idat3), c("PID", "list_id")))]
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

#' Import Category Judgments From Simulated Data
#'
#' @param path Directory containing response files.
#'
#' @return A table with the category judgment data.
#' 
#' @export
import_cjudgments_simulated <- function(path) {
  ifiles <- dir(sub("/$", "", path), rfiles_iregex, full.names = TRUE)
  df <- tibble::tibble(fname = ifiles)
  df[["data"]] <- purrr::map(df[["fname"]], read_cjudgments_simulated)
  tidyr::unnest(df, c(data))[, c("PID", "stim_id", "category")]
}

#' Import Category Judgments From Raw Data
#'
#' @param path Directory containing response files.
#'
#' @return A table with the category judgment data.
#' 
#' @export
import_cjudgments <- function(path) {
  ifiles <- dir(sub("/$", "", path), "^[Pp]1\\.[Cc][Ss][Vv]$", full.names = TRUE)
  df <- tibble::tibble(fname = ifiles)
  df[["data"]] <- purrr::map(df[["fname"]], read_cjudgments)
  tidyr::unnest(df, c(data))[, c("PID", "stim_id", "category")]
}

#' Import Truth Ratings From Simulated Data
#'
#' @param path Directory containing response files.
#'
#' @return A table with the truth rating data.
#' 
#' @export
import_tratings_simulated <- function(path) {
  ifiles <- dir(sub("/$", "", path), rfiles_regex, full.names = TRUE)
  phase_id <- factor(as.integer(substr(basename(ifiles), 2, 2)), levels = 1:4)
  df <- tibble::tibble(fname = ifiles,
                       phase_id = phase_id)
  df[["data"]] <- purrr::map(df[["fname"]], read_tratings_simulated)
  tidyr::unnest(df, c(data))[, c("PID", "phase_id", "stim_id", "trating")]
}

#' Import Truth Ratings From Raw Data
#'
#' @param path Directory containing response files.
#'
#' @return A table with the truth rating data.
#' 
#' @export
import_tratings <- function(path) {
  ifiles <- locate_data_files(path)
  phase_id <- factor(as.integer(substr(basename(ifiles), 2, 2)), levels = 1:4)
  df <- tibble::tibble(fname = ifiles,
                       phase_id = phase_id)
  df[["data"]] <- purrr::map(df[["fname"]], read_tratings)
  tidyr::unnest(df, c(data))[, c("PID", "phase_id", "stim_id", "trating")]
}

#' Import Session Information From Simulated Data
#'
#' @param path Directory containing response files.
#'
#' @return A table with session information.
#'
#' @export
import_sessions_simulated <- function(path) {
  ifiles <- dir(sub("/$", "", path), rfiles_regex, full.names = TRUE)
  list_id <- factor(as.integer(substr(basename(ifiles), 4, 4)), levels = 1:8)
  phase_id <- factor(as.integer(substr(basename(ifiles), 2, 2)), levels = 1:4)
  df <- tibble::tibble(fname = ifiles,
                       list_id = list_id,
                       phase_id = phase_id)
  df[["data"]] <- purrr::map(df[["fname"]], read_sessions_simulated)

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

#' Import Session Information From Raw Data
#'
#' @param path Directory containing response files.
#'
#' @return A table with session information.
#'
#' @export
import_sessions <- function(path) {
  ifiles <- locate_data_files(path)

  phase_id <- sub(".+P([1-4]{1})\\.[Cc][Ss][Vv]$", "\\1", ifiles)
  df <- tibble::tibble(fname = ifiles,
                       phase_id = phase_id)
  
  df[["data"]] <- purrr::map(df[["fname"]], read_sessions)

  ## if we only have one phase
  if (length(ifiles) == 1L) {
    df[["data"]][[1]][, c("PID", "list_id", "ConsentAll", "Age",
                          "Gender", "NativeLang", "Nationality")]
  } else {
    ## only do the below if we have data from more than one phase
    ## lets figure out the columns that only appear in a single session
    phases <- as.integer(sub(".*[Pp]([1-4])\\.[Cc][Ss][Vv]$", "\\1",
                             locate_data_files(path)))

    cnames <- purrr::map(df[["data"]], names)
    by_phase <- split(cnames, df[["phase_id"]])
    pdat <- split(df, df[["phase_id"]])
    pcols <- purrr::map(by_phase, function(x) {unique(unlist(x))})
    ## get the columns unique to each phase
    pdat2 <- list()

    for (i in phases) {
      others <- setdiff(phases, i)
      unique_cols <-
        unique(c("PID", "list_id", 
                 setdiff(pcols[[i]],
                         union(union(pcols[[others[1]]], pcols[[others[2]]]),
                               pcols[[others[3]]]))))
      pdat[[i]][["data2"]] <- purrr::map(pdat[[i]][["data"]],
                                         function(x) x[, unique_cols])
      pdat2[[i]] <- tidyr::unnest(pdat[[i]][, c("data2")], c("data2"))    
    }
    has_data <- purrr::map_lgl(
                         pdat2,
                         ~ !(is.null(.x[["PID"]]) | is.null(.x[["list_id"]])))
    
    df2 <- purrr::reduce(pdat2[has_data],
                         dplyr::full_join, by = c("PID", "list_id"))
    df2[["Age"]] <- as.integer(df2[["Age"]])
    
    df2[, c("PID", "list_id", setdiff(names(df2), c("PID", "list_id")))]
  }
}

#' Import Phase Information From Raw Data
#'
#' @param path Directory containing response files.
#'
#' @return A table with phase information.
#'
#' @export
import_phase_info <- function(path) {
  ifiles <- locate_data_files(path)

  phase_id <- factor(as.integer(substr(basename(ifiles), 2, 2)), levels = 1:4)
  df <- tibble::tibble(fname = ifiles,
                       phase_id = phase_id)

  df[["data"]] <- purrr::map(df[["fname"]], read_sessions)

  if (length(ifiles) == 1L) {
    df2 <- tidyr::unnest(df[1, c("phase_id", "data")], data)
    df2[, c("PID", "list_id", "phase_id", "StartDate", "EndDate", "Status", "Progress",
            "Duration (in seconds)", "Finished", "RecordedDate", "ResponseId",
            "DistributionChannel", "UserLanguage", "Consent", "TechDiff", "Cheat")]
  } else {
    ## lets figure out the columns that only appear in a single session
    phases <- as.integer(sub(".*[Pp]([1-4])\\.[Cc][Ss][Vv]$", "\\1",
                             locate_data_files(path)))
    
    cnames <- purrr::map(df[["data"]], names)
    by_phase <- split(cnames, df[["phase_id"]])
    pdat <- split(df, df[["phase_id"]])
    pcols <- purrr::map(by_phase, function(x) {unique(unlist(x))})
    ## get the columns common to all phases
    has_data <- purrr::map_lgl(pcols, ~ (length(.x) > 0L))
    common_cols <- purrr::reduce(pcols[has_data], intersect)
    pdat2 <- list()

    for (i in phases) {
      pdat[[i]][["data2"]] <- purrr::map(pdat[[i]][["data"]],
                                         function(x) x[, common_cols])
      pdat2[[i]] <- tidyr::unnest(pdat[[i]][, c("phase_id", "data2")],
                                  c("data2"))
    }
    df2 <- dplyr::bind_rows(pdat2)
    df2[["Duration (in seconds)"]] <- as.integer(df2[["Duration (in seconds)"]])
    
    df2[, c("PID", "list_id", "phase_id",
            setdiff(names(df2), c("PID", "list_id", "phase_id")))]
  }
}

#' Import Phase Information From Simulated Data
#'
#' @param path Directory containing response files.
#'
#' @return A table with phase information.
#'
#' @export
import_phase_info_simulated <- function(path) {
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

#' Pre-process and Anonymize Simulated Response Data
#'
#' @param path Path to the directory containing simulated response files.
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
preprocess_simulated <- function(path,
                       outpath = paste0("anon-", Sys.Date()),
                       report = paste0(basename(outpath),
                                       "-preprocessing.html")) {
  path <- normalize_path(path)
  outpath <- normalize_path(outpath)
  if (!dir.exists(path)) {stop("directory '", path, "' does not exist")}
  tf <- tempfile(fileext = ".Rmd")
  infile <- rmarkdown::draft(tf, "illusory-truth-preprocessing-sim", "truthiness",
                             FALSE, FALSE)
  message("Pre-processing (simulated) data in '", path, "'")
  ofile <- rmarkdown::render(infile,
                             output_file = file.path(getwd(), report),
                             knit_root_dir = getwd(),
                             output_dir = NULL,
                             params = list(subdir = path,
                                           anondir = outpath))
  file.copy(ofile, report)
  invisible(report)  
}

#' Pre-process and Anonymize Raw Response Data
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
  message("Pre-processing raw data in '", path, "'")
  ofile <- rmarkdown::render(infile,
                             output_file = file.path(getwd(), report),
                             knit_root_dir = getwd(),
                             output_dir = NULL,
                             params = list(subdir = path,
                                           anondir = outpath))
  file.copy(ofile, report)
  invisible(report)  
}
