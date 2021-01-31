#' Reproduce the Master Data Analysis on Raw Data
#'
#' @param outfile Path to the HTML output file.
#'
#' @param recipe Include instructions on how to reproduce the analysis.
#'
#' @param infile Path to the R Markdown script; \code{NULL} to use the
#'   built-in script.
#' 
#' @details Runs R Markdown script on the data in the provided
#'   subdirectory and renders the HTML report to \code{outfile}. The
#'   master R Markdown script can be accessed using:
#'
#' \code{rmarkdown::draft("master_script.Rmd",
#'                        "illusory-truth-analysis", "truthiness")}
#' 
#' @export
reproduce_analysis <- function(outfile = "analysis.html",
                               recipe = FALSE,
                               infile = NULL) {

  if (is.null(infile)) {
    tf <- tempfile(fileext = ".Rmd")
    infile <- rmarkdown::draft(tf, "illusory-truth-analysis", "truthiness",
                               FALSE, FALSE)
    message("Processing built-in analysis script")
  } else {
    message("Processing '", infile, "'")
  }
  ofile <- rmarkdown::render(infile, output_file = basename(outfile),
                             knit_root_dir = getwd(),
                             envir = new.env(),
                             output_dir = dirname(outfile),
                             params = list(recipe = recipe))
  invisible(ofile)
}

#' Reproduce the Master Data Analysis on Simulated Data
#'
#' @param path Path to the anonymized data files.
#'
#' @param outfile Path to the HTML output file.
#'
#' @param recipe Include instructions on how to reproduce the analysis.
#'
#' @param infile Path to the R Markdown script; \code{NULL} to use the
#'   built-in script.
#' 
#' @details Runs R Markdown script on the data in the provided
#'   subdirectory and renders the HTML report to \code{outfile}. The
#'   master R Markdown script can be accessed using:
#'
#' \code{rmarkdown::draft("master_script.Rmd",
#'                        "illusory-truth-analysis-sim", "truthiness")}
#' 
#' @export
reproduce_analysis_sim <- function(path,
                               outfile = "analysis.html",
                               recipe = FALSE,
                               infile = NULL) {

  path <- normalize_path(path)
  if (!dir.exists(path)) {stop("directory '", path, "' does not exist")}
  if (is.null(infile)) {
    tf <- tempfile(fileext = ".Rmd")
    infile <- rmarkdown::draft(tf, "illusory-truth-analysis-sim", "truthiness",
                               FALSE, FALSE)
    message("Processing built-in script against data in '", path, "'")
  } else {
    message("Processing '", infile, "' against data in '", path, "'")
  }
  ofile <- rmarkdown::render(infile, output_file = basename(outfile),
                             knit_root_dir = getwd(),
                             output_dir = dirname(outfile),
                             params = list(subdir = path,
                                           recipe = recipe))
  invisible(ofile)
}
