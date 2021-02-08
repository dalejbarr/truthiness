#' Reproduce the Analysis for Longitudinal Illusory Truth Study
#'
#' Re-run the analysis for the
#' \insertCite{Henderson_Simons_Barr_2021;textual}{truthiness}
#' longitudinal truth study. 
#' 
#' @param outfile Path to the HTML output file.
#'
#' @param recipe Include instructions on how to reproduce the analysis.
#'
#' @param refit Whether to re-fit the cumulative link mixed model
#'   \code{TRUE} or to use the built-in model fits (\code{FALSE}). Due
#'   to the extremely time-consuming nature of model estimation, the
#'   default is set to \code{FALSE}.
#'
#' @param savefig Whether to save the two plots as separate PNG files
#'   (\code{means_plot.png} and \code{validation_plot.png}).
#'
#' @param infile Path to the R Markdown script; \code{NULL} to use the
#'   built-in script.
#'
#' @details Runs R Markdown script containing the analysis code. The
#'   analysis is performed on the built-in preprocessed anonymized
#'   data (documented in \code{\link{truth_trajectory_data}}). The
#'   script output is rendered as an HTML report, specified by
#'   \code{outfile}. Although it is not necessary to do so, the master
#'   R Markdown script for processing real data can be accessed using
#'
#' \code{rmarkdown::draft("analysis.Rmd",
#'                        "illusory-truth-analysis", "truthiness")}
#' 
#' @return Path to the rendered HTML report.
#'
#' @seealso \code{\link{reproduce_analysis_sim}}
#'
#' @examples
#' tf <- tempfile(fileext = ".html")
#'
#' result <- reproduce_analysis(tf) # without re-fitting; default
#'
#' \dontrun{
#' browseURL(result)
#'
#' ## to re-fit the data (takes VERY long):
#' result <- reproduce_analysis(refit = TRUE)
#' }
#' 
#' @export
reproduce_analysis <- function(outfile = "analysis.html",
                               refit = FALSE,
                               savefig = FALSE,
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
                             params = list(recipe = recipe,
                                           savefig = savefig,
                                           refit = refit))
  invisible(ofile)
}

#' Simulate the Analysis for Longitudinal Illusory Truth Study
#'
#' Runs the main analysis for
#' \insertCite{Henderson_Simons_Barr_2021;textual}{truthiness} on
#' simulated data.
#' 
#' @param path Path to a subdirectory containing the preprocessed
#'   anonymized (simulated) data files (see
#'   \code{\link{simulate_resp_files}}).
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
#' \code{rmarkdown::draft("analysis.Rmd",
#'                        "illusory-truth-analysis-sim", "truthiness")}
#'
#' Note that this script can take *very* long to run, depending on the
#' size of the simulated dataset, the number of processing cores, and
#' the computational power of the hardware.
#'
#' @examples
#' td_raw <- tempfile()  # temp dir for raw data
#' td_anon <- tempfile() # temp dir for preprocessed data
#'
#' ## simulate data and preprocess it
#'
#' set.seed(62)
#' simulate_resp_files(40, path = td_raw, overwrite = TRUE)
#' report <- preprocess_simulated(td_raw, td_anon)
#' 
#' \dontrun{
#' reproduce_analysis_sim(td_anon)
#' }
#'
#' ## clean up
#' file.remove(report)
#' unlink(td_raw, TRUE, TRUE)
#' unlink(td_anon, TRUE, TRUE)
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
