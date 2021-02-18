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
#' @param parallel Whether to fit models using a single CPU processing
#'   core (\code{FALSE}) or multiple cores (\code{TRUE}, the
#'   default). If \code{refit} is \code{FALSE}, this parameter is
#'   ignored.
#'
#' @param infile Path to the R Markdown script; \code{NULL} to use the
#'   built-in script.
#'
#' @return A string with the path to the generated HTML report.
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
#' \donttest{
#' tf <- tempfile(fileext = ".html")
#' 
#' ## Run the built-in R Markdown script without refitting models.
#' ## To re-fit the models, set refit = TRUE
#' ## (NB: refitting can take ~ 24 hours)
#' reproduce_analysis(tf)
#'
#' browseURL(tf)
#'
#' ## clean up
#' if (file.exists(tf)) file.remove(tf)
#' }
#'
#' 
#' @export
reproduce_analysis <- function(outfile = "analysis.html",
                               refit = FALSE,
                               savefig = FALSE,
                               recipe = FALSE,
                               parallel = TRUE,
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
                             quiet = TRUE,
                             params = list(recipe = recipe,
                                           savefig = savefig,
                                           refit = refit,
                                           parallel = parallel))
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
#' @param parallel Whether to fit models using a single CPU processing
#'   core (\code{FALSE}) or multiple cores (\code{TRUE}, the
#'   default).
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
#' simulate_resp_files(32, path = td_raw, overwrite = TRUE)
#' 
#' \donttest{
#' ## temporary files 
#' tf1 <- tempfile(fileext = ".html")
#' tf2 <- tempfile(fileext = ".html")
#' 
#' ## run the built-in R Markdown preprocessing script
#' pp_report <- preprocess_simulated(path = td_raw, outpath = td_anon,
#'                                   report = tf1)
#'
#' ## run the built-in R Markdown analysis script
#' ## this can take very long due to the CLMM fits
#' a_report <- reproduce_analysis_sim(path = td_anon,
#'                                    outfile = tf2,
#'                                    parallel = FALSE)
#'
#' browseURL(a_report)
#'
#' ## clean up
#' file.remove(pp_report)
#' file.remove(a_report)
#' }
#'
#' ## clean up
#' unlink(td_raw, TRUE, TRUE)
#' unlink(td_anon, TRUE, TRUE)
#' 
#' @export
reproduce_analysis_sim <- function(path,
                                   outfile = "analysis.html",
                                   recipe = FALSE,
                                   parallel = TRUE,
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
                             envir = new.env(),
                             quiet = TRUE,
                             params = list(subdir = path,
                                           recipe = recipe,
                                           parallel = parallel))
  invisible(ofile)
}
