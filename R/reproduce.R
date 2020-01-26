#' Reproduce the Master Data Analysis
#'
#' @param path Path to the anonymized data files.
#'
#' @param inferential Whether to run inferential statistics.
#'
#' @param outfile Path to the HTML output file.
#'
#' @details Runs the R Markdown script on the data in the provided
#'   subdirectory and renders the HTML report to \code{outfile}. The
#'   master R Markdown script can be accessed using:
#'
#' \code{rmarkdown::draft("master_script.Rmd",
#'                        "illusory-truth-analysis", "truthiness")}
#' @export
reproduce_analysis <- function(path,
                               inferential = FALSE,
                               outfile = "my_analysis.html") {
  tf <- tempfile(fileext = ".Rmd")
  doc <- rmarkdown::draft(tf, "illusory-truth-analysis", "truthiness", FALSE, FALSE)
  ofile <- rmarkdown::render(doc, output_file = outfile,
                             params = list(subdir = path,
                                           inferential = inferential))
  message("Successfully wrote report to ", ofile)
  invisible(ofile)
}
