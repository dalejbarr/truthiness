#' Reproduce the Master Data Analysis
#'
#' @param path Path to the anonymized data files.
#'
#' @param outfile Path to the HTML output file.
#'
#' @param inferential Whether to run inferential statistics.
#'
#' @param instructions Include instructions on how to reproduce the analysis.
#'
#' @details Runs the R Markdown script on the data in the provided
#'   subdirectory and renders the HTML report to \code{outfile}. The
#'   master R Markdown script can be accessed using:
#'
#' \code{rmarkdown::draft("master_script.Rmd",
#'                        "illusory-truth-analysis", "truthiness")}
#' @export
reproduce_analysis <- function(path,
                               outfile = "analysis.html",
                               inferential = FALSE,
                               instructions = FALSE) {
  if (!dir.exists(path)) {stop("directory '", path, "' does not exist")}
  path <- sub("/$", "", path)
  simulated <- file.exists(file.path(path, ".fake"))
  tf <- tempfile(fileext = ".Rmd")
  doc <- rmarkdown::draft(tf, "illusory-truth-analysis", "truthiness", FALSE, FALSE)
  if (instructions) {
    ## overwrite file, including instructions
    rdoc <- readLines(doc)
    ix <- grep("^<!-- insert instructions", rdoc)
    idoc <- readLines(system.file("repr_instr.Rmd", package = "truthiness"))
    writeLines(c(rdoc[1:(ix - 1L)], idoc, rdoc[(ix + 1L):length(rdoc)]), doc)
  }
  ofile <- rmarkdown::render(doc, output_file = basename(outfile),
                             knit_root_dir = getwd(),
                             output_dir = dirname(outfile),
                             params = list(subdir = path,
                                           simulated = simulated,
                                           inferential = inferential))
  invisible(ofile)
}
