#' Run power simulations
#'
#' @param model Which type of model to fit: use 'lmem' for linear
#'   mixed-effects model, 'clmm' for cumulative logit mixed-effects
#'   model.
#'
#' @param version Which assumption to make about the form of the
#'   repetition-by-interval interaction; 'asymptote' for the
#'   asymptotic version, 'single' for the effect-at-a-single-phase
#'   version (see \code{\link{gen_data}}).
#'
#' @param effect Which effect to test, the main effect (\code{'main'})
#'   or the interaction effect (\code{'interaction'}).
#' 
#' @param nsubj Number of subjects.
#'
#' @param raw_eff Size of the illusory-truth effect, in raw logit
#'   units.
#'
#' @param nruns How many simulations to run.
#'
#' @param outfile Name of output file; \code{NULL} to return the
#'   simulation results.
#'
#' @return Either the name of the outfile (if \code{outfile} is
#'   non-null) or the results of the simulation (a matrix containing
#'   results of \code{\link{fit_lmem}} or \code{\link{fit_clmm}}).
#'
#' @examples
#' set.seed(62)
#' power_sim("lmem", "asymptote", "main", 40, .14, 1, NULL)
#' \dontrun{
#' power_sim("clmm", "asymptote", "main", 24, .14, 1, NULL) # takes ~10 minutes
#' }
#' @export
power_sim <- function(model,
                      version,
                      effect,
                      nsubj,
                      raw_eff,
                      nruns,
                      outfile = sprintf("%s_%s_%s_%04d_128_%05d_%0.2f_%s_%d.rds",
                                        model, version, effect,
                                        nsubj, nruns,
                                        raw_eff,
                                        Sys.info()[["nodename"]],
                                        Sys.getpid())) {
  ## make sure the current directory is writeable
  if (!is.null(outfile)) {
    writeLines("test", ".zzzxyztest")
    if (!file.exists(".zzzxyztest"))
      stop("current directory is not writeable")
    file.remove(".zzzxyztest")

    if (file.exists(outfile))
      stop("file '", outfile, "' already exists")
  }

  if (!(model %in% c("lmem", "clmm")))
    stop("'model' must be either 'lmem' or 'clmm'")

  if (!(version %in% c("asymptote", "single")))
    stop("'version' must be either 'asymptote' or 'single'")

  if (!(effect %in% c("main", "interaction")))
    stop("'effect' must be either 'main' or 'interaction'")

  message("Running ", nruns, " simulations of '", version, "' version.")
  message("Testing ", effect, " effect, ",
          "model type ", model, "...")
  
  stime <- system.time(results <- replicate(nruns, {
    dat <- gen_data(nsubj, raw_eff = raw_eff, version = version)
    do.call(paste0("fit_", model), list(dat))
  }))

  message(nruns, " simulations completed in ",
          round(stime[[3]]), " seconds")

  power <- sum(results["p_RI", ] < .05) / nruns
  message("power for raw effect of ", raw_eff,
          ": ", power)  
  
  if (is.null(outfile)) {
    results
  } else {
    saveRDS(results, outfile)
    message("Results saved to ", outfile)
    cat(outfile, "\n", sep = "")
  }
}
