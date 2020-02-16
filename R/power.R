#' Run power simulations
#'
#' @param model Which type of model to fit: use 'lmem' for linear
#'   mixed-effects model, 'clmm' for cumulative logit mixed-effects
#'   model, and 'gamm' for generalized additive mixed model.
#'
#' @param phase_eff A four-element vector, each element of which
#'   specifies the illusory truth effect at the corresponding phase,
#'   on the log odds scale (see \code{\link{gen_data}}).
#'
#' @param target_effect Which effect to test, the main effect
#'   (\code{'main'}) or the interaction effect (\code{'interaction'}).
#' 
#' @param nsubj Number of subjects.
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
#' power_sim("lmem", c(0, .14, .14, .14), "main", 40, 1, NULL)
#' \dontrun{
#' power_sim("clmm", c(0, .14, .14, .14), "main", 24, 1, NULL) # takes ~10 minutes
#' }
#' @export
power_sim <- function(model,
                      phase_eff,
                      target_effect,
                      nsubj,
                      nruns,
                      outfile =
                        sprintf("%s_%s_%s_%04d_128_%05d_%s_%d.rds",
                                model,
                                sprintf("%0.2f~%0.2f~%0.2f~%0.2f",
                                        phase_eff[1], phase_eff[2],
                                        phase_eff[3], phase_eff[4]),
                                target_effect,
                                nsubj, nruns,
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

  if (!(model %in% c("lmem", "clmm", "gamm")))
    stop("'model' must be one of 'lmem', 'clmm', 'gamm'")

  if (!(target_effect %in% c("main", "interaction")))
    stop("'effect' must be either 'main' or 'interaction'")

  message("Running ", nruns, " simulations with effect profile of ",
          sprintf("%0.2f %0.2f %0.2f %0.2f.",
                  phase_eff[1], phase_eff[2],
                  phase_eff[3], phase_eff[4]))
  message("Testing ", target_effect, " effect, ",
          "model type ", model, "...")
  
  stime <- system.time(results <- replicate(nruns, {
    dat <- gen_data(nsubj, phase_eff = phase_eff)
    do.call(paste0("fit_", model),
            list(dat, main_effect = (target_effect == "main")))
  }))

  message(nruns, " simulations completed in ",
          round(stime[[3]]), " seconds")

  power <- sum(results["p_RI", ] < .05) / nruns
  message("power for effect profile ",
          sprintf("%0.2f %0.2f %0.2f %0.2f", phase_eff[1],
                  phase_eff[2], phase_eff[3], phase_eff[4]),
          ": ", power)  
  
  if (is.null(outfile)) {
    results
  } else {
    saveRDS(results, outfile)
    message("Results saved to ", outfile)
    cat(outfile, "\n", sep = "")
  }
}

#' Power Simulation For Equivalence Test
#'
#' @param phase_eff A four-element vector, each element of which
#'   specifies the illusory truth effect at the corresponding phase,
#'   on the log odds scale (see \code{\link{gen_data}}).
#'
#' @param delta Smallest (raw) effect size of interest, on log odds scale.
#'
#' @param target_effect Which effect to test, the main effect
#'   (\code{'main'}) or the interaction effect (\code{'interaction'}).
#' 
#' @param nsubj Number of subjects.
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
#' power_equiv(c(0, .14, .14, .14), "main", 40, 1, NULL)
#' \dontrun{
#' power_equiv(c(0, .14, .14, .14), "main", 24, 1, NULL) # takes ~10 minutes
#' }
#' @export
power_equiv <- function(phase_eff,
                        delta,
                        target_effect,
                        nsubj,
                        nruns,
                        outfile =
                          sprintf("equiv_%s_%s_%04d_128_%05d_%s_%d.rds",
                                  sprintf("%0.2f~%0.2f~%0.2f~%0.2f~%0.2f",
                                          phase_eff[1], phase_eff[2],
                                          phase_eff[3], phase_eff[4], delta),
                                  target_effect,
                                  nsubj, nruns,
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

  if (!(target_effect %in% c("main", "interaction")))
    stop("'effect' must be either 'main' or 'interaction'")

  message("Running ", nruns, " simulations with effect profile of ",
          sprintf("%0.2f %0.2f %0.2f %0.2f.",
                  phase_eff[1], phase_eff[2],
                  phase_eff[3], phase_eff[4]))
  message("Testing ", target_effect, " effect...")
  
  stime <- system.time(results <- replicate(nruns, {
    dat <- gen_data(nsubj, phase_eff = phase_eff)
    run_equiv(dat, main_effect = (target_effect == "main"), delta)
  }))

  message(nruns, " simulations completed in ", round(stime[[3]]),
          " seconds")

  if (is.null(outfile)) {
    results
  } else {
    saveRDS(results, outfile)
    message("Results saved to ", outfile)
    cat(outfile, "\n", sep = "")
  }
}
