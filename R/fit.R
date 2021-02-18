#' @importFrom stats deviance
#' @importFrom stats logLik
#' @importFrom stats pchisq
NULL

#' Get Ratings Data with Model Predictors
#'
#' Apply participant/phase-level exclusions and then add numeric and
#' factor predictors to the ratings data.
#'
#' @return A data frame, with columns:
#' 
#' \describe{
#'   \item{subj_id}{Unique subject identifier.}
#'   \item{stim_id}{Unique stimulus identifier.}
#'   \item{repetition}{Whether the statement was repeated or new.}
#'   \item{interval}{Presentation interval.}
#'   \item{R}{Deviation-coded numerical predictor for repetition.}
#'   \item{I1}{Deviation-coded numerical predictor for interval (1 day vs. immediate).}
#'   \item{I2}{Deviation-coded numerical predictor for interval (1 week vs. immediate).}
#'   \item{I3}{Deviation-coded numerical predictor for interval (1 month vs. immediate).}
#'   \item{Rep}{Deviation-coded factor for repetition.}
#'   \item{Int}{Deviation-coded factor for interval.}
#' }
#'
#' @examples
#' get_model_data()
#' 
#' @seealso \code{\link{truth_trajectory_data}}
#'
#' @export
get_model_data <- function() {
  keep <- ID <- list_id <- trating <- repetition <- interval <-
    subj_id <- stim_id <- R <- I1 <- I2 <- I3 <- Rep <- Int <- NULL
  truthiness::ratings %>%
    dplyr::inner_join(truthiness::phases %>%
                      dplyr::filter(keep), c("ID", "phase_id")) %>%
    dplyr::inner_join(truthiness::sessions %>%
                      dplyr::select(ID, list_id), "ID") %>%
    dplyr::inner_join(stimulus_conditions, c("list_id", "stim_id")) %>%
    dplyr::mutate(subj_id = factor(ID),
           T = factor(trating, levels = 1:7, ordered = TRUE),
           R = dplyr::if_else(repetition == "repeated", 1/2, -1/2),
           I1 = dplyr::if_else(interval == "1 day", 3/4, -1/4),
           I2 = dplyr::if_else(interval == "1 week", 3/4, -1/4),
           I3 = dplyr::if_else(interval == "1 month", 3/4, -1/4),
           Rep = C(repetition,
                   matrix(c(.5, -.5), nrow = 2,
                          dimnames = list(c("repeated", "new")))),
           Int = C(interval,
                   matrix(c(-1/4, -1/4, -1/4,
                            3/4, -1/4, -1/4,
                            -1/4,  3/4, -1/4,
                            -1/4, -1/4,  3/4),
                          nrow = 4, byrow = TRUE,
                          dimnames = list(c("immediate", "1 day",
                                            "1 week", "1 month"),
                                          c("I1", "I2", "I3"))))) %>%
    dplyr::select(subj_id, stim_id, repetition, interval,
                  R, I1, I2, I3, T,
                  Rep, Int)  
}

## run lme4::lmer with error trapping
tryFit <- function(tf.formula, tf.data, ...) {
  converged <- TRUE
  w.handler <- function(w) {
    converged <- FALSE
    invokeRestart("muffleWarning")
  }
  arg.list <- c(list(formula=tf.formula, data=tf.data), list(...))
  list(value=withCallingHandlers(tryCatch(
         do.call(lme4::lmer, arg.list),
         error=function(e) e),
         warning=w.handler),
       converged=converged)
}

#' Fit Linear Mixed-Effects Model to Simulated Ratings
#'
#' Fit a linear mixed-effects model (LMM) to simulated ratings data.
#' 
#' @param .data Data frame, with the format as resulting from a call
#'   to \code{\link{gen_data}}.
#' 
#' @param main_effect Whether to test the main effect of repetition
#'   (TRUE) or the repetition-by-interval interaction (FALSE; the
#'   default).
#'
#' @details This function is used to estimate parameters for power
#'   analysis with simulated data. \code{fit_lmem} fits a linear-mixed
#'   effects model to the data with \code{\link[lme4]{lmer}} and tests
#'   the specified effect (interaction or main effect) using a
#'   likelihood-ratio test. If the interaction is to be tested, the
#'   following two models are compared.
#'
#' \code{trating ~ R * (I1 + I2 + I3) +
#'    (1 + R:I1 + R:I2 + R:I3 || subj_id) +
#'    (1 + R:I1 + R:I2 + R:I3 || stim_id)}
#'
#' \code{trating ~ R + I1 + I2 + I3 +
#'    (1 + R:I1 + R:I2 + R:I3 || subj_id) +
#'    (1 + R:I1 + R:I2 + R:I3 || stim_id)}.
#'
#' If the main effect is to be tested, then the following two models
#' are compared.
#'
#' \code{trating ~ R * (I1 + I2 + I3) +
#'    (1 + R || subj_id) +
#'    (1 + R || stim_id)}
#'
#' \code{trating ~ I1 + I2 + I3 + R:I1 + R:I2 + R:I3) +
#'    (1 + R || subj_id) +
#'    (1 + R || stim_id)}.
#'
#' @return A vector, with the following elements.
#' \describe{
#'   \item{\code{(Intercept)}}{Fixed-effects estimate of the intercept.}
#'   \item{\code{R}}{Fixed-effects estimate of the main effect of repetition.}
#'   \item{\code{I1}}{Fixed-effects estimate of the main effect of interval (1).}
#'   \item{\code{I2}}{Fixed-effects estimate of the main effect of interval (2).}
#'   \item{\code{I3}}{Fixed-effects estimate of the main effect of interval (3).}
#'   \item{\code{R:I1}}{Fixed-effects estimate of the interaction (1).}
#'   \item{\code{R:I2}}{Fixed-effects estimate of the interaction (2).}
#'   \item{\code{R:I3}}{Fixed-effects estimate of the interaction (3).}
#'   \item{dev1}{Deviance for the model including the effect(s) of interest.}
#'   \item{dev2}{Deviance for the model excluding the effect(s) of interest.}
#'   \item{chisq_RI}{Chi-square value for the likelihood ratio test.}
#'   \item{p_RI}{Associated p-value.}
#'   \item{m1_singular}{Whether the covariance matrix for model 1 was singular.}
#'   \item{m2_singular}{Whether the covariance matrix for model 2 was singular.}
#'   \item{m1_conv}{Whether model 1 converged.}
#'   \item{m2_conv}{Whether model 2 converged.}
#' }
#'
#' @seealso \code{\link{gen_data}}, \code{\link{power_sim}}.
#'
#' @examples
#' set.seed(62)
#' dat <- gen_data(40) 
#' fit_lmem(dat, TRUE) # test main effect
#'
#' @export
fit_lmem <- function(.data, main_effect = FALSE) {
  form <- form2 <- NULL

  .data[["trating"]] <- as.integer(.data[["trating"]])
  
  if (main_effect) {
    form <- trating ~ R + I1 + I2 + I3 + R:I1 + R:I2 + R:I3 +
      (1 + R || subj_id) +
      (1 + R || stim_id)
    
    form2 <- trating ~ I1 + I2 + I3 + R:I1 + R:I2 + R:I3 +
      (1 + R || subj_id) +
      (1 + R || stim_id)
  } else {
    form <- trating ~ R * (I1 + I2 + I3) +
      (1 + R:I1 + R:I2 + R:I3 || subj_id) +
      (1 + R:I1 + R:I2 + R:I3 || stim_id)
    
    form2 <- trating ~ R + I1 + I2 + I3 +
      (1 + R:I1 + R:I2 + R:I3 || subj_id) +
      (1 + R:I1 + R:I2 + R:I3 || stim_id)
  }

  ## fit the model and print the results
  el1 <- suppressMessages({tryFit(form, .data, REML = FALSE)})
  el2 <- suppressMessages({tryFit(form2, .data, REML = FALSE)})

  fef <- lme4::fixef(el1$value)

  mychisq1 <- deviance(el2$value) - deviance(el1$value)
  pval1 <- pchisq(abs(mychisq1), 3, lower.tail = FALSE)

  c(fef, dev1 = deviance(el1$value), dev2 = deviance(el2$value), 
    chisq_RI = mychisq1, p_RI = pval1,
    m1_singular = lme4::isSingular(el1$value),
    m2_singular = lme4::isSingular(el2$value),
    m1_conv = el1$converged,
    m2_conv = el2$converged)
}

#' Fit Cumulative Link Mixed-Effects Model to Simulated Ratings
#'
#' @inheritParams fit_lmem
#' 
#' @details Fits a cumulative link mixed-effects model to the data
#'   and tests the specified effect (interaction or main effect) using
#'   a likelihood-ratio test using \code{ordinal::clmm()}. The
#'   function's main purpose is to be used in power simulation.
#'
#' If the interaction is to be tested, the following two models are
#'   compared:
#'
#' \code{trating ~ R * (I1 + I2 + I3) +
#'    (1 + R:I1 + R:I2 + R:I3 | subj_id) +
#'    (1 + R:I1 + R:I2 + R:I3 | stim_id)}
#'
#' \code{trating ~ R + I1 + I2 + I3 +
#'    (1 + R:I1 + R:I2 + R:I3 | subj_id) +
#'    (1 + R:I1 + R:I2 + R:I3 | stim_id)}.
#'
#' If the main effect is to be tested, then the following two models
#' are compared.
#'
#' \code{trating ~ R * (I1 + I2 + I3) +
#'    (1 + R | subj_id) +
#'    (1 + R | stim_id)}
#'
#' \code{trating ~ I1 + I2 + I3 + R:I1 + R:I2 + R:I3) +
#'    (1 + R | subj_id) +
#'    (1 + R | stim_id)}.
#' 
#' @return A vector, with the following elements.
#' \describe{
#'   \item{\code{R}}{Fixed-effects estimate of the main effect of repetition.}
#'   \item{\code{I1}}{Fixed-effects estimate of the main effect of interval (1).}
#'   \item{\code{I2}}{Fixed-effects estimate of the main effect of interval (2).}
#'   \item{\code{I3}}{Fixed-effects estimate of the main effect of interval (3).}
#'   \item{\code{R:I1}}{Fixed-effects estimate of the interaction (1).}
#'   \item{\code{R:I2}}{Fixed-effects estimate of the interaction (2).}
#'   \item{\code{R:I3}}{Fixed-effects estimate of the interaction (3).}
#'   \item{dev1}{Deviance for the model including the effect(s) of interest.}
#'   \item{dev2}{Deviance for the model excluding the effect(s) of interest.}
#'   \item{chisq_RI}{Chi-square value for the likelihood ratio test.}
#'   \item{p_RI}{Associated p-value.}
#'   \item{thresh.1|2}{First cut-point (threshold).}
#'   \item{thresh.2|3}{Second cut-point.}
#'   \item{thresh.3|4}{Third cut-point.}
#'   \item{thresh.4|5}{Fourth cut-point.}
#'   \item{thresh.5|6}{Fifth cut-point.}
#'   \item{thresh.6|7}{Sixth cut-point.}
#' }
#'
#' @seealso \code{\link{gen_data}}, \code{\link{power_sim}}.
#'
#' @examples
#'
#' set.seed(62)
#' dat <- gen_data(24) # test main effect
#' \donttest{
#' fit_clmm(dat, TRUE) # takes a few minutes
#' }
#'
#' @export
fit_clmm <- function(.data, main_effect = FALSE) {
  if (main_effect) {
    form <- trating ~ R + I1 + I2 + I3 + R:I1 + R:I2 + R:I3 +
      (1 + R | subj_id) +
      (1 + R | stim_id)
    
    form2 <- trating ~ I1 + I2 + I3 + R:I1 + R:I2 + R:I3 +
      (1 + R | subj_id) +
      (1 + R | stim_id)    
  } else {
    form <- trating ~ R * (I1 + I2 + I3) +
      (1 + R:I1 + R:I2 + R:I3 | subj_id) +
      (1 + R:I1 + R:I2 + R:I3 | stim_id)
    
    form2 <- trating ~ R + I1 + I2 + I3 +
      (1 + R:I1 + R:I2 + R:I3 | subj_id) +
      (1 + R:I1 + R:I2 + R:I3 | stim_id)
  }

  ## fit the model and print the results
  browser()
  el1 <- ordinal::clmm(form, .data, Hess = FALSE)
  el2 <- ordinal::clmm(form2, .data, Hess = FALSE)

  fef <- el1$beta

  dev1 <- as.numeric(-2 * logLik(el1))
  dev2 <- as.numeric(-2 * logLik(el2))

  mychisq1 <- dev2 - dev1
  pval1 <- pchisq(abs(mychisq1), 3, lower.tail = FALSE)

  c(fef,
    dev1 = dev1,
    dev2 = dev2, 
    chisq_RI = mychisq1, p_RI = pval1,
    thresh = el1$alpha)
}


strip <- function(x) {
  ## Strip away all the unnecessary elements from a CLMM to reduce memory size
  nonessential <- c("L", "condVar", "Zt", "fitted.values", "model",
                    "gfList", "ranef", "u")
  res <- x[setdiff(names(x), nonessential)]
  attr(res[["formula"]], ".Environment") <- NULL
  attr(res[["terms"]], ".Environment") <- NULL
  class(res) <- "clmm"
  res
}

#' Fit CLMM and Run Equivalence Test
#'
#' @inheritParams fit_lmem
#'
#' @importFrom stats C
#'
#' @param delta Smallest (raw) effect size of interest (log odds scale).
#'
#' @details This function is intended to be used in data simulation.
#'
#' @return A vector with p-values; the element(s) named \code{simple}
#'   provide p-values for simple effects; the element(s) named
#'   \code{equiv} provides the p-value for the corresponding
#'   equivalence test.
#' 
#' @examples
#' 
#' set.seed(62)
#' dat <- gen_data(24)
#' \donttest{run_equiv(dat, main_effect = TRUE)}
#' 
#' @export
run_equiv <- function(.data, main_effect = FALSE, delta = .14) {

  .data[["Rep"]] <-
    C(.data[["repetition"]],
      matrix(c(.5, -.5), nrow = 2,
             dimnames = list(c("repeated", "new"))))

  .data[["Int"]] <- 
    C(.data[["interval"]],
      matrix(c(-1/4, -1/4, -1/4,
               3/4, -1/4, -1/4,
               -1/4,  3/4, -1/4,
               -1/4, -1/4,  3/4),
             nrow = 4, byrow = TRUE,
             dimnames = list(c("immediate", "1 day",
                               "1 week", "1 month"),
                             c("I1", "I2", "I3"))))
  
  if (main_effect) {
    mod <- ordinal::clmm(trating ~ Rep * Int +
                           (Rep | subj_id) + (Rep | stim_id),
                         data = .data)
    suppressMessages(main_emm <-
                       emmeans::emmeans(mod, pairwise ~ Rep, data = .data))
    if (!is.null(delta)) {
      res <- 
        c(simple = as.data.frame(main_emm$contrasts)$p.value,
          equiv = emmeans::test(
                             main_emm,
                             delta = delta, side = "equivalence")$contrasts$p.value)
    } else {
      res <- list(mod = strip(mod), data = .data)
    }
  } else {
    ## allse.emmc <- allsimp.emmc # a hack, I admit
    mod <- ordinal::clmm(trating ~ Rep * Int +
                           (R:I1 + R:I2 + R:I3 | subj_id) +
                           (R:I1 + R:I2 + R:I3 | stim_id),
                         data = .data)
    mod_emm <- emmeans::emmeans(mod, allsimp ~ Rep * Int, data = .data)
    ## perform equivalence test using emmeans
    if (!is.null(delta)) {
      res <- c((mod_emm$contrasts %>% as.data.frame())[["p.value"]],
               emmeans::test(mod_emm,
                             delta = delta, side = "equivalence")$contrasts$p.value)
      names(res) <- c(paste0("simple", 1:6), paste0("equiv", 1:6))
    } else {
      res <- list(mod = strip(mod), data = .data)
    }
  }
  res
}


#' Run Equivalence Tests on Existing CLMM Object
#'
#' @param mod Fitted model object, result of call to \code{clmm}.
#' @param .data Data frame containing source data.
#' @param main_effect Whether to perform the test for the main effect (TRUE) or interaction (FALSE).
#' @param delta Delta (SESOI) for the equivalence test, in raw log odds units.
#'
#' @return A vector with p-values from the equivalence test(s);
#'   elements named \code{simple} test simple effects, while elements
#'   named \code{equiv} contain the corresponding equivalence test
#'   results.
#'
#' @examples
#' moddata <- get_model_data()
#' 
#' equivtest(truth_trajectory_models[["main2"]], moddata,
#'           main_effect = TRUE)
#' 
#' @export
equivtest <- function(mod, .data, main_effect = FALSE, delta = .14) {
  if (main_effect) {
    suppressMessages(main_emm <-
                       emmeans::emmeans(mod, pairwise ~ Rep, data = .data))
    c(simple = as.data.frame(main_emm$contrasts)$p.value,
      equiv = emmeans::test(
                         main_emm,
                         delta = delta, side = "equivalence")$contrasts$p.value)
  } else {
    mod_emm <- emmeans::emmeans(mod, allsimp ~ Rep * Int, data = .data)
    ## perform equivalence test using emmeans
    etest <- c((mod_emm$contrasts %>% as.data.frame())[["p.value"]],
               emmeans::test(mod_emm,
                             delta = delta, side = "equivalence")$contrasts$p.value)
    names(etest) <- c(paste0("simple", 1:6), paste0("equiv", 1:6))
    etest    
  }
}

