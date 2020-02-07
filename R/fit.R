#' @importFrom stats deviance
#' @importFrom stats logLik
#' @importFrom stats pchisq
NULL

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

#' Fit generalized additive mixed model to ratings data
#'
#' @param .data Data frame, with the format as resulting from a call
#'   to \code{\link{gen_data}}.
#'
#' @param main_effect Whether to test the main effect of repetition
#'   (TRUE) or the repetition-by-interval interaction (FALSE; the
#'   default).
#'
#' @details Fits a generalized additive mixed model to the data and
#'   tests the specified effect (interaction or main effect) using a
#'   likelihood-ratio test using \code{mgcv::bam()}. If the
#'   interaction is to be tested, the following two models are
#'   compared.
#'
#' \code{trating ~ R * (I1 + I2 + I3) +
#'    s(subj_id, bs = "re") +
#'    s(subj_id, R, I1, bs = "re") +
#'    s(subj_id, R, I2, bs = "re") +
#'    s(subj_id, R, I3, bs = "re") +
#'    s(stim_id, bs = "re") +
#'    s(stim_id, R, I1, bs = "re") +
#'    s(stim_id, R, I2, bs = "re") +
#'    s(stim_id, R, I3, bs = "re")}
#'
#' \code{trating ~ R + I1 + I2 + I3 +
#'    s(subj_id, bs = "re") +
#'    s(subj_id, R, I1, bs = "re") +
#'    s(subj_id, R, I2, bs = "re") +
#'    s(subj_id, R, I3, bs = "re") +
#'    s(stim_id, bs = "re") +
#'    s(stim_id, R, I1, bs = "re") +
#'    s(stim_id, R, I2, bs = "re") +
#'    s(stim_id, R, I3, bs = "re")}
#' 
#' If the main effect is to be tested, then the following two models
#' are compared.
#'
#' \code{trating ~ R * (I1 + I2 + I3) +
#'    s(subj_id, bs = "re") +
#'    s(subj_id, R, bs = "re") +
#'    s(stim_id, bs = "re") +
#'    s(stim_id, R, bs = "re")}
#'
#' \code{trating ~ I1 + I2 + I3 + R:I1 + R:I2 + R:I3 +
#'    s(subj_id, bs = "re") +
#'    s(subj_id, R, bs = "re") +
#'    s(stim_id, bs = "re") +
#'    s(stim_id, R, bs = "re")}
#' 
#' @return A vector, with the following elements.
#'
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
#' @seealso \code{\link{gen_data}}
#' @examples
#' set.seed(62)
#' dat <- gen_data(40) 
#' fit_gamm(dat, TRUE) # test main effect
#' 
#' @export
fit_gamm <- function(.data, main_effect = FALSE) {
  form <- form2 <- NULL

  .data[["trating"]] <- as.integer(.data[["trating"]])

  if (main_effect) {
    form <- trating ~ R * (I1 + I2 + I3) +
      s(subj_id, bs = "re") +
      s(subj_id, R, bs = "re") +
      s(stim_id, bs = "re") +
      s(stim_id, R, bs = "re")

    form2 <- trating ~ I1 + I2 + I3 + R:I1 + R:I2 + R:I3 +
      s(subj_id, bs = "re") +
      s(subj_id, R, bs = "re") +
      s(stim_id, bs = "re") +
      s(stim_id, R, bs = "re")    
  } else {
    form <- trating ~ R * (I1 + I2 + I3) +
      s(subj_id, bs = "re") +
      s(subj_id, R, I1, bs = "re") +
      s(subj_id, R, I2, bs = "re") +
      s(subj_id, R, I3, bs = "re") +
      s(stim_id, bs = "re") +
      s(stim_id, R, I1, bs = "re") +
      s(stim_id, R, I2, bs = "re") +
      s(stim_id, R, I3, bs = "re")

    form2 <- trating ~ R + I1 + I2 + I3 +
      s(subj_id, bs = "re") +
      s(subj_id, R, I1, bs = "re") +
      s(subj_id, R, I2, bs = "re") +
      s(subj_id, R, I3, bs = "re") +
      s(stim_id, bs = "re") +
      s(stim_id, R, I1, bs = "re") +
      s(stim_id, R, I2, bs = "re") +
      s(stim_id, R, I3, bs = "re")
  }

  ## fit the model and print the results
  el1 <- suppressWarnings(mgcv::bam(form, family = mgcv::ocat(R = 7), .data))
  el2 <- suppressWarnings(mgcv::bam(form2, family = mgcv::ocat(R = 7), .data))

  fef <- coefficients(el1)[1:8]

  mychisq1 <- deviance(el2) - deviance(el1)
  pval1 <- pchisq(abs(mychisq1), 3, lower.tail = FALSE)

  thetas <- el1$family$getTheta(TRUE)
  names(thetas) <- paste0("thresh.", 1:6, "|", 2:7)
  
  c(fef, dev1 = deviance(el1), dev2 = deviance(el2), 
    chisq_RI = mychisq1, p_RI = pval1,
    thetas)
}

#' Fit linear mixed-effects model to ratings data
#'
#' @param .data Data frame, with the format as resulting from a call
#'   to \code{\link{gen_data}}.
#' 
#' @param main_effect Whether to test the main effect of repetition
#'   (TRUE) or the repetition-by-interval interaction (FALSE; the
#'   default).
#'
#' @details Fits a linear-mixed effects model to the data and tests
#'   the specified effect (interaction or main effect) using a
#'   likelihood-ratio test (with lme4 \code{REML = FALSE}).  using
#'   \code{lme4::lmer()}. If the interaction is to be tested, the
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
#' @seealso \code{\link{gen_data}}
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

#' Fit cumulative logit mixed-effects model to ratings data
#'
#' @inheritParams fit_lmem
#' 
#' @details Fits a cumulative logit mixed-effects model to the data
#'   and tests the specified effect (interaction or main effect) using
#'   a likelihood-ratio test using \code{ordinal::clmm()}.  If the
#'   interaction is to be tested, the following two models are
#'   compared.
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
#' @seealso \code{\link{gen_data}}
#' @examples
#' \dontrun{
#'   set.seed(62)
#'   dat <- gen_data(24) # test main effect
#'   fit_clmm(dat, TRUE) # takes a few minutes
#' }
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

#' Run equivalence test
#'
#' @param .data Data frame.
#'
#' @param main_effect Whether to run the test for the main effect
#'   (TRUE) or the interaction (FALSE).
#'
#' @param delta Smallest (raw) effect size of interest (log odds scale).
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
    res <- 
      c(simple = as.data.frame(main_emm$contrasts)$p.value,
        equiv = emmeans::test(
                           main_emm,
                           delta = delta, side = "equivalence")$contrasts$p.value)
  } else {
    mod <- ordinal::clmm(trating ~ Rep * Int +
                           (R:I1 + R:I2 + R:I3 | subj_id) +
                           (R:I1 + R:I2 + R:I3 | stim_id),
                         data = .data)
    mod_emm <- emmeans::emmeans(mod, allsimp ~ Rep * Int, data = .data)
    ## perform equivalence test using emmeans
    res <- c((mod_emm$contrasts %>% as.data.frame())[["p.value"]],
             emmeans::test(mod_emm,
                           delta = delta, side = "equivalence")$contrasts$p.value)
    names(res) <- c(paste0("simple", 1:6), paste0("equiv", 1:6))
  }
  res
}
