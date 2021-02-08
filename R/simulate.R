#' @importFrom stats qnorm
#' @importFrom stats rnorm
NULL

#' Derive Fixed-Effects Parameters from Phase-by-Phase Effects
#'
#' @param phase_eff A four-element vector specifying the illusory
#'   truth effect at each of the four testing phases (in log odds
#'   units).
#'
#' @return Vector with eight elements containing the fixed effects
#'   coefficients for deviation-coded predictors.
#' 
#' @export
derive_fixed <- function(phase_eff) {
  if (length(phase_eff) != 4L)
    stop("'phase_eff' must be a vector of length 4.")

  a <- phase_eff[1]
  b <- phase_eff[2]
  c <- phase_eff[3]
  d <- phase_eff[4]

  c(`(Intercept)` = (a + b + c + d) / 8,
    R             = (a + b + c + d) / 4,
    I1            = (b - a) / 2,
    I2            = (c - a) / 2,
    I3            = (d - a) / 2,
    `R:I1`        = (b - a),
    `R:I2`        = (c - a),
    `R:I3`        = (d - a))
}

#' Simulate Truth Rating Data
#'
#' @param nsubj Number of subjects. Because of counterbalancing, must
#'   be a multiple of 8.
#' 
#' @param phase_eff A four-element vector giving the size of the
#'   illusory truth effect at each of the four phases (on the log odds
#'   scale). Use \code{rep(0, 4)} for testing Type I error rate. A
#'   value of .14 gives an effect of approximately 1/10 of a scale
#'   point.
#'
#' @param thresh Cut-points (thresholds) for the seven point scale
#'   (must be a six-element vector).
#' 
#' @param subj_rfx A 4x4 covariance matrix with by-subject variance
#'   components for the intercept, main effect of repetition, main
#'   effect of interval, and repetition-by-interval interaction. Only
#'   the variances (elements on the diagonal) are used in the
#'   simulation (see Details).
#' @param item_rfx A 4x4 covariance matrix with by-statement variance
#'   components for the intercept, main effect of repetition, main
#'   effect of interval, and repetition-by-interval interaction. Only
#'   the variances (elements on the diagonal) are used in the
#'   simulation (see Details).
#' @param dropout A vector encoding assumptions about the proportion
#'   of subjects dropping out of the study over the four testing
#'   intervals (immediate, 1 day, 1 week, 1 month). The first element
#'   represents the proportion of subjects who completed the first
#'   phase (immediate) but who drop out before the next interval one
#'   day later. The second element represents the proportion of the
#'   remaining participants dropping out after 1 day and before 1
#'   week. The third and final element represents the proportion of
#'   remaining participants dropping out after 1 week and before 1
#'   month. For example, the default values of \code{c(.05, .1, .1)}
#'   encode dropout rates of 5\%, 10\%, and 10\%.
#' 
#' @details By default, the thresholds and parameter estimates for
#'   variance components used in the simulation are from the
#'   cumulative link mixed model fit to the Nadarevic and Erdfelder
#'   data. Only the variances from the by-subject and by-item
#'   covariance matrices are used. Unlike Nadarevic and Erdfelder, who
#'   only had two testing intervals, the simulated study assumes four
#'   intervals, coded by three predictors for the main effect and
#'   three for the interaction with repetition. The code below depicts
#'   how the four-element variance vector from the original study is
#'   translated into the eight variances needed for the simulated
#'   data.
#'
#' \code{newvar_subj <- rep(diag(subj_rfx), c(1, 1, 3, 3))}
#'
#' \code{newvar_item <- rep(diag(item_rfx), c(1, 1, 3, 3))}
#'
#' The simulated data includes ratings for 128 stimulus items for each
#'   subject. Half of the statements are repeated (old) and half are
#'   new.  A quarter of the items (32) are tested at each phase.
#'
#' It is assumed that the key effect present in the data is the
#' interaction term, which is designed to represent an illusory-truth
#' effect that first appears at the second testing interval (1 day)
#' and remains over the subsequent two intervals without changing
#' size. All other fixed effects in the model (main effect of R and
#' three effects encoding the main effect of interval) are driven by
#' the interaction term.
#' 
#' @return A data frame, with \code{nsubj * 128} rows and 11 variables, where:
#' 
#' \describe{
#'
#' \item{\code{subj_id}}{Unique subject identifier.}
#'
#' \item{\code{list_id}}{Which set of statements the subject received.}
#'
#' \item{\code{stim_id}}{Unique stimulus (statement) identifier.}
#'
#' \item{\code{repetition}}{Whether the statement was old or new.}
#'
#' \item{\code{interval}}{Testing interval (immediate, 1 day, 1 week, 1 month).}
#'
#' \item{\code{eta}}{The simulated response tendency, on the log odds scale.}
#'
#' \item{\code{trating}}{The simulated rating value.}
#'
#' \item{\code{R}}{Deviation-coded predictor for repetition (old = 1/2,
#'   new = -1/2).}
#'
#' \item{\code{I1}}{Deviation-coded predictor for interval comparing baseline
#'   (immediate) to 1 day.}
#'
#' \item{\code{I2}}{Deviation-coded predictor for interval comparing
#'   baseline (immediate) to 1 week.}
#'
#' \item{\code{I3}}{Deviation-coded predictor for interval comparing
#' baseline (immediate) to 1 month.}
#'
#' }
#'
#' @examples
#' # demonstrate how to convert from four variances to eight
#' four_var <- diag(ordinal::VarCorr(clmm_maximal)$subj_id)
#' four_var
#' rep(four_var, c(1, 1, 3, 3))
#'
#' # basic usage
#' dat <- gen_data(256)
#' 
#' # demonstrate deviation coding
#' dat %>% dplyr::distinct(repetition, interval, R, I1, I2, I3)
#'
#' # demonstrate dropouts
#' dat %>% dplyr::distinct(subj_id, interval) %>% dplyr::count(interval)
#'
#' @seealso \code{\link{clmm_maximal}}, \code{\link{NE_exp1}}
#' @export
gen_data <- function(nsubj,
                     phase_eff = rep(0, 4),
                     thresh = alpha_6_to_7(truthiness::clmm_maximal$alpha),
                     subj_rfx =
                       ordinal::VarCorr(truthiness::clmm_maximal)$subj_id,
                     item_rfx =
                       ordinal::VarCorr(truthiness::clmm_maximal)$item_id,
                     dropout = c(.05, .1, .1)) {
  list_id <- n <- repetition <- interval <- NULL
  `(Intercept).s` <- `(Intercept).i` <- R.s <- R.i <- R <- NULL
  I1.s <- I1.i <- I1 <- I2.s <- I2.i <- I2 <- I3.s <- I3.i <- I3 <- NULL
  `R:I1.s` <- `R:I2.s` <- `R:I3.s` <- `R:I1.i` <- `R:I2.i` <- `R:I3.i` <- NULL
  eta <- subj_id <- stim_id <- trating <- NULL
  
  if (nsubj %% 8)
    stop("'nsubj' must be multiple of 8 (the number of stimulus lists)")

  if (length(thresh) != 6L)
    stop("'thresh' must be a six-element vector")

  nitem <- truthiness::stimulus_conditions %>%
    dplyr::count(list_id) %>% dplyr::pull(n) %>% unique()
  stopifnot(length(nitem) == 1L)

  betas <- derive_fixed(phase_eff)

  ## variance-covariance matrices
  ## covariances set to zero
  subj_mx <- matrix(0, nrow = 8, ncol = 8,
                    dimnames = list(names(betas), names(betas)))
  diag(subj_mx) <- rep(diag(subj_rfx), c(1, 1, 3, 3))

  item_mx <- matrix(0, nrow = 8, ncol = 8,
                    dimnames = list(names(betas), names(betas)))
  diag(item_mx) <- rep(diag(item_rfx), c(1, 1, 3, 3))

  ## generate random effects for subjects
  mus <- rep(0, length(names(betas)))
  names(mus) <- names(betas)
  sfx <- MASS::mvrnorm(nsubj, mus, subj_mx) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(subj_id = factor(dplyr::row_number()))

  ## generate random effects for items
  ifx <- MASS::mvrnorm(nitem, mus, item_mx) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(stim_id = factor(dplyr::row_number()))

  subj <- tibble::tibble(subj_id = factor(seq_len(nsubj)),
                         list_id = factor(rep(1:8, nsubj / 8L)))

  trials <- dplyr::inner_join(subj, truthiness::stimulus_conditions,
                              "list_id") %>%
    dplyr::inner_join(sfx, "subj_id") %>%
    dplyr::inner_join(ifx, "stim_id", suffix = c(".s", ".i")) %>%
    dplyr::mutate(
             R = dplyr::if_else(repetition == "repeated", 1/2, -1/2),
             I1 = dplyr::if_else(interval == "1 day", 3/4, -1/4),
             I2 = dplyr::if_else(interval == "1 week", 3/4, -1/4),
             I3 = dplyr::if_else(interval == "1 month", 3/4, -1/4),
             eta =
               `(Intercept).s` + `(Intercept).i` +
               (R.s + R.i + betas["R"]) * R +
               (I1.s + I1.i + betas["I1"]) * I1 +
               (I2.s + I2.i + betas["I2"]) * I2 +
               (I3.s + I3.i + betas["I3"]) * I3 +
               (`R:I1.s` + `R:I1.i` + betas["R:I1"]) * R * I1 +
               (`R:I2.s` + `R:I2.i` + betas["R:I2"]) * R * I2 +
               (`R:I3.s` + `R:I3.i` + betas["R:I3"]) * R * I3,
             trating = factor(eta2resp(eta, thresh + betas["(Intercept)"]),
                              levels = 1:7,
                              ordered = TRUE)) %>%
    dplyr::select(subj_id, list_id, stim_id, repetition, interval, eta,
                  trating, R, I1, I2, I3)

  ## now drop participants
  ## nremaining * dropout rate
  ndrop <- ceiling(dropout * nsubj)
  ndrop[2] <- ceiling((nsubj - ndrop[1]) * dropout[2])
  ndrop[3] <- ceiling((nsubj - ndrop[1] - ndrop[2]) * dropout[3])

  all_sphase <- dplyr::distinct(trials, subj_id, interval)

  remaining <- dplyr::distinct(trials, subj_id) %>% dplyr::pull() %>%
    as.integer()
  lvls <- trials %>% dplyr::pull(interval) %>% levels()
  res <- list()
  for (.i in seq_along(ndrop)) {
    dropped <- sample(remaining, ndrop[.i])
    res[[.i]] <- tidyr::crossing(subj_id = dropped,
                                 interval = lvls[(.i + 1L):length(lvls)])
    remaining <- dplyr::setdiff(remaining, dropped)
  }

  stopifnot((dplyr::intersect(
                      dplyr::intersect(dplyr::distinct(res[[1]], subj_id),
                                       dplyr::distinct(res[[2]], subj_id)),
                      dplyr::distinct(res[[3]], subj_id)) %>% nrow()) == 0L)

  discard <- dplyr::bind_rows(res[[1]], res[[2]], res[[3]]) %>%
    dplyr::mutate(subj_id = factor(subj_id,
                                   levels = levels(trials[["subj_id"]])),
                  interval = factor(interval, levels = lvls))
  stopifnot(nrow(dplyr::distinct(discard, subj_id)) == sum(ndrop))

  dplyr::anti_join(trials, discard, c("subj_id", "interval"))
}
