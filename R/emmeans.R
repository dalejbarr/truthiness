#' Custom Contrast Function for emmeans
#'
#' Create a contrast matrix for equivalence test of a 2x4
#' interaction.
#'
#' @param levels Interaction levels (should be of length 8).
#'
#' @param ... Any other arguments (NB: currently ignored).
#'
#' @details  Runs all six ways of comparing the simple effects of
#' the two-level factor. For use with the emmeans package.
#'
#' @export
allsimp.emmc <- function(levels, ...) {
  if (length(levels) != 8L) stop("'levels' must have 8 levels")
  M <- data.frame("eff @ 1 day vs eff @ immediate" = c(1,-1,-1,1,0,0,0,0),
                  "eff @ 1 week vs eff @ immediate" = c(1, -1, 0, 0, -1, 1, 0, 0),
                  "eff @ 1 month vs eff @ immediate" = c(1, -1, 0, 0, 0, 0, -1, 1),
                  "eff @ 1 week vs eff @ 1 day" = c(0, 0, 1, -1, -1, 1, 0, 0),
                  "eff @ 1 month vs eff @ 1 day" = c(0, 0, 1, -1, 0, 0, -1, 1),
                  "eff @ 1 week vs eff @ 1 month" = c(0, 0, 0, 0, 1, -1, -1, 1),
                  check.names = FALSE)
  rownames(M) <- levels
  attr(M, "desc") <- "Simple effects comparison"
  M
}
