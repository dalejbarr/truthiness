#' Create simulated response data files.
#'
#' Create response files in Prolific format.
#'
#' @param path Output directory for the files. Must not already exist.
#'
#' @param nsubj Number of subjects to simulate.
#'
#' @export
simulate_response_files <- function(path, nsubj) {
  dat <- gen_data(nsubj)
  tpres <-
    presentation_lists[presentation_lists[["task"]] == "truth",
                       c("phase_id", "list_id", "stim_id",
                         "task_id", "order")]
  
  df1 <- dplyr::inner_join(dat, tpres, c("list_id", "stim_id"))

             

}
