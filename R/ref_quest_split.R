#' Generate reference and questioned observations from
#'
#' @param df all available data
#' @param k_ref number of reference samples
#' @param k_quest number of questioned samples
#' @param col_source column containing the source identifier
#' @inheritParams make_idx_splits
#' @export
#' @return a list of indexes and a list of dataframes
make_dataset_splits <- function(df, k_ref, k_quest, col_source = 'source', ...) {
   sources <- df %>% pluck(col_source)

   list_idx <- make_idx_splits(sources, n_ref = k_ref, n_quest = k_quest, ...)

   c(list_idx, list(
      df_reference = df[list_idx$idx_reference,],
      df_questioned = df[list_idx$idx_questioned,],
      df_background = df[list_idx$idx_background,]
   ))
}




#' Extract questioned/reference/background samples from a list of populations from sources
#'
#' The function splits a list of items (rows) into a sample of reference items, questioned items and background items.
#' Reference and questioned samples are always non-intersecting, even when the source is the same.
#'
#' If `background` is `outside`, the background dataset comprises all items who do not lie in any of the reference and questioned sets.
#' If `background` is `others`, the background dataset comprises all items from the non-reference and non-questioned sources
#'
#' Sampling with replacement is used, if necessary.
#'
#' @param sources all class labels
#' @param source_ref the reference source (scalar; if NULL, a random source will be picked)
#' @param source_quest the questioned source(s) (if NULL, anything but the reference source)
#' @param n_ref number of reference samples
#' @param n_quest number of questioned samples
#' @param background see details
#' @param replace use sampling with replacement, else error
#' @importFrom assertthat assert_that
#' @return list of indexes
#' @export
make_idx_splits <- function(sources, n_ref, n_quest, source_ref = NULL, source_quest = NULL, background = 'outside', use_replace = TRUE) {
   sources <- as.vector(sources)
   source_all <- unique(sources)
   idx_all <- seq_along(sources)

   if (is.null(source_ref)) {
      source_ref <- sample(source_all, 1)
   }
   assert_that(assertthat::is.scalar(source_ref))

   if (is.null(source_quest)) {
      source_quest <- setdiff(source_all, source_ref)
      if (length(source_quest) == 0) {
         # Only one source in the dataset: must sample both from that one
         source_quest <- source_ref
      }
   } else {
      source_quest <- as.vector(source_quest)
   }

   if (identical(unique(source_ref), unique(source_quest))) {
      sampling_same <- TRUE
   } else {
      sampling_same <- FALSE
   }

   idx_ref_all <- which(sources %in% source_ref)
   len_ref <- length(idx_ref_all)
   if (len_ref == 0) {
      stop('Reference class not found.')
   }

   idx_quest_all <- which(sources %in% source_quest)
   len_quest_all <- length(idx_quest_all)
   if (len_quest_all == 0) {
      stop('Questioned class not found.')
   }

   # Build the reference sample
   do_replace_ref <- (len_ref < n_ref) & use_replace
   idx_reference <- idx_ref_all %>% sample(n_ref, replace = do_replace_ref) %>% sort
   if (do_replace_ref) {
      if (use_replace) { message('Reference items: sampling with replacement is being used.') }
      else { error('Reference items: sampling with replacement is being used.') }
   }

   # Build the questioned sample w/o reference items
   idx_quest_all_no_ref <- setdiff(idx_quest_all, idx_reference)
   do_replace_quest <- (length(idx_quest_all_no_ref) < n_quest) & use_replace
   idx_questioned <- sample(idx_quest_all_no_ref, n_quest, replace = do_replace_quest) %>% sort
   if (do_replace_quest) {
      if (use_replace) { message('Questioned items: sampling with replacement is being used.') }
      else { error('Questioned items: sampling with replacement is being used.') }
   }

   # Build the background dataset
   if (background == 'others') {
      idx_background <- setdiff(setdiff(idx_all, idx_ref_all), idx_quest_all)
   } else if (background == 'outside') {
      idx_background <- setdiff(setdiff(idx_all, idx_reference), idx_questioned)
   } else {
      stop('background not specified.')
   }

   list(idx_reference = idx_reference, idx_questioned = idx_questioned, idx_background = idx_background)
}