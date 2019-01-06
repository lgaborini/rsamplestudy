#' Generate reference/questioned/background observations from a multiple-source dataset.
#'
#' The function splits a dataframe by rows, into a sample of reference items, questioned items and background items.
#' The split is done according by the item source.
#'
#' Reference and questioned samples are always non-intersecting, even when the source is the same.
#'
#' Sampling with replacement is used, if necessary and not forbidden.
#'
#' @param df all available data
#' @param k_ref number of reference samples
#' @param k_quest number of questioned samples
#' @param col_source column containing the source identifier (string or column number)
#' @param source_ref the reference source (scalar; if \code{NULL}, a random source will be picked)
#' @param source_quest the questioned source(s) (if \code{NULL}, anything but the reference source)
#' @param background see details (default: \code{outside})
#' @param replace use sampling with replacement, else error
#' @inheritSection make_idx_splits Background selection
#' @inheritSection make_idx_splits Source sampling
#' @inheritDotParams make_idx_splits
#' @importFrom purrr pluck
#' @export
#' @return a list of indexes (\code{idx_reference}, \code{idx_questioned}, \code{idx_background}) and a list of dataframes (\code{df_reference}, \code{df_questioned}, \code{df_background})
#' @seealso \link{make_idx_splits}
make_dataset_splits <- function(df, k_ref, k_quest, col_source = 'source', ...) {
   sources <- purrr::pluck(df, col_source)

   list_idx <- make_idx_splits(sources, k_ref = k_ref, k_quest = k_quest, ...)

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
#' Sampling with replacement is used, if necessary and not forbidden.
#'
#' @section Source sampling:
#'
#' If `source_quest` is NULL, questioned items are sampled from all but the reference source.
#' Else, questioned items will be sampled from the questioned source(s), even if it contains the reference one.
#' 
#' Items will never be sampled once (unless `replace` is TRUE): they appear once in the reference/questioned/background items.
#'
#' @section Background selection:
#'
#' If \code{background} is \code{outside}, the background dataset comprises all items who do not lie in any of the reference and questioned sets.
#' If \code{background} is \code{others}, the background dataset comprises all items from the non-reference and non-questioned sources
#'
#' By default, \code{background} is \code{outside}: background data can contain items from \emph{all} sources.
#'
#' @param sources all class labels
#' @param source_ref the reference source (scalar; if \code{NULL}, a random source will be picked)
#' @param source_quest the questioned source(s) (if \code{NULL}, anything but the reference source)
#' @param k_ref number of reference samples
#' @param k_quest number of questioned samples
#' @param background see details (default: \code{outside})
#' @param replace use sampling with replacement, else error
#' @importFrom assertthat assert_that
#' @return list of indexes (\code{idx_reference}, \code{idx_questioned}, \code{idx_background})
#' @export
#' @seealso \link{make_dataset_splits}
make_idx_splits <- function(sources, k_ref, k_quest,
                            source_ref = NULL, source_quest = NULL,
                            same_source = NULL,
                            background = 'outside', use_replace = TRUE) {
   sources <- as.vector(sources)
   source_all <- unique(sources)
   idx_all <- seq_along(sources)

   if (is.null(source_ref)) {
      source_ref <- sample(source_all, 1)
   }
   assertthat::assert_that(assertthat::is.scalar(source_ref))

   if (is.null(source_quest)) {
      source_quest <- setdiff(source_all, source_ref)
      if (length(source_quest) == 0) {
         # Only one source in the dataset: must sample both from that one
         source_quest <- source_ref
      }
   } else {
      source_quest <- as.vector(source_quest)
   }

   # Check whether the questioned and reference sources are the same
   if (identical(unique(source_ref), unique(source_quest))) {
      sampling_same <- TRUE
   } else {
      sampling_same <- FALSE
   }

   # Indexes of items from the reference source
   idx_ref_all <- which(sources %in% source_ref)
   len_ref <- length(idx_ref_all)
   if (len_ref == 0) {
      stop('Reference class not found.')
   }

   # Indexes of items from the questioned sources
   idx_quest_all <- which(sources %in% source_quest)
   len_quest_all <- length(idx_quest_all)
   if (len_quest_all == 0) {
      stop('Questioned class not found.')
   }

   # Build the reference sample
   do_replace_ref <- (len_ref < k_ref) & use_replace
   idx_reference <- sort(resample(idx_ref_all, k_ref, replace = do_replace_ref))
   if (do_replace_ref) {
      if (use_replace) { message('Reference items: sampling with replacement is being used.') }
      else { stop('Reference items: sampling with replacement is being used.') }
   }

   # Build the questioned sample w/o reference items

   # Do not sample items in the reference sample
   # All other items are kept, and can be sampled from
   idx_quest_all_no_ref <- setdiff(idx_quest_all, idx_reference)
   do_replace_quest <- (length(idx_quest_all_no_ref) < k_quest) & use_replace
   idx_questioned <- sort(resample(idx_quest_all_no_ref, k_quest, replace = do_replace_quest))
   if (do_replace_quest) {
      if (use_replace) { message('Questioned items: sampling with replacement is being used.') }
      else { stop('Questioned items: sampling with replacement is being used.') }
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



#' Random Samples and Permutations
#'
#' `resample`` takes a sample of the specified size from the elements of x using either with or without replacement.
#'
#' Works like `sample`, but it is safer if x is a scalar.
#'
#' @param x a vector of one or more elements from which to choose
#' @param size a non-negative integer giving the number of items to choose.
#' @export
#' @inheritParams sample
resample <- function(x, size, replace = FALSE) {
   x[sample.int(length(x), size = size, replace = replace)]
}
