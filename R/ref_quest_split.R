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
#' @param col_source column containing the source identifier (string or column number)
#' @param k_ref number of reference samples
#' @param k_quest number of questioned samples
#' @inheritSection make_idx_splits Background selection
#' @inheritSection make_idx_splits Source sampling
#' @inheritDotParams make_idx_splits -sources -k_ref -k_quest
#' @export
#' @return a list of indexes (`idx_reference`, `idx_questioned`, `idx_background`) and a list of dataframes (`df_reference`, `df_questioned`, `df_background`)
#' @family set sampling functions
#' @concept set sampling
#' @examples
#' \dontrun{
#' # Sample different species
#' make_dataset_splits(iris, 5, 5, col_source = 'Species')
#'
#' # Sample same species
#' make_dataset_splits(iris, 5, 5, col_source = 'Species', same_source = TRUE)
#'
#' # Sample from custom species
#' make_dataset_splits(iris, 5, 5, col_source = 'Species',
#'    source_ref = 'virginica', source_quest = 'versicolor')
#' make_dataset_splits(iris, 5, 5, col_source = 'Species',
#'    source_ref = 'virginica', source_quest = c('virginica', 'versicolor'))
#'
#' # Sample from reference source with replacement
#' make_dataset_splits(iris, 500, 5, col_source = 'Species', replace = TRUE)
#'
#' # Use background sources from non-sampled items
#' make_dataset_splits(iris, 50, 50, col_source = 'Species',
#'    source_ref = 'virginica', source_quest = 'versicolor', background = 'others')
#' }
make_dataset_splits <- function(df, k_ref, k_quest, col_source = 'source', ...) {
   sources <- purrr::pluck(df, col_source)

   if (is.null(sources)) {
      stop(paste('column "', col_source, '" not found.'))
   }

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
#'
#' Reference/questioned/background samples are always non-intersecting, even when the source is the same.
#'
#' Sampling with replacement is used, if necessary and not forbidden. If it is used, a message appears.
#'
#' It is **always** guaranteed that no sample appear more than once across reference/questioned/background items (but it can appear multiple times in a set).
#'
#' @section Source sampling:
#'
#' If `source_quest` is `NULL`:
#'
#' - if `same_source` is `NULL` or `FALSE`, questioned items are sampled from all but the reference source.
#' - if `same_source` is `TRUE`, questioned items are sampled from the reference source.
#'
#' Else, questioned items will be sampled from the questioned source(s), even if it contains the reference one.
#'
#' Items will never be sampled once (unless `replace` is `TRUE`): they appear once in the reference/questioned/background items.
#'
#' @section Background selection:
#'
#' - If `background` is `'outside'`, the background dataset comprises all items who **do not lie** in any of the reference and questioned sets.
#'   It can contain items from reference and questioned sources.
#' - If `background` is `'others'`, the background dataset comprises all items from the **non**-reference and **non**-questioned **potential sources**.
#' - If `background` is `'unobserved'`, the background dataset comprises all items from the sources who do not appear in any of the he reference and questioned sets.
#'
#' By default, `background` is `'outside'`.
#'
#' Notice that `background = 'others'` generates no background data if questioned sources are not specified:
#' the union of reference and questioned sources fills the available sources in the population.
#'
#' @param sources all class labels
#' @param source_ref the reference source (scalar; if `NULL`, a random source will be picked)
#' @param source_quest the questioned source(s) (if `NULL`, anything but the reference source: behaviour overridden by `same_source`)
#' @param same_source if `source_quest` is `NULL` and `same_source` is `TRUE`, questioned source is the reference source
#' @param k_ref number of reference samples
#' @param k_quest number of questioned samples
#' @param background see details (default: `'outside'`)
#' @param replace use sampling with replacement, else error
#' @return list of indexes (`idx_reference`, `idx_questioned`, `idx_background`)
#' @export
#' @family set sampling functions
#' @concept set sampling
make_idx_splits <- function(sources, k_ref, k_quest,
                            source_ref = NULL, source_quest = NULL,
                            same_source = NULL,
                            background = 'outside', replace = TRUE) {
   sources <- as.vector(sources)
   source_all <- unique(sources)
   idx_all <- seq_along(sources)

   assertthat::assert_that(is.vector(sources))
   assertthat::assert_that(is.null(source_ref) || assertthat::is.scalar(source_ref))
   assertthat::assert_that(is.null(source_quest) || is.vector(source_quest))
   assertthat::assert_that(assertthat::is.string(background))
   assertthat::assert_that(background %in% c('outside', 'unobserved', 'others'), msg = "'background' parameter is invalid")

   # Setup reference source
   if (is.null(source_ref)) {
      source_ref <- sample(source_all, 1)
   }
   assertthat::assert_that(assertthat::is.scalar(source_ref))

   # Setup questioned source(s)
   if (is.null(source_quest)) {

      # Force sampling from the same source
      if (!is.null(same_source) && same_source) {
         source_quest <- source_ref
      } else {
         source_quest <- setdiff(source_all, source_ref)
         if (length(source_quest) == 0) {
            # Only one source in the dataset: must sample both from that one
            source_quest <- source_ref
         }
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
   need_replace_ref <- (len_ref < k_ref)
   if (need_replace_ref) {
      if (replace) { message('Reference items: sampling with replacement is being used.') }
      else { stop('Reference items: not enough items with the reference source, sampling without replacemnt.') }
   }
   idx_reference <- sort(resample(idx_ref_all, k_ref, replace = need_replace_ref))

   # Build the questioned sample w/o reference items

   # Do not sample items in the reference sample
   # All other items are kept, and can be sampled from
   idx_quest_all_no_ref <- setdiff(idx_quest_all, idx_reference)
   need_replace_quest <- (length(idx_quest_all_no_ref) < k_quest)
   if (need_replace_quest) {
      if (replace) { message('Questioned items: sampling with replacement is being used.') }
      else { stop('Questioned items: not enough items with the questioned source(s), sampling without replacemnt.') }
   }
   idx_questioned <- sort(resample(idx_quest_all_no_ref, k_quest, replace = need_replace_quest))

   # Build the background dataset
   if (background == 'others') {
      # Remove reference and questioned potential SOURCES
      idx_background <- setdiff(setdiff(idx_all, idx_ref_all), idx_quest_all)
   } else if (background == 'outside') {
      # Remove reference and questioned ITEMS
      idx_background <- setdiff(setdiff(idx_all, idx_reference), idx_questioned)
   } else if (background == 'unobserved') {
      # Remove reference source from candidates
      idx_background <- setdiff(idx_all, idx_ref_all)
      # Remove OBSEVED questioned sources from candidates
      sources_quest_observed <- unique(sources[idx_questioned])
      idx_quest_observed_all <- which(sources %in% sources_quest_observed)
      idx_background <- setdiff(idx_background, idx_quest_observed_all)

   } else {
      stop('background not specified.')
   }

   if (length(idx_background) == 0) {
      warning('No background data!')
   }

   list(idx_reference = idx_reference, idx_questioned = idx_questioned, idx_background = idx_background)
}



#' Random Samples and Permutations
#'
#' `resample` takes a sample of the specified size from the elements of `x` using either with or without replacement.
#'
#' Works like [base::sample()], but it is safer if `x` is a scalar.
#'
#' @param x a vector of one or more elements from which to choose
#' @param size a non-negative integer giving the number of items to choose.
#' @param replace should sampling be with replacement?
resample <- function(x, size, replace = FALSE) {
   x[sample.int(length(x), size = size, replace = replace)]
}
