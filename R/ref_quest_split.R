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
      stop_serious(paste('column "', col_source, '" not found.'))
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
#' It is **always** guaranteed that no sample appear more than once across reference/questioned/background items (but it can appear multiple times in a set if `replace` is `TRUE`).
#'
#' @section Source sampling:
#'
#' Sampling happens in steps:
#' 1. a reference source is picked
#' 2. questioned sources are picked
#' 3. items from reference sources are picked
#' 4. items from questioned sources are picked
#' 5. remaining items form the background set: apply background restrictions
#'
#' By design, the package identifies sources which are allowed to be sampled from.
#' By default, all available sources can appear in the reference or questioned samples.
#'
#' This restriction can be modified using the parameters `source_ref_allowed` and `source_quest_allowed`.
#' It is also forbidden to specify a `source_ref` or a `source_quest` which are not allowed.
#'
#' The behaviour of `same_source`, when specified, is stronger, and `source_quest_allowed` is overridden.
#'
#' If `source_quest` is `NULL`:
#'
#' - if `same_source` is `NULL` or `FALSE`, questioned items are sampled from all but the reference source (or `source_quest_allowed`).
#' - if `same_source` is `TRUE`, questioned items are sampled from the reference source (`source_ref_allowed` is ignored).
#'
#' If `source_quest` is not `NULL`:
#'
#' - `same_source` always has priority: if specified, `source_quest` will be ignored, the chosen reference source will be picked.
#' - if `source_quest` conflicts with `same_source`, **an error is raised**.
#'
#' Else, questioned items will be sampled from the questioned source(s), even if it contains the reference one.
#'
#' Items will never be sampled once (unless `replace` is `TRUE`): they appear once in the reference/questioned/background items.
#'
#'
#' @section Background selection:
#'
#' - If `background` is `'outside'`, the background dataset comprises all items who **do not lie** in any of the reference and questioned sets.
#'   It can contain items from reference and questioned sources.
#' - If `background` is `'others'`, the background dataset comprises all items from the **non**-reference and **non**-questioned **potential sources**.
#' - If `background` is `'unobserved'`, the background dataset comprises all items from the sources who do not appear in any of the reference and questioned sets.
#'
#' By default, `background` is `'outside'`.
#'
#' Notice that `background = 'others'` generates no background data if questioned sources are not specified:
#' the union of reference and questioned sources fills the available sources in the population.
#'
#' @param sources all class labels
#' @param source_ref the reference source (scalar; if `NULL`, a random source will be picked)
#' @param source_quest the questioned source(s) (if `NULL`, anything but the reference source: behaviour overridden by `same_source`)
#' @param same_source if `source_quest` is `NULL` and `same_source` is `TRUE`, questioned source is the reference source; see details for conflict resolution
#' @param source_ref_allowed allowed reference sources (if not `NULL` (default), reference source will be picked among these)
#' @param source_quest_allowed allowed  questioned sources (if not `NULL` (default), questioned source(s) will be picked among these)
#' @param k_ref number of reference samples
#' @param k_quest number of questioned samples
#' @param background see details (default: `'outside'`)
#' @param replace use sampling with replacement, else error
#' @param strict fail at any incoherence between parameters instead of giving warnings or assuming (default: `FALSE`)
#' @return list of indexes (`idx_reference`, `idx_questioned`, `idx_background`)
#' @export
#' @family set sampling functions
#' @concept set sampling
make_idx_splits <- function(sources, k_ref, k_quest,
                            source_ref = NULL, source_quest = NULL,
                            source_ref_allowed = NULL, source_quest_allowed = NULL,
                            same_source = NULL,
                            background = 'outside',
                            replace = TRUE,
                            strict = FALSE) {

   sources <- as.vector(sources)
   source_all <- unique(sources)
   idx_all <- seq_along(sources)


   # Activate strict mode
   if (strict) {
      warning <- function(x,...) { stop(paste('[STRICT, was warning]', x), ...) }
      message <- function(x,...) { stop(paste('[STRICT, was message]', x), ...) }
      stop_serious <- function(x,...) { stop(paste('[STRICT, was error/serious]', x), ...) }
   }


   ## Parameter validation -------------------------

   assertthat::assert_that(is.vector(sources))
   assertthat::assert_that(is.null(source_ref) || assertthat::is.scalar(source_ref))
   assertthat::assert_that(is.null(source_quest) || is.vector(source_quest))
   assertthat::assert_that(assertthat::is.string(background))
   assertthat::assert_that(background %in% c('outside', 'unobserved', 'others'), msg = "'background' parameter is invalid")

   assertthat::assert_that(is.null(source_ref_allowed) ||
      (is.vector(source_ref_allowed) && all(source_ref_allowed %in% source_all)),
      msg = '"source_ref_allowed" is not a vector, or contains extraneous sources.')

   assertthat::assert_that(is.null(source_quest_allowed) ||
      (is.vector(source_quest_allowed) && all(source_quest_allowed %in% source_all)),
      msg = '"source_quest_allowed" is not a vector, or contains extraneous sources.')

   # Forbid forcing a reference source which is not allowed
   if (!is.null(source_ref_allowed) && !is.null(source_ref)) {
      assertthat::assert_that(all(source_ref %in% source_ref_allowed),
                              msg = '"source_ref" is not in the allowed set.')
   }

   # Forbid forcing a questioned source which is not allowed
   if (!is.null(source_quest_allowed) && !is.null(source_quest)) {
      assertthat::assert_that(all(source_quest %in% source_quest_allowed),
                              msg = '"source_quest" is not in the allowed set.')
   }

   # Optional arguments
   source_ref_in_arguments <- !is.null(source_ref)
   source_quest_in_arguments <- !is.null(source_quest)
   same_source_in_arguments <- !is.null(same_source)

   # Forbid forcing a reference source which does not exist
   if (source_ref_in_arguments) {
      assertthat::assert_that(source_ref %in% source_all,
                              msg = '"source_ref" is not in available sources.')
   }

   # Forbid forcing a questioned source which does not exist
   if (source_quest_in_arguments) {
      assertthat::assert_that(all(source_quest %in% source_all),
                              msg = '"source_quest" is not in available sources.')
   }


   ## Setup sources -------------------------

   # Setup reference source
   if (!source_ref_in_arguments) {
      # source_ref not specified

      # Honor allowed reference sources
      if (is.null(source_ref_allowed)) {
         source_ref_allowed <- source_all
      }
      # Sample source_ref from allowed reference sources
      source_ref <- sample_safe(source_ref_allowed, 1)
   }
   assertthat::assert_that(assertthat::is.scalar(source_ref), msg = '"source_ref" must be a single source.')

   # Reference source is here:
   # source_ref


   # Setup questioned source(s)

   if (!source_quest_in_arguments) {
      # source_quest is NULL

      # Force sampling from the same source if same_source is TRUE
      if (same_source_in_arguments && same_source) {

         source_quest <- source_ref

      } else {

         # same_source is FALSE or not specified (default: FALSE)
         # source_quest is NULL
         # -> honoring same_source

         if (is.null(source_quest_allowed)) {
            source_quest_allowed <- setdiff(source_all, source_ref)
         } else {
            # Avoid sampling from the reference source
            source_quest_allowed <- setdiff(source_quest_allowed, source_ref)
         }

         source_quest <- source_quest_allowed

         if (length(source_quest) == 0) {
            # Only one source in the dataset: must sample both from that one
            source_quest <- source_ref
         }
      }
   } else {

      # source_quest is not NULL

      # same_source always has priority over explicit sources
      if (same_source_in_arguments) {
         # same_source specified

         if (same_source) {
            # same_source = TRUE: honoring it

            # source_ref specified?
            if (source_ref_in_arguments) {

               # source_ref has been specified
               # source_quest has been specified: might be != source_ref
               # CONFLICT!
               if (!identical(source_quest, source_ref)) {
                  warning('same_source is TRUE, but source_quest != source_ref.\nHonoring same_source: ignoring source_quest!')
                  source_quest <- source_ref
               }
            } else {
               # source_ref not specified, already been picked
               # source_quest has been specified: might be != source_ref
               if (!identical(source_quest, source_ref)) {
                  # conflict!
                  # Less severe since source_ref has not been fixed
                  warning('same_source is TRUE, only source_quest has been specified, sampled source_ref != source_quest.\nHonoring same_source: ignoring source_quest!')
                  source_quest <- source_ref
               }
            }

         } else {

            # same_source = FALSE: honoring it
            # source_quest has been specified: might be == source_ref
            # source_ref might be identical to explicit source_quest

            # source_ref specified?
            if (source_ref_in_arguments) {

               # source_ref has been specified
               # CONFLICT!
               if (identical(source_quest, source_ref)) {
                  stop_serious('same_source is FALSE, but source_quest == source_ref.\nCould not honor same_source!')
                  # should resample, either source_ref or source_quest, too difficult

                  # source_quest <- source_ref
               }

            } else {

               # same_source = FALSE: honoring it
               # source_quest has been specified
               # source_ref not specified, already been picked
               # source_ref might be identical to explicit source_quest

               if (identical(source_quest, source_ref)) {
                  stop_serious('same_source is FALSE, only source_quest has been specified, sampled source_ref == source_quest.\nCould not honor same_source!')
                  # should resample, too difficult

                  # source_quest <- source_ref
               } else {
                  message('same_source is FALSE, only source_quest has been specified, sampled source_ref != source_quest.\nHonoring same_source.!')
               }
            }

         }


      } else {
         # same_source not specified: honoring source_quest
         source_quest <- as.vector(source_quest)
      }
   }

   # Questioned source(s) is(are) here:
   # source_quest

   ## Setup sampling -------------------------

   # Check whether same_source is actually honored
   # this should NEVER fail: that's why it raises a "fatal" condition: class fatal/error/condition

   if (identical(unique(source_ref), unique(source_quest)) && (same_source_in_arguments && !same_source)) {
      stop_fatal('this should not happen: same_source = FALSE, yet sources are the same.')
   }
   if (!identical(unique(source_ref), unique(source_quest)) && (same_source_in_arguments && same_source)) {
      stop_fatal('this should not happen: same_source = TRUE, yet sources are different.')
   }
   # # same with assertthat: raise a class assertError/simpleError/error/condition
   #
   # Check whether the questioned and reference sources are the same
   # assertthat::assert_that(
   #    !(identical(unique(source_ref), unique(source_quest)) && (same_source_in_arguments && !same_source)),
   #    msg = 'this should not happen: same_source = FALSE, yet sources are the same.')
   #
   # assertthat::assert_that(
   #    !(!identical(unique(source_ref), unique(source_quest)) && (same_source_in_arguments && same_source)),
   #    msg = 'this should not happen: same_source = TRUE, yet sources are different.'
   # )

   # These should never fail! Already dealt with in the beginning...
   # Indexes of items from the reference source
   idx_ref_all <- which(sources %in% source_ref)
   len_ref <- length(idx_ref_all)
   if (len_ref == 0) {
      stop_fatal('Reference class not found.')
   }

   # Indexes of items from the questioned sources
   idx_quest_all <- which(sources %in% source_quest)
   if (length(idx_quest_all) == 0) {
      stop_fatal('Questioned class not found.')
   }

   ## Sample reference/questioned items -------------------------

   # Build the reference sample
   need_replace_ref <- (len_ref < k_ref)
   if (need_replace_ref) {
      if (replace) {
         message('Reference items: sampling with replacement is being used.')
      }
      else {
         stop_serious('Reference items: not enough items with the reference source, asked to sample without replacement.')
      }
   }
   idx_reference <- sort(sample_safe(idx_ref_all, k_ref, replace = need_replace_ref))

   # Build the questioned sample w/o reference items

   # Do not sample items in the reference sample
   #
   # All other items are kept, and can be sampled from
   idx_quest_all_no_ref <- setdiff(idx_quest_all, idx_reference)
   need_replace_quest <- (length(idx_quest_all_no_ref) < k_quest)
   if (need_replace_quest) {
      if (replace) {
         message('Questioned items: sampling with replacement is being used.')
      }
      else {
         stop_serious('Questioned items: not enough items with the questioned source(s), asked to sample without replacement.')
      }
   }
   idx_questioned <- sort(sample_safe(idx_quest_all_no_ref, k_quest, replace = need_replace_quest))

   ## Sample the background -------------------------

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

   }

   if (length(idx_background) == 0) {
      warning('No background data!')
   }

   list(idx_reference = idx_reference, idx_questioned = idx_questioned, idx_background = idx_background)
}



#' Random Samples and Permutations (safer)
#'
#' `sample_safe` takes a sample of the specified size from the elements of `x` using either with or without replacement.
#'
#' Works like [base::sample()], but it is safer if `x` is a scalar.
#'
#' @param x a vector of one or more elements from which to choose
#' @param size a non-negative integer giving the number of items to choose.
#' @param replace should sampling be with replacement?
#' @export
sample_safe <- function(x, size, replace = FALSE) {
   x[sample.int(length(x), size = size, replace = replace)]
}



