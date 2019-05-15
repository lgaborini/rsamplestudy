library(testthat)
library(purrr)

library(rsamplestudy)


seed <- floor(runif(1, min = 0, max = 1000))
cat(paste('Using seed:', seed, '\n\n'))
set.seed(seed)
set.seed(196)

context("test-ref-quest-split")


# set.seed(seed)


# Replication -------------------------------------------------------------



# Rerun tests multiple times

if (exists('TRAVIS') && exists('NOT_CRAN')) {
   n_replicate <- ifelse(TRAVIS || !NOT_CRAN, 1, 20)
} else {
   n_replicate <- 20
}

replicate(n_replicate, {


# Parameters --------------------------------------------------------------

# Number of sources
n <- 10
# Number of items per source
m <- 20
sources <- as.numeric(sapply(seq(n), function(s){rep(s, m)}))

sources_all <- 1:n


# Sampling properties

# Number of reference and questioned items
k_ref <- 5
k_quest <- 4

# Number of different questioned sources
# n_quest_diff <- sample.int(n - 1, 1)        # from 0 to m-2 background sources
n_quest_diff <- sample.int(n - 1 - 1, 1)    # guarantee that there is at least one background source
# n_quest_diff <- n - 1                       # no background

# Pick out the reference source
s_ref <- sample(sources_all, 1)
s_quest_same <- s_ref

# and the different questioned source(s)
s_quest_diff_candidates <- setdiff(unique(sources), s_ref)
s_quest_diff <- sort(sample_safe(s_quest_diff_candidates, n_quest_diff))

is_background_empty <- isTRUE(all.equal(unique(sources), unique(sort(union(s_ref, s_quest_diff)))))


# Source restriction (new in 0.3)
n_allowed <- floor(n/2)

# Restricted sources
s_ref_allowed <- sort(union(sample(sources_all, n_allowed), s_ref))
s_quest_allowed_same <- sort(union(sample(sources_all, n_allowed), s_quest_same))
s_quest_allowed_diff <- sort(union(sample(sources_all, n_allowed), s_quest_diff))

# Sources which are not in the restricted set
s_ref_forbidden <- sort(setdiff(sources_all, s_ref_allowed))
s_quest_forbidden_same <- sort(setdiff(sources_all, s_quest_allowed_same))
s_quest_forbidden_diff <- sort(setdiff(sources_all, s_quest_allowed_diff))

# Wrong candidates: explicit sources are not allowed
s_ref_allowed_wrong <- sort(setdiff(sample(sources_all, n_allowed), s_ref))
s_quest_allowed_same_wrong <- sort(setdiff(sample(sources_all, n_allowed), s_quest_same))
s_quest_allowed_diff_wrong <- sort(setdiff(sample(sources_all, n_allowed), s_quest_diff))


# Error expectations
# expect only errors which can be expected
# tests still fail on failed assertions (explicitly: class `fatal`)
# expect_error_all_but_fatal <- partial(expect_error, class = c('simpleError', 'serious'))
expect_error_all_but_fatal <- partial(expect_error, class = c('serious', 'assertError', 'simpleError'))

# make_idx_splits: idx tests, explicit sources -------------------------------------------------------------------


# Source not found
test_that("make_idx_splits: missing reference or questioned source", {
   expect_error_all_but_fatal(make_idx_splits(sources, k_ref, k_quest, source_ref = n + 1, source_quest = s_quest_same))
   expect_error(make_idx_splits(sources, k_ref, k_quest, source_ref = n + 1, source_quest = s_quest_same))
   expect_error_all_but_fatal(make_idx_splits(sources, k_ref, k_quest, source_ref = n + 1, source_quest = s_quest_diff))
   expect_error_all_but_fatal(make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = n + 1))
})

# Same source

test_that("make_idx_splits: quest=ref", {
   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_same)
   expect_true(all(unique(sources[splits$idx_reference]) %in% s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_same))
})

test_that("make_idx_splits: quest=ref, same_source = TRUE", {
   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, same_source = TRUE)
   expect_true(all(unique(sources[splits$idx_reference]) %in% s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_ref))
})

# Different sources

test_that("make_idx_splits: reference source selection, different", {
   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff)
   expect_equal(unique(sources[splits$idx_reference]), s_ref)
   expect_true(all(sources[splits$idx_questioned] %in% s_quest_diff))
})

test_that("make_idx_splits: quest!=ref", {
   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff)
   expect_true(unique(sources[splits$idx_reference] == s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_diff))
   expect_length(intersect(sources[splits$idx_reference], sources[splits$idx_questioned]), 0)
})


test_that("make_idx_splits: quest!=ref, same_source = FALSE", {

   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, same_source = FALSE)
   source_ref_this <- unique(sources[splits$idx_reference])
   source_quest_this <- unique(sources[splits$idx_questioned])

   expect_true(source_ref_this == s_ref)
   expect_true(all(source_quest_this %in% s_quest_diff))
   expect_length(intersect(source_ref_this, source_quest_this), 0)
})


test_that("make_idx_splits: quest!=ref, same_source = TRUE (WARNING)", {

   ## Should complain about overriding
   expect_warning(make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, same_source = TRUE))

   splits <- expect_warning(make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, same_source = TRUE))
   source_ref_this <- unique(sources[splits$idx_reference])
   source_quest_this <- unique(sources[splits$idx_questioned])

   expect_true(source_ref_this == s_ref)
   expect_true(all(source_quest_this %in% s_ref))
})


test_that("make_idx_splits: quest==ref, same_source = FALSE (ERROR)", {
   expect_error_all_but_fatal(
      make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_ref, same_source = FALSE)
   )
})

# make_idx_splits: idx tests, with candidates -------------------------------------------------------------------

# Generic: forbid not allowed explicit sources
test_that("make_idx_splits: forbid restricted explicit sources", {

   expect_error_all_but_fatal(make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_ref_allowed = s_ref_allowed_wrong))
   expect_error_all_but_fatal(make_idx_splits(sources, k_ref, k_quest, source_quest = s_quest_same, source_quest_allowed = s_quest_allowed_same_wrong))
   expect_error_all_but_fatal(make_idx_splits(sources, k_ref, k_quest, source_quest = s_quest_diff, source_quest_allowed = s_quest_allowed_diff_wrong))
})

# With specified candidate sources

test_that("make_idx_splits: quest=ref, restricted ref", {

   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_same, source_ref_allowed = s_ref_allowed)

   expect_true(all(unique(sources[splits$idx_reference]) %in% s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_same))

   # Check candidate restriction
   expect_true(all(unique(sources[splits$idx_reference]) %in% s_ref_allowed))
   expect_true(!any(unique(sources[splits$idx_reference]) %in% s_ref_forbidden))
})

test_that("make_idx_splits: quest=ref, restricted quest", {

   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_same, source_quest_allowed = s_quest_allowed_same)

   expect_true(all(unique(sources[splits$idx_reference]) %in% s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_same))

   # Check candidate restriction
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_allowed_same))
   expect_true(!any(unique(sources[splits$idx_questioned]) %in% s_quest_forbidden_same))
})


# Different sources

test_that("make_idx_splits: quest!=ref, restricted ref", {

   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, source_ref_allowed = s_ref_allowed)

   expect_true(all(unique(sources[splits$idx_reference]) %in% s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_diff))

   # Check candidate restriction
   expect_true(all(unique(sources[splits$idx_reference]) %in% s_ref_allowed))
   expect_true(!any(unique(sources[splits$idx_reference]) %in% s_ref_forbidden))
})

test_that("make_idx_splits: quest!=ref, restricted quest", {

   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, source_quest_allowed = s_quest_allowed_diff)

   expect_true(all(unique(sources[splits$idx_reference]) %in% s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_diff))

   # Check candidate restriction
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_allowed_diff))
   expect_true(!any(unique(sources[splits$idx_questioned]) %in% s_quest_forbidden_diff))
})

test_that("make_idx_splits: quest!=ref, same_source = FALSE, restricted ref", {
   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, same_source = FALSE, source_ref_allowed = s_ref_allowed)
   expect_true(unique(sources[splits$idx_reference] == s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_diff))
   expect_length(intersect(sources[splits$idx_reference], sources[splits$idx_questioned]), 0)

   # Check candidate restriction
   expect_true(all(unique(sources[splits$idx_reference]) %in% s_ref_allowed))
   expect_true(!any(unique(sources[splits$idx_reference]) %in% s_ref_forbidden))
})

test_that("make_idx_splits: quest!=ref, same_source = FALSE, restricted quest", {
   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, same_source = FALSE, source_quest_allowed = s_quest_allowed_diff)
   expect_true(unique(sources[splits$idx_reference] == s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_diff))
   expect_length(intersect(sources[splits$idx_reference], sources[splits$idx_questioned]), 0)

   # Check candidate restriction
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_allowed_diff))
   expect_true(!any(unique(sources[splits$idx_questioned]) %in% s_quest_forbidden_diff))
})


test_that("make_idx_splits: quest!=ref, same_source = TRUE (WARNING)", {


   expect_warning(make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, same_source = TRUE,
                                  source_ref_allowed = s_ref_allowed,
                                  source_quest_allowed = s_quest_allowed_diff))

   splits <- expect_warning(make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, same_source = TRUE))

   expect_true(unique(sources[splits$idx_reference] == s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_ref))
})

test_that("make_idx_splits: quest!=ref, same_source = TRUE, restricted ref+quest", {

   ## Should complain about overriding
   splits <- expect_warning(make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, same_source = TRUE,
      source_ref_allowed = s_ref_allowed,
      source_quest_allowed = s_quest_allowed_diff))

   expect_true(unique(sources[splits$idx_reference] == s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_ref))
})

# make_idx_splits: no explicit sources ------------------------------------


# With specified candidate sources

test_that("make_idx_splits: restricted ref, same_source = TRUE", {

   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref_allowed = s_ref_allowed, same_source = TRUE)

   source_ref_this <- unique(sources[splits$idx_reference])
   source_quest_this <- unique(sources[splits$idx_questioned])

   expect_equal(source_ref_this, source_quest_this)

   # Check candidate restriction
   expect_true(all(source_ref_this %in% s_ref_allowed))
   expect_true(!any(source_ref_this %in% s_ref_forbidden))
})

test_that("make_idx_splits: restricted ref, same_source = FALSE", {

   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref_allowed = s_ref_allowed, same_source = FALSE)

   source_ref_this <- unique(sources[splits$idx_reference])
   source_quest_this <- unique(sources[splits$idx_questioned])

   expect_true(!any(source_ref_this %in% source_quest_this))

   # Check candidate restriction
   expect_true(all(source_ref_this %in% s_ref_allowed))
   expect_true(!any(source_ref_this %in% s_ref_forbidden))
})


test_that("make_idx_splits: restricted quest, same_source = FALSE", {

   splits <- make_idx_splits(sources, k_ref, k_quest, source_quest_allowed = s_quest_allowed_diff, same_source = FALSE)

   source_ref_this <- unique(sources[splits$idx_reference])
   source_quest_this <- unique(sources[splits$idx_questioned])

   expect_true(!any(source_ref_this %in% source_quest_this))

   # Check candidate restriction
   expect_true(all(source_quest_this %in% s_quest_allowed_diff))
   expect_true(!any(source_quest_this %in% s_quest_forbidden_diff))
})


test_that("make_idx_splits: restricted quest, same_source = TRUE", {

   splits <- make_idx_splits(sources, k_ref, k_quest, source_quest_allowed = s_quest_allowed_diff, same_source = TRUE)

   source_ref_this <- unique(sources[splits$idx_reference])
   source_quest_this <- unique(sources[splits$idx_questioned])

   expect_identical(source_ref_this, source_quest_this)

   # Check candidate restriction
   # these may fail as questioned restriction is not honored
   # expect_true(all(source_quest_this %in% s_quest_allowed_diff))
   # expect_true(!any(source_quest_this %in% s_quest_forbidden_diff))
})

# make_idx_splits: Sample with replacement -----------------------------------------------------


test_that("make_idx_splits: quest=ref, not enough samples, replace", {

   # Not enough reference items
   expect_error_all_but_fatal(make_idx_splits(sources, m + 1, k_quest, source_ref = s_ref, source_quest = s_quest_same, replace = FALSE))
   expect_message(make_idx_splits(sources, m + 1, k_quest, source_ref = s_ref, source_quest = s_quest_same, replace = TRUE))
   expect_message(make_idx_splits(sources, m + 1, k_quest, source_ref = s_ref, source_quest = s_quest_same))

   # Not enough questioned items
   expect_error_all_but_fatal(make_idx_splits(sources, k_ref, m + 1, source_ref = s_ref, source_quest = s_quest_same, replace = FALSE))
   expect_message(make_idx_splits(sources, k_ref, m + 1, source_ref = s_ref, source_quest = s_quest_same, replace = TRUE))
   expect_message(make_idx_splits(sources, k_ref, m + 1, source_ref = s_ref, source_quest = s_quest_same))

   # quest=ref: one sample is already taken
   expect_error_all_but_fatal(make_idx_splits(sources, k_ref, m, source_ref = s_ref, source_quest = s_quest_same, replace = FALSE))
   expect_message(make_idx_splits(sources, k_ref, m, source_ref = s_ref, source_quest = s_quest_same, replace = TRUE))
   expect_message(make_idx_splits(sources, k_ref, m, source_ref = s_ref, source_quest = s_quest_same))

})


test_that("make_idx_splits: quest!=ref, not enough samples, replace", {

   # Not enough reference items
   expect_error_all_but_fatal(make_idx_splits(sources, m + 1, k_quest, source_ref = s_ref, source_quest = s_quest_diff, replace = FALSE))
   expect_message(make_idx_splits(sources, m + 1, k_quest, source_ref = s_ref, source_quest = s_quest_diff, replace = TRUE))
   expect_message(make_idx_splits(sources, m + 1, k_quest, source_ref = s_ref, source_quest = s_quest_diff))

   # Not enough questioned items
   expect_error_all_but_fatal(make_idx_splits(sources, k_ref, m*n_quest_diff + 1, source_ref = s_ref, source_quest = s_quest_diff, replace = FALSE))
   expect_message(make_idx_splits(sources, k_ref, m*n_quest_diff + 1, source_ref = s_ref, source_quest = s_quest_diff, replace = TRUE))
   expect_message(make_idx_splits(sources, k_ref, m*n_quest_diff + 1, source_ref = s_ref, source_quest = s_quest_diff))

   # quest!=ref: should be silent
   expect_silent(make_idx_splits(sources, k_ref, m*n_quest_diff, source_ref = s_ref, source_quest = s_quest_diff, replace = FALSE))
   expect_silent(make_idx_splits(sources, k_ref, m*n_quest_diff, source_ref = s_ref, source_quest = s_quest_diff, replace = TRUE))
   expect_silent(make_idx_splits(sources, k_ref, m*n_quest_diff, source_ref = s_ref, source_quest = s_quest_diff))

})


# make_idx_splits: Sample intersections -----------------------------------------------------


test_that("make_idx_splits: verify that background is non-intersecting, outside", {
   splits <- make_idx_splits(sources, k_ref, k_quest, background = 'outside')
   expect_length(intersect(splits$idx_reference, splits$idx_questioned), 0)
   expect_length(intersect(splits$idx_reference, splits$idx_background), 0)
   expect_length(intersect(splits$idx_questioned, splits$idx_background), 0)
})

# background: others
test_that("make_idx_splits: others (sampling from other sources), no background (WARNING)", {
   expect_warning(make_idx_splits(sources, k_ref, k_quest, background = 'others'))
})

if (!is_background_empty) {
   test_that("make_idx_splits: verify that background is non-intersecting, others (sampling from other sources), with background", {
      splits <- make_idx_splits(sources, k_ref, k_quest, background = 'others', source_ref = s_ref, source_quest = s_quest_diff)

      expect_length(intersect(splits$idx_reference, splits$idx_questioned), 0)
      expect_length(intersect(splits$idx_reference, splits$idx_background), 0)
      expect_length(intersect(splits$idx_questioned, splits$idx_background), 0)

      source_ref <- unique(sources[splits$idx_reference])
      source_quest <- unique(sources[splits$idx_questioned])
      source_background <- unique(sources[splits$idx_background])
      expect_length(intersect(source_ref, source_background), 0)
      expect_length(intersect(source_quest, source_background), 0)
   })
}

# background: unobserved
test_that("make_idx_splits: verify that background is non-intersecting, unobserved", {
   splits <- make_idx_splits(sources, k_ref, k_quest, background = 'unobserved')
   expect_length(intersect(splits$idx_reference, splits$idx_questioned), 0)
   expect_length(intersect(splits$idx_reference, splits$idx_background), 0)
   expect_length(intersect(splits$idx_questioned, splits$idx_background), 0)
})

if (!is_background_empty) {

   test_that("make_idx_splits: verify that background is non-intersecting, unobserved (sampling from unseen sources), with actual background", {
      splits <- make_idx_splits(sources, k_ref, k_quest, background = 'unobserved', source_ref = s_ref)

      expect_length(intersect(splits$idx_reference, splits$idx_questioned), 0)
      expect_length(intersect(splits$idx_reference, splits$idx_background), 0)
      expect_length(intersect(splits$idx_questioned, splits$idx_background), 0)

      source_ref <- unique(sources[splits$idx_reference])
      source_background <- unique(sources[splits$idx_background])
      source_quest_actual <- unique(sources[splits$idx_questioned])
      source_quest_potential <- setdiff(sources_all, s_ref)
      source_quest_in_background <- setdiff(source_quest_potential, source_quest_actual)

      expect_length(intersect(source_ref, source_background), 0)
      expect_length(intersect(source_ref, source_quest_actual), 0)
      expect_length(intersect(source_quest_actual, source_background), 0)
      expect_gte(length(intersect(source_quest_in_background, source_background)), 0)
   })

}

# make_dataset_splits: df tests ----------------------------------------------------------------

df <- data.frame(source = sources, x = rnorm(length(sources)))
df_item <- data.frame(item = sources, x = rnorm(length(sources)))


# Source not found
test_that("make_idx_splits: missing reference or questioned source", {
   expect_error_all_but_fatal(make_dataset_splits(df, k_ref, k_quest, source_ref = n + 1, source_quest = s_quest_same))
   expect_error_all_but_fatal(make_dataset_splits(df, k_ref, k_quest, source_ref = n + 1, source_quest = s_quest_diff))
   expect_error_all_but_fatal(make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, source_quest = n + 1))
})

test_that("make_dataset_splits: quest=ref", {
   splits <- make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_same)
   expect_true(all(unique(splits$df_reference$source) %in% s_ref))
   expect_true(all(unique(splits$df_questioned$source) %in% s_quest_same))
})


test_that("make_dataset_splits: quest~=ref", {
   splits <- make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff)
   expect_true(unique(splits$df_reference$source) == s_ref)
   expect_true(all(unique(splits$df_questioned$source) %in% s_quest_diff))
   expect_length(intersect(splits$df_reference$source, splits$df_questioned$source), 0)
})


test_that("make_dataset_splits: quest~=ref, renamed column", {
   expect_error_all_but_fatal(make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, col_source = 'item'))
   expect_error_all_but_fatal(make_dataset_splits(df_item, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff))
   col_source <- 'item'
   splits <- make_dataset_splits(df_item, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, col_source = 'item')
   expect_true(unique(splits$df_reference[[col_source]]) == s_ref)
   expect_true(all(unique(splits$df_questioned[[col_source]]) %in% s_quest_diff))
   expect_length(intersect(splits$df_reference[[col_source]], splits$df_questioned[[col_source]]), 0)
})


# make_dataset_splits: idx tests -------------------------------------------------------------------

# Same source
test_that("make_dataset_splits: reference source selection, same", {
   splits <- make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_same)
   expect_equal(unique(df$source[splits$idx_reference]), s_ref)
   expect_equal(unique(df$source[splits$idx_questioned]), s_quest_same)
})


test_that("make_dataset_splits: quest=ref", {
   splits <- make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_same)
   expect_true(all(unique(df$source[splits$idx_reference]) %in% s_ref))
   expect_true(all(unique(df$source[splits$idx_questioned]) %in% s_quest_same))
})


test_that("make_dataset_splits: quest=ref, same_source = TRUE", {
   splits <- make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, same_source = TRUE)
   expect_true(all(unique(df$source[splits$idx_reference]) %in% s_ref))
   expect_true(all(unique(df$source[splits$idx_questioned]) %in% s_ref))
})

# Different sources

test_that("make_dataset_splits: reference source selection, different", {
   splits <- make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff)
   expect_equal(unique(df$source[splits$idx_reference]), s_ref)
   expect_true(all(df$source[splits$idx_questioned] %in% s_quest_diff))
})

test_that("make_dataset_splits: quest~=ref", {
   splits <- make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff)
   expect_true(unique(df$source[splits$idx_reference] == s_ref))
   expect_true(all(unique(df$source[splits$idx_questioned]) %in% s_quest_diff))
   expect_length(intersect(df$source[splits$idx_reference], df$source[splits$idx_questioned]), 0)
})


test_that("make_dataset_splits: quest~=ref, same_source = FALSE", {
   splits <- make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, same_source = FALSE)
   expect_true(unique(df$source[splits$idx_reference] == s_ref))
   expect_true(all(unique(df$source[splits$idx_questioned]) %in% s_quest_diff))
   expect_length(intersect(df$source[splits$idx_reference], df$source[splits$idx_questioned]), 0)
})


# make_dataset_splits: Sample intersections -----------------------------------------------------


test_that("make_dataset_splits: verify that background is non-intersecting, outside", {
   splits <- make_dataset_splits(df, k_ref, k_quest, background = 'outside')
   expect_length(intersect(splits$idx_reference, splits$idx_questioned), 0)
   expect_length(intersect(splits$idx_reference, splits$idx_background), 0)
   expect_length(intersect(splits$idx_questioned, splits$idx_background), 0)
})

test_that("make_dataset_splits: others (sampling from other sources), no background (WARNING)", {
   expect_warning(make_dataset_splits(df, k_ref, k_quest, background = 'others'))

})

if (!is_background_empty) {
   test_that("make_dataset_splits: verify that background is non-intersecting, others (sampling from other sources), with background", {
      splits <- make_dataset_splits(df, k_ref, k_quest, background = 'others', source_ref = s_ref, source_quest = s_quest_diff)

      expect_length(intersect(splits$idx_reference, splits$idx_questioned), 0)
      expect_length(intersect(splits$idx_reference, splits$idx_background), 0)
      expect_length(intersect(splits$idx_questioned, splits$idx_background), 0)

      source_ref <- unique(df$source[splits$idx_reference])
      source_quest <- unique(df$source[splits$idx_questioned])
      source_background <- unique(df$source[splits$idx_background])
      expect_length(intersect(source_ref, source_background), 0)
      expect_length(intersect(source_quest, source_background), 0)
   })
}



# background: unobserved
test_that("make_dataset_splits: verify that background is non-intersecting, unobserved", {
   splits <- make_dataset_splits(df, k_ref, k_quest, background = 'unobserved')
   expect_length(intersect(splits$idx_reference, splits$idx_questioned), 0)
   expect_length(intersect(splits$idx_reference, splits$idx_background), 0)
   expect_length(intersect(splits$idx_questioned, splits$idx_background), 0)
})

if (!is_background_empty) {

   test_that("make_dataset_splits: verify that background is non-intersecting, unobserved (sampling from unseen sources), with actual background", {
      splits <- make_dataset_splits(df, k_ref, k_quest, background = 'unobserved', source_ref = s_ref)

      expect_length(intersect(splits$idx_reference, splits$idx_questioned), 0)
      expect_length(intersect(splits$idx_reference, splits$idx_background), 0)
      expect_length(intersect(splits$idx_questioned, splits$idx_background), 0)

      source_ref <- unique(splits$df_reference$source)
      source_background <- unique(splits$df_background$source)
      source_quest_actual <- unique(splits$df_questioned$source)
      source_quest_potential <- setdiff(sources_all, s_ref)
      source_quest_in_background <- setdiff(source_quest_potential, source_quest_actual)

      expect_length(intersect(source_ref, source_background), 0)
      expect_length(intersect(source_ref, source_quest_actual), 0)
      expect_length(intersect(source_quest_actual, source_background), 0)
      expect_gte(length(intersect(source_quest_in_background, source_background)), 0)
   })

}


# End replicate -----------------------------------------------------------



})    # /Rerun tests multiple times
