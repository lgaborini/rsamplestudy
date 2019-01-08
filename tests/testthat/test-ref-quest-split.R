library(testthat)
library(purrr)

library(rsamplestudy)

context("test-ref-quest-split")


# Rerun tests multiple times
n_replicate <- 20
replicate(n_replicate, {


# Parameters --------------------------------------------------------------

# Number of sources
n <- 10
# Number of items per source
m <- 20
sources <- as.numeric(sapply(seq(n), function(s){rep(s, m)}))


# Sampling properties

# Number of reference and questioned items
k_ref <- 5
k_quest <- 4

# Number of different questioned sources
n_quest_diff <- sample.int(n - 1, 1)
n_quest_diff <- sample.int(n - 1 - 1, 1)  # guarantee that there is at least one background source
# n_quest_diff <- n - 1                     # no background

# Pick out the reference source
s_ref <- sample(seq(n), 1)
s_quest_same <- s_ref

# and the different questioned source(s)
s_quest_diff_candidates <- setdiff(unique(sources), s_ref)
s_quest_diff <- sort(resample(s_quest_diff_candidates, n_quest_diff))

is_background_empty <- isTRUE(all.equal(unique(sources), unique(sort(union(s_ref, s_quest_diff)))))

# make_idx_splits: idx tests -------------------------------------------------------------------


# Source not found
test_that("make_idx_splits: missing reference or questioned source", {
   expect_error(make_idx_splits(sources, k_ref, k_quest, source_ref = n + 1, source_quest = s_quest_same))
   expect_error(make_idx_splits(sources, k_ref, k_quest, source_ref = n + 1, source_quest = s_quest_diff))
   expect_error(make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = n + 1))
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
   expect_true(unique(sources[splits$idx_reference] == s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_diff))
   expect_length(intersect(sources[splits$idx_reference], sources[splits$idx_questioned]), 0)
})

# make_idx_splits: Sample with replacement -----------------------------------------------------


test_that("make_idx_splits: quest=ref, not enough samples, replace", {

   # Not enough reference items
   expect_error(make_idx_splits(sources, m + 1, k_quest, source_ref = s_ref, source_quest = s_quest_same, replace = FALSE))
   expect_message(make_idx_splits(sources, m + 1, k_quest, source_ref = s_ref, source_quest = s_quest_same, replace = TRUE))
   expect_message(make_idx_splits(sources, m + 1, k_quest, source_ref = s_ref, source_quest = s_quest_same))

   # Not enough questioned items
   expect_error(make_idx_splits(sources, k_ref, m + 1, source_ref = s_ref, source_quest = s_quest_same, replace = FALSE))
   expect_message(make_idx_splits(sources, k_ref, m + 1, source_ref = s_ref, source_quest = s_quest_same, replace = TRUE))
   expect_message(make_idx_splits(sources, k_ref, m + 1, source_ref = s_ref, source_quest = s_quest_same))

   # quest=ref: one sample is already taken
   expect_error(make_idx_splits(sources, k_ref, m, source_ref = s_ref, source_quest = s_quest_same, replace = FALSE))
   expect_message(make_idx_splits(sources, k_ref, m, source_ref = s_ref, source_quest = s_quest_same, replace = TRUE))
   expect_message(make_idx_splits(sources, k_ref, m, source_ref = s_ref, source_quest = s_quest_same))

})


test_that("make_idx_splits: quest!=ref, not enough samples, replace", {

   # Not enough reference items
   expect_error(make_idx_splits(sources, m + 1, k_quest, source_ref = s_ref, source_quest = s_quest_diff, replace = FALSE))
   expect_message(make_idx_splits(sources, m + 1, k_quest, source_ref = s_ref, source_quest = s_quest_diff, replace = TRUE))
   expect_message(make_idx_splits(sources, m + 1, k_quest, source_ref = s_ref, source_quest = s_quest_diff))

   # Not enough questioned items
   expect_error(make_idx_splits(sources, k_ref, m*n_quest_diff + 1, source_ref = s_ref, source_quest = s_quest_diff, replace = FALSE))
   expect_message(make_idx_splits(sources, k_ref, m*n_quest_diff + 1, source_ref = s_ref, source_quest = s_quest_diff, replace = TRUE))
   expect_message(make_idx_splits(sources, k_ref, m*n_quest_diff + 1, source_ref = s_ref, source_quest = s_quest_diff))

   # quest!=ref: should be silent
   expect_silent(make_idx_splits(sources, k_ref, m*n_quest_diff, source_ref = s_ref, source_quest = s_quest_diff, replace = FALSE))
   expect_silent(make_idx_splits(sources, k_ref, m*n_quest_diff, source_ref = s_ref, source_quest = s_quest_diff, replace = TRUE))
   expect_silent(make_idx_splits(sources, k_ref, m*n_quest_diff, source_ref = s_ref, source_quest = s_quest_diff))

})



# make_idx_splits: Sample intersections -----------------------------------------------------


test_that("make_idx_splits: background is non-intersecting, outside", {
   splits <- make_idx_splits(sources, k_ref, k_quest, background = 'outside')
   expect_length(intersect(splits$idx_reference, splits$idx_questioned), 0)
   expect_length(intersect(splits$idx_reference, splits$idx_background), 0)
   expect_length(intersect(splits$idx_questioned, splits$idx_background), 0)
})

test_that("make_idx_splits: background is non-intersecting, others (sampling from other sources), no background", {
   expect_warning(make_idx_splits(sources, k_ref, k_quest, background = 'others'))

})

if (!is_background_empty) {
   test_that("make_idx_splits: background is non-intersecting, others (sampling from other sources), with background", {
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

# make_dataset_splits: df tests ----------------------------------------------------------------

df <- data.frame(source = sources, x = rnorm(length(sources)))
df_item <- data.frame(item = sources, x = rnorm(length(sources)))


# Source not found
test_that("make_idx_splits: missing reference or questioned source", {
   expect_error(make_dataset_splits(df, k_ref, k_quest, source_ref = n + 1, source_quest = s_quest_same))
   expect_error(make_dataset_splits(df, k_ref, k_quest, source_ref = n + 1, source_quest = s_quest_diff))
   expect_error(make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, source_quest = n + 1))
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
   expect_error(make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, col_source = 'item'))
   expect_error(make_dataset_splits(df_item, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff))
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


test_that("make_dataset_splits: background is non-intersecting, outside", {
   splits <- make_dataset_splits(df, k_ref, k_quest, background = 'outside')
   expect_length(intersect(splits$idx_reference, splits$idx_questioned), 0)
   expect_length(intersect(splits$idx_reference, splits$idx_background), 0)
   expect_length(intersect(splits$idx_questioned, splits$idx_background), 0)
})

test_that("make_dataset_splits: background is non-intersecting, others (sampling from other sources), no background", {
   expect_warning(make_dataset_splits(df, k_ref, k_quest, background = 'others'))

})

if (!is_background_empty) {
   test_that("make_dataset_splits: background is non-intersecting, others (sampling from other sources), with background", {
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

})    # /Rerun tests multiple times