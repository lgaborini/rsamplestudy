library(testthat)
library(purrr)

library(rsamplestudy)

context("test-ref-quest-split")


# Parameters --------------------------------------------------------------

n <- 10    # number of sources
m <- 20    # number of items per source
sources <- as.numeric(sapply(seq(n), function(s){rep(s, m)}))
T
k_ref <- 5
k_quest <- 4

# number of different questioned sources
n_quest_diff <- 5
n_quest_diff <- n - 1
n_quest_diff <- 1


s_ref <- sample(1:m, 1)
s_quest_same <- s_ref
s_quest_diff <- sort(sample(setdiff(unique(sources), s_ref), n_quest_diff))



# idx tests -------------------------------------------------------------------

# Same source
test_that("make_idx_splits: reference source selection, same", {
   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_same)
   expect_equal(unique(sources[splits$idx_reference]), s_ref)
   expect_equal(unique(sources[splits$idx_questioned]), s_quest_same)
})


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

test_that("make_idx_splits: quest~=ref", {
   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff)
   expect_true(unique(sources[splits$idx_reference] == s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_diff))
   expect_length(intersect(sources[splits$idx_reference], sources[splits$idx_questioned]), 0)
})


test_that("make_idx_splits: quest~=ref, same_source = FALSE", {
   splits <- make_idx_splits(sources, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff, same_source = FALSE)
   expect_true(unique(sources[splits$idx_reference] == s_ref))
   expect_true(all(unique(sources[splits$idx_questioned]) %in% s_quest_diff))
   expect_length(intersect(sources[splits$idx_reference], sources[splits$idx_questioned]), 0)
})

# Sampe intersections -----------------------------------------------------


test_that("make_idx_splits: background is non-intersecting, outside", {
   splits <- make_idx_splits(sources, k_ref, k_quest, background = 'outside')
   expect_length(intersect(splits$idx_reference, splits$idx_questioned), 0)
   expect_length(intersect(splits$idx_reference, splits$idx_background), 0)
   expect_length(intersect(splits$idx_questioned, splits$idx_background), 0)
})

test_that("make_idx_splits: background is non-intersecting, others", {
   splits <- make_idx_splits(sources, k_ref, k_quest, background = 'others')
   expect_length(intersect(splits$idx_reference, splits$idx_questioned), 0)
   expect_length(intersect(splits$idx_reference, splits$idx_background), 0)
   expect_length(intersect(splits$idx_questioned, splits$idx_background), 0)
   expect_length(intersect(splits$idx_questioned, splits$idx_background), 0)
})

test_that("make_idx_splits: background is non-intersecting, sampling from other sources", {
   splits <- make_idx_splits(sources, k_ref, k_quest, background = 'others')
   source_ref <- unique(sources[splits$idx_reference])
   source_quest <- unique(sources[splits$idx_questioned])
   source_background <- unique(sources[splits$idx_background])
   expect_length(intersect(source_ref, source_background), 0)
   expect_length(intersect(source_background, source_background), 0)
})

# df tests ----------------------------------------------------------------

df <- data.frame(source = sources, x = rnorm(length(sources)))



test_that("make_idx_splits: quest=ref", {
   splits <- make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_same)
   expect_true(all(unique(splits$df_reference$source) %in% s_ref))
   expect_true(all(unique(splits$df_questioned$source) %in% s_quest_same))
})


test_that("make_idx_splits: quest~=ref", {
   splits <- make_dataset_splits(df, k_ref, k_quest, source_ref = s_ref, source_quest = s_quest_diff)
   expect_true(unique(splits$df_reference$source) == s_ref)
   expect_true(all(unique(splits$df_questioned$source) %in% s_quest_diff))
   expect_length(intersect(splits$df_reference$source, splits$df_questioned$source), 0)
})


