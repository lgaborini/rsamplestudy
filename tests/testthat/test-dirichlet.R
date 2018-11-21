library(rsamplestudy)
library(testthat)
context("test-dirichlet")

set.seed(123)

target <- structure(list(`x[1]` = c(0.0109400869678687, 0.0665685887825875, 0.0486625581711922), `x[2]` = c(0.198839172166766, 0.0265597753429308, 0.0968249523653577), `x[3]` = c(0.0325546613582063, 0.0389865451662945, 0.100355956690105), `x[4]` = c(0.224908393864136, 0.466846926568328, 0.374707891621962), `x[5]` = c(0.532757685643023, 0.401038164139859, 0.379448641151384)), class = "data.frame", row.names = c(NA, -3L))
test_that("rdirichlet works", {
   set.seed(123)
   expect_equal(as.data.frame(fun_rdirichlet(3, 1:5)), target)
})

target_df_pop <- structure(list(source = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), `x[1]` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), `x[2]` = c(0, 0, 6.07834207322629e-64, 0, 0, 3.83088388856914e-244, 6.38540825854815e-29, 0, 0, 3.76918729734925e-209, 0, 0, 0, 0, 0, 4.53418557420225e-180, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), `x[3]` = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), `x[4]` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)), class = "data.frame", row.names = c(NA, -30L))
target_df_sources <- structure(list(source = 1:3, `theta[1]` = c(1.53313808454979e-89, 2.84176711379251e-18, 3.89961008682007e-70), `theta[2]` = c(0.00135797930993232, 3.50688724363659e-05, 8.58292741076733e-07), `theta[3]` = c(0.998642020690068, 0.999964931127564, 0.999999141707259), `theta[4]` = c(0, 0, 0 )), class = "data.frame", row.names = c(NA, -3L))
target_alpha <- structure(list(`alpha[1]` = 0.0054360038282456, `alpha[2]` = 0.0618219556613139, `alpha[3]` = 0.932652845992375, `alpha[4]` = 8.9194518065798e-05), class = "data.frame", row.names = c(NA, -1L))

test_that("rdirichlet_population works", {
   set.seed(123)
   list_pop <- fun_rdirichlet_population(10, 3, 4)
   expect_equal(list_pop$alpha %>% as.data.frame(), target_alpha)
   expect_equal(list_pop$df_pop %>% as.data.frame(), target_df_pop)
   expect_equal(list_pop$df_sources %>% as.data.frame(), target_df_sources)
})

