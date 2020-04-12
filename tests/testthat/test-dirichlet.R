context("test-dirichlet")

set.seed(123)

target <- structure(list(`x[1]` = c(0.0109400869678687, 0.0665685887825875, 0.0486625581711922), `x[2]` = c(0.198839172166766, 0.0265597753429308, 0.0968249523653577), `x[3]` = c(0.0325546613582063, 0.0389865451662945, 0.100355956690105), `x[4]` = c(0.224908393864136, 0.466846926568328, 0.374707891621962), `x[5]` = c(0.532757685643023, 0.401038164139859, 0.379448641151384)), class = "data.frame", row.names = c(NA, -3L))
test_that("rdirichlet works", {
   set.seed(123)
   expect_equal(as.data.frame(fun_rdirichlet(3, 1:5)), target)
})

set.seed(123)
target_alpha <- structure(list(`alpha[1]` = 0.0266946739107592, `alpha[2]` = 0.248426309452277,
                               `alpha[3]` = 0.237457770420735, `alpha[4]` = 0.358644295316019,
                               `alpha[5]` = 0.128776950900209), row.names = c(NA, -1L), class = "data.frame")

test_that('fun_rdirichlet_hyperparameter works', {
   set.seed(123)
   expect_equal(fun_rdirichlet_hyperparameter(5) %>% as.data.frame(), target_alpha)
})

target_df_pop <- structure(list(source = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                                           1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L,
                                           3L, 3L, 3L, 3L, 3L), `x[1]` = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3.85539115088201e-98, 3.82288184957026e-20,
                                                                           1.84356277916428e-116, 4.48595401752531e-25, 4.35937882242145e-120,
                                                                           3.41398338118433e-14, 4.43674982598351e-59, 0.0033212535387766,
                                                                           1.10315498014476e-101, 7.50808261107561e-45), `x[2]` = c(3.15744085343108e-05,
                                                                                                                                    1.80170661603836e-11, 2.1190739892559e-07, 0.90479094521583,
                                                                                                                                    2.90571167805466e-13, 7.4472153441854e-07, 8.35490812440887e-05,
                                                                                                                                    7.99702498307031e-16, 2.22783997483947e-11, 0.358991100358271,
                                                                                                                                    0.463207516090584, 0.375800851193689, 0.245665524071608, 0.214548580013953,
                                                                                                                                    0.995720561932198, 0.949579993027434, 0.44828249203877, 0.850850422411632,
                                                                                                                                    0.838530436344301, 0.858072078495411, 0.0361183965131137, 0.418209501876425,
                                                                                                                                    0.999963922845657, 0.261486969426057, 0.760829956516477, 0.991776632194984,
                                                                                                                                    0.378913127097145, 0.474907801250555, 0.997086611576001, 0.0518507498990801
                                                                           ), `x[3]` = c(0.999968425591466, 0.999999999981983, 0.999999788092601,
                                                                                         0.0952090547841702, 0.999999999999709, 0.999999255278466, 0.999916450918756,
                                                                                         0.999999999999999, 0.998924058946169, 0.641008899641729, 0.536792483909416,
                                                                                         0.623223082566973, 0.754334475928392, 0.785451419986047, 0.00427943806567603,
                                                                                         0.0504200069725662, 0.551717507961227, 0.139023358093678, 0.161469563650228,
                                                                                         0.141927921504589, 0.963626873373538, 9.48877686829652e-38, 3.32834096878719e-05,
                                                                                         0.738062294060354, 1.82327784209532e-25, 0.00822083314305214,
                                                                                         0.6162943267893, 0.000313065171526518, 1.29237887340362e-28,
                                                                                         2.24699657314896e-05), `x[4]` = c(2.53976947623737e-22, 1.28671943161904e-41,
                                                                                                                           7.01911445911556e-31, 2.94042942827373e-70, 4.53877035871441e-300,
                                                                                                                           1.13696325346315e-112, 9.35384276509439e-132, 8.10909578417763e-174,
                                                                                                                           0.00107594103155254, 1.41906636292793e-24, 5.29564181047691e-18,
                                                                                                                           0.000976066239337692, 1.00859668760891e-32, 6.51368594112166e-26,
                                                                                                                           2.1261838816413e-12, 5.25760216795696e-32, 2.99996506429303e-15,
                                                                                                                           0.0101262194946897, 5.47093612146069e-12, 8.88852360018686e-47,
                                                                                                                           0.000254730113348415, 0.581790498123575, 2.793744655521e-06,
                                                                                                                           0.000450736513589167, 0.239170043483523, 2.53466193024938e-06,
                                                                                                                           0.00479254611355559, 0.521457880039142, 0.0029133884239986, 0.948126780135188
                                                                                         )), row.names = c(NA, -30L), class = "data.frame")
target_df_sources <- structure(list(source = 1:3, `theta[1]` = c(1.63986104920644e-18,
                                                                 4.34844737477348e-16, 0.00538933153756473), `theta[2]` = c(0.0794047632000204,
                                                                                                                            0.570858558002579, 0.696709769531401), `theta[3]` = c(0.917642035822619,
                                                                                                                                                                                  0.403441491826432, 0.0338381594321575), `theta[4]` = c(0.00295320097736021,
                                                                                                                                                                                                                                         0.0256999501709879, 0.264062739498877)), row.names = c(NA, -3L
                                                                                                                                                                                                                                         ), class = "data.frame")
target_alpha <- structure(list(`alpha[1]` = 0.030640458764655, `alpha[2]` = 0.285146621991887,
                               `alpha[3]` = 0.272556804673721, `alpha[4]` = 0.411656114569737), row.names = c(NA,-1L), class = "data.frame")

test_that("rdirichlet_population works", {
   set.seed(123)
   list_pop <- fun_rdirichlet_population(10, 3, 4)
   expect_equal(list_pop$alpha %>% as.data.frame(), target_alpha)
   expect_equal(list_pop$df_pop %>% as.data.frame(), target_df_pop)
   expect_equal(list_pop$df_sources %>% as.data.frame(), target_df_sources)
})

test_that("rdirichlet_population works, fixed alpha", {
   set.seed(123)
   list_pop <- fun_rdirichlet_population(10, 3, 4, alpha = target_alpha)
   expect_equal(list_pop$alpha %>% as.data.frame(), target_alpha)
   expect_equal(list_pop$df_pop %>% as.data.frame(), target_df_pop)
   expect_equal(list_pop$df_sources %>% as.data.frame(), target_df_sources)
})

test_that("rdirichlet_population generates the correct names", {
   p <- 4
   name_var <- 'kk'
   name_source <- 'tau'
   name_var_target <- fun_var_names(p, name_var)
   name_source_target <- fun_var_names(p, name_source)

   list_pop <- fun_rdirichlet_population(10, 3, p, name_var = name_var, name_source = name_source)
   expect_true(all(names(list_pop$df_pop) %in% c('source', name_var_target)))
   expect_true(all(names(list_pop$df_sources) %in% c('source', name_source_target)))
})

