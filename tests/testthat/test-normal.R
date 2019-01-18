context("test-normal")

test_that("rnormal_population works", {
   n <- 100
   m <- 20
   list_hyper <- list(m_mu = -1000, m_sigma = 0.001, s_mu = 0.001, s_sigma = 0.001)
   list_pop <- fun_rnorm_population(n = n, m = m, list_hyper = list_hyper)

   expect_true(all(list_pop$df_sources$mu < -900))
   expect_true(all(list_pop$df_sources$mu > -1100))

   expect_true(all(list_pop$df_pop$x < -950))
   expect_true(all(list_pop$df_pop$x > -1050))

})




test_that("rnormal_population generates the correct names", {
   name_var <- 'kk'
   name_source <- list(mu = 'mu_src', sigma = 'sigma_src')

   list_pop <- fun_rnorm_population(10, 3, name_var = name_var, name_source = name_source)
   expect_true(all(names(list_pop$df_pop) %in% c('source', name_var)))
   expect_true(all(names(list_pop$df_sources) %in% c('source', name_source)))
})
