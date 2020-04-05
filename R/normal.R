#' Generate a Normal-Normal population.
#'
#' Generate samples from m sources and p parameters, n sample per source.
#' Optionally pass the between-source hyperparameters used to generate the source parameters.
#'
#'
#' @section Model:
#'
#' The Normal-Normal model:
#'
#' \deqn{X_{ij} ~ N(\mu_i, \sigma_i^2)  i = 1, \ldots, n, j = 1, \ldots, m}
#' \deqn{\mu_i ~ N(m_{\mu}, s_{\mu}^2)   j = 1, \ldots, m}
#' \deqn{\sigma_i ~ N(m_{\sigma}, s_{\sigma}^2)    j = 1, \ldots, m}
#'
#'
#' @param n number of samples per source
#' @param m number of sources
#' @param list_hyper a list containing the hyperparameters:
#'    - 'm_mu', 'm_sigma': the between-source mean hyperparameters
#'    - 's_mu', 's_sigma': the between-source sd hyperparameters
#'    A partial list will be merged.
#'    If `NULL`, they are generated from the N(0,1) distribution.
#'
#' @param name_var names for data variables (default: `'x'`)
#' @param name_source named list or character vector with names for source parameters \eqn{\mu} and \eqn{\sigma} (default: `list(mu = 'mu', sigma = 'sigma')`)
#'
#' @return list of samples:
#'
#' - `list_hyper`: a list containing the hyperparameters:
#' - `df_sources`: tibble of the Dirichlet population parameters, source column is `'source'`, variables start with `name_source`
#' - `df_pop`: the Dirichlet data, source column is `'source'`, variables start with `name_var`
#' - `names_var`: names of columns containing data variables
#' - `names_source`: names of columns containing source variables
#'
#' @export
#' @importFrom utils modifyList
#' @importFrom stats rnorm
#' @family population functions
#' @concept population
#' @importFrom rlang :=
fun_rnorm_population <- function(n, m, list_hyper = NULL, name_var = 'x', name_source = list(mu = 'mu', sigma = 'sigma')) {

   if (!is.list(name_source) | is.null(names(name_source))) {
      stop('name_source must be a named list.')
   }
   if (length(name_source) != 2 | !all(sapply(name_source, is.character)))
      stop('name_source must be a 2-length character vector.')
   if (!is.character(name_var) | length(name_var) != 1) stop('name_var must be a 1-length character vector.')

   stopifnot(is.numeric(n) & n >= 1)
   stopifnot(is.numeric(m) & m >= 1)

   # Setup hyperparameters
   list_hyper_defaults <- list()
   list_hyper_defaults$m_mu <- rnorm(1)
   list_hyper_defaults$m_sigma <- abs(rnorm(1))
   list_hyper_defaults$s_mu <- rnorm(1)
   list_hyper_defaults$s_sigma <- abs(rnorm(1))

   if (is.null(list_hyper)) list_hyper <- list()
   list_hyper <- utils::modifyList(list_hyper_defaults, list_hyper)

   col_source <- 'source'
   df_sources <- with(list_hyper, tibble::tibble(
         mu = rnorm(m, m_mu, m_sigma),
         sigma = abs(rnorm(m, s_mu, s_sigma))
      )) %>%
      purrr::set_names(c(name_source$mu, name_source$sigma)) %>%
      tibble::add_column(source = 1:m, .before = 1)

   samples <- .nest_data <- NULL   # avoid R CMD check error
   df_pop <- df_sources %>%
      dplyr::group_by(source) %>%
      tidyr::nest(.nest_data = -source) %>%
      dplyr::mutate(samples = purrr::map(.nest_data, ~ stats::rnorm(n, purrr::pluck(.x, name_source$mu), purrr::pluck(.x, name_source$sigma)) )) %>%
      tidyr::unnest(samples) %>%
      dplyr::rename(!!name_var := samples) %>%
      dplyr::select(-.nest_data) %>%
      dplyr::ungroup()

   list(
      list_hyper = list_hyper,
      df_sources = df_sources,
      df_pop = df_pop,
      names_var = name_var,
      names_source = name_source
   )
}