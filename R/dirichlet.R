#' Sample n times from a Dirichlet
#'
#' @param n number of samples
#' @param a Dirichlet parameter (a vector)
#' @param text optional variable names (default: `'x'`)
#' @return a tibble with named colums, where each row is a Dirichlet sample
#' @export
#' @inheritParams fun_rdirichlet_hyperparameter
#' @seealso [Compositional::rdiri()]
#' @family RNG functions
fun_rdirichlet <- function(n, a, text='x') {
   a <- as.numeric(a)
   p <- length(a)

   Compositional::rdiri(n, a) %>%
      tibble::as_tibble(.name_repair = 'minimal') %>%
      purrr::set_names(fun_var_names(p, text=text))
}


#' Sample the Dirichlet hyperparameter
#'
#' Assume it comes from a Uniform distribution on the $(p-1)$-simplex.
#'
#' Formally, we sample from a Dirichlet with constant base measure, and concentration parameter = p.
#' The resulting Dirichlet parameter is constant = 1.
#'
#' @param p number of variables
#' @return a tibble with named colums
#' @export
#' @family RNG functions
fun_rdirichlet_hyperparameter <- function(p) {
   fun_rdirichlet(1, p*rep(1/p, p), 'alpha')
}


#' Generate a Dirichlet-Dirichlet population.
#'
#' Generate samples from m sources and p parameters, n sample per source.
#' Optionally pass the between-source alpha hyperparameter used to generate the source parameters.
#'
#' @param n number of samples per source
#' @param m number of sources
#' @param alpha between-source alpha hyperparameter.
#'    If `NULL`, it is generated from the Uniform distribution on the (p-1)-simplex (see [fun_rdirichlet_hyperparameter()]).
#' @param name_var names for data variables (default: `'x'`)
#' @param name_source names for source parameters (default: `'theta'`)
#'
#' @return list of samples:
#'
#' - `alpha`: the Dirichlet hyperparameter
#' - `df_sources`: tibble of the Dirichlet population parameters, source column is `'source'`, variables start with `name_source`
#' - `df_pop`: the Dirichlet data, source column is `'source'`, variables start with `name_var`
#' - `names_var`: names of columns containing data variables
#' - `names_source`: names of columns containing source variables
#'
#' @export
#' @inheritParams fun_rdirichlet_hyperparameter
#' @family population functions
#' @concept population
fun_rdirichlet_population <- function(n, m, p, alpha=NULL, name_var='x', name_source='theta') {

   if (is.null(alpha)) {
      alpha <- fun_rdirichlet_hyperparameter(p)
   } else {
      stopifnot(length(alpha) == p)
   }

   col_source <- 'source'
   df_sources <- fun_rdirichlet(m, alpha, text = name_source) %>%
      tibble::add_column(source = 1:m, .before = 1)
   names_source <- setdiff(colnames(df_sources), col_source)

   samples <- NULL   # avoid R CMD check error
   df_pop <- df_sources %>%
      dplyr::group_by(source) %>%
      tidyr::nest(.key = name_source) %>%
      dplyr::mutate(samples = purrr::map(name_source, ~ fun_rdirichlet(n, .x, name_var))) %>%
      tidyr::unnest(samples)
   names_var <- setdiff(colnames(df_pop), col_source)

   list(alpha = alpha,
        df_sources = df_sources,
        df_pop = df_pop,
        names_var = names_var,
        names_source = names_source)
}

