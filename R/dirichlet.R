#' Sample n times from a Dirichlet
#'
#' @param n number of samples
#' @param a Dirichlet parameter (a vector that will be coerced to numeric)
#' @param text optional variable names (default: `'x'`)
#' @return a tibble with named columns, where each row is a Dirichlet sample
#' @export
#' @seealso [Compositional::rdiri()]
#' @family RNG functions
#' @importFrom magrittr set_colnames
#' @importFrom tibble as_tibble
#' @importFrom Compositional rdiri
fun_rdirichlet <- function(n, a, text = 'x') {

   a <- as.numeric(a)
   p <- length(a)

   Compositional::rdiri(n, a) %>%
      magrittr::set_colnames(fun_var_names(p, text = text)) %>%
      tibble::as_tibble()
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
   stopifnot(is.numeric(p) && p >= 1)
   fun_rdirichlet(n = 1, a = p * rep(1/p, p), text = 'alpha')
}


#' Generate a Dirichlet-Dirichlet population.
#'
#' Generate samples from m sources and p parameters, n sample per source.
#' Optionally pass the between-source alpha hyperparameter used to generate the source parameters.
#'
#' @param n number of samples per source
#' @param m number of sources
#' @param alpha between-source alpha hyperparameter, a numeric vector or 1-row data.frame.
#'    If `NULL` (default), it is generated from the Uniform distribution on the (p-1)-simplex (see [fun_rdirichlet_hyperparameter()]).
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
#' @importFrom withr with_preserve_seed
#' @importFrom tibble add_column
#' @importFrom dplyr group_by mutate select ungroup
#' @importFrom purrr map
#' @importFrom tidyr nest unnest
fun_rdirichlet_population <- function(n, m, p,
                                      alpha = NULL, name_var = 'x', name_source = 'theta') {

   stopifnot(is.numeric(n) & n >= 1)
   stopifnot(is.numeric(m) & m >= 1)

   if (is.null(alpha)) {
      # do not advance RNG seed
      df_alpha <- withr::with_preserve_seed(fun_rdirichlet_hyperparameter(p))
      alpha <- as.numeric(df_alpha)
   } else {
      stopifnot(is.numeric(alpha) | is.data.frame(alpha))
      stopifnot(length(alpha) == p)

      alpha <- as.numeric(alpha)

      # set names on alpha
      df_alpha <- setNames(alpha, nm = fun_var_names(p, text = 'alpha')) %>% dplyr::bind_rows()
      # df_alpha <- setNames(alpha, nm = fun_var_names(p, text = 'alpha')) %>% tibble::as_tibble_row()
   }

   col_source <- 'source'

   df_sources <- fun_rdirichlet(m, alpha, text = name_source) %>%
      tibble::add_column(source = 1:m, .before = 1)

   names_source <- setdiff(colnames(df_sources), col_source)

   samples <- NULL   # avoid R CMD check error

   df_pop <- df_sources %>%
      dplyr::group_by(source) %>%
      tidyr::nest(name_source = -source) %>%
      dplyr::mutate(samples = purrr::map(name_source, ~ fun_rdirichlet(n = n, a = as.vector(.x), text = name_var))) %>%
      tidyr::unnest(samples) %>%
      dplyr::select(-name_source) %>%
      dplyr::ungroup()

   names_var <- setdiff(colnames(df_pop), col_source)

   list(
      alpha = df_alpha,
      df_sources = df_sources,
      df_pop = df_pop,
      names_var = names_var,
      names_source = names_source
   )
}

