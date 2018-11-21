
#' Generate variable names
#'
#' Generated variable names follow the rule "text[index]", with index from 1 to p.
#'
#' @param p number of variables
#' @param text variable name (default: 'x')
#' @return variable names as strings
#' @export
fun_var_names <- function(p, text = 'x'){
   paste0(text, '[', seq(p), ']')
}


