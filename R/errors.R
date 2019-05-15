


#' Constructor for custom condition class
#' Constructor for custom condition class.
#'
#' @param subclass the subclass for the condition
#' @param ... other classes
#' @inheritParams base::simpleCondition
#' @keywords internal
condition <- function(subclass, message, call = sys.call(-1), ...) {
   structure(
      class = c(subclass, "condition"),
      list(message = message, call = call),
      ...
   )
}

#' Raise a FATAL error
#'
#' Raise a FATAL error. Useful in testing.
#' These errors should never happen.
#'
#' @inheritParams condition
#' @keywords internal
stop_fatal <- function(message, call = sys.call(-1), ...) {
   c <- condition(c('fatal', "error"), paste('[FATAL]', message), call = call, ...)
   stop(c)
}


#' Raise a SERIOUS error
#'
#' Raise a SERIOUS error.
#' These errors might be expected.
#'
#' @inheritParams condition
#' @keywords internal
stop_serious <- function(message, call = sys.call(-1), ...) {
   c <- condition(c('serious', "error"), paste('[Error]', message), call = call, ...)
   stop(c)
}


