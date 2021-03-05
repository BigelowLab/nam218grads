#' Bind a list of \code{stars} objects.
#'
#' @seealso \code{\link[stars]{c.stars}}
#'
#' @export
#' @param x list of one or more \code{stars} objects
#' @param nms character, vector of names to apply to attributes
#' @param ... arguments for \code{\link[stars]{c.stars}}
#' @return \code{stars} objects
bind_stars <- function(x, nms = names(x), ...){
  y <- do.call(c, x)
  if (is.null(nms)) nms <- paste("b", seq_len(length(x)))
  y <- stats::setNames(y, nms)
  y
}
