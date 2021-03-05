#' nam218grads
#'
#' @description Provides opendap access to the NOMADS/NCEP grads server for NAM218 forecast data.
#' @docType package
#' @name nam218grads
#' @importFrom dplyr %>%
#' @importFrom rlang .data := !!
NULL

load_packages <- function(packages = c("ncdf4", "dplyr", "sf", "stars")){
  for (p in packages){require(p, character.only = TRUE)}
}
