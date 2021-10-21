#' Format fixed effect parameters from a klmer object for easy plotting

#' @param x A \code{\link{klmer}} object.

#' @return a \code{\link[tibble]{tibble}} of fixed effects against distance.

#' @family RSPP plot functions
#'
#' @importFrom dplyr rename mutate
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export

makePlotData_klmer <- function(x)
{
  tibble::as_tibble(
    cbind(distance = as.numeric(names(x)), do.call('rbind', lapply(x, fixef)))
  ) %>% 
    dplyr::rename("Intercept" = "(Intercept)") %>%
    tidyr::pivot_longer(cols = - .data$distance, 
                        names_to="term", values_to="estimate", 
                        names_transform=list(term = factor)) %>%
    dplyr::mutate(term = relevel(.data$term, "Intercept"))
}
