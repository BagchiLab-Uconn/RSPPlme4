#' Format fixed effect parameters from a klmerHyper object for easy plotting

#' @param x A \code{\link{klmerHyper}} object.

#' @return a \code{\link[tibble]{tibble}} of fixed effects against distance.

#' @family RSPP plot functions
#'
#' @importFrom dplyr rename mutate
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @export

makePlotData_klmerHyper <- function(x)
{
  tibble::as_tibble(
    do.call('rbind', lapply(x, fixef))
  ) %>% 
    dplyr::rename("Intercept" = "(Intercept)") %>%
    dplyr::mutate(distance = as.numeric(names(x))) %>%
    tidyr::pivot_longer(cols = - .data$distance, 
                        names_to="term", values_to="estimate", 
                        names_transform=list(term = factor)) %>%
    mutate(term = relevel(.data$term, "Intercept"))
}
