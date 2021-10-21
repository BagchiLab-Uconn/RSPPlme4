#' Format fixed effect parameters from a klm object for easy plotting

#' @param x A \code{\link{klm}} object.

#' @return a \code{\link[tibble]{tibble}} of coefficients against distance.

#' @family RSPP plot functions
#'
#' @importFrom dplyr rename mutate
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data

makePlotData_klm <- function(x)
{
  dists <- as.numeric(names(x))
  tibble::as_tibble(
    do.call('rbind', lapply(x, coef))
  ) %>% 
    dplyr::rename("Intercept" = "(Intercept)") %>%
    dplyr::mutate(distance = dists) %>% 
    tidyr::pivot_longer(cols = - .data$distance, 
                        names_to="term", values_to="estimate", 
                        names_transform=list(term = factor)) %>%
    mutate(term = relevel(.data$term, "Intercept"))
}
