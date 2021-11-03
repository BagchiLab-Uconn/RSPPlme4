#' Transformation from K to L functions.
#' @import scales
#'
#' @examples
#' plot(sqrt_trans(), xlim = c(0, 5))
#' @export


L_trans <- function() {
  trans_new(
    "L",
    trans = function(x) sqrt(x/pi),
    inverse = function(x) pi *x ^ 2,
    domain = c(0, Inf)
  )
}
