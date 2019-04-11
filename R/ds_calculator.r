#' Calculator for d<sub>s</sub> (between subjects effect size)
#'
#' Calculator for d<sub>s</sub> (between subjects effect size)
#' @param x,y vectors of the DV for each group
#' @export
#' @examples ds_calculator()

ds_calculator <- function(group1, group2) {

  mean1 <- mean(group1)
  sd1 <- sd(group1)
  n1 <- length(group1)

  mean2 <- mean(group2)
  sd2 <- sd(group2)
  n2 <- length(group2)

  ds <- (mean1 - mean2)/
    (sqrt(
        ((n1-1)*sd1^2 + (n2-1)*sd2^2)/
          (n1 + n2 - 2)))

  return(abs(ds))
}
