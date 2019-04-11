#' Qualitative colour palette for standard VMAC colours
#'
#' Qualitative colour palette for standard VMAC colours
#' @param high,low,absent,other TRUE/FALSE to pull colour for each trial type
#' @export
#' @examples vmac_pal()

vmac_pal <- function(high = TRUE, low = TRUE, absent = TRUE, other = FALSE) {
  pal <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
           "#CC79A7", "#000000", "#999999")

  if (high) {a = 1} else {a = NA}
  if (low) {b = 2} else {b = NA}
  if (absent) {c = 9} else {c = NA}
  if (other) {d = 3} else {d = NA}

  pal <- pal[c(a,b,d,c)]
  pal <- pal[!is.na(pal)]
  pal
}
