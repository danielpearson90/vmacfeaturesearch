#' Personal Light Theme
#'
#' Personal [ggplot2] theme for manuscript figures, based heavily on the package: hrbrthemes (https://github.com/hrbrmstr/hrbrthemes)
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin plot title family, face, size and margi
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size facet label font family, face and size
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font family, face and size
#' @param axis_title_just axis title font justification, one of `[blmcrt]`
#' @param plot_margin plot margin (specify with [ggplot2::margin()])
#' @param grid_col,axis_col grid & axis colors; both default to `#cccccc`
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param axis_text_size font size of axis text
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#' @export
#' @examples theme_dp()
theme_dp <- function(base_family="Roboto Condensed",
                     base_size = 12,
                     plot_title_family = base_family,
                     plot_title_size = 18,
                     plot_title_face ="bold",
                     plot_title_margin = 10,
                     subtitle_family = base_family,
                     subtitle_size = 12,
                     subtitle_face = "plain",
                     subtitle_margin = 15,
                     strip_text_family = base_family,
                     strip_text_size = 12,
                     strip_text_face = "plain",
                     caption_family = base_family,
                     caption_size = 9,
                     caption_face = "italic",
                     caption_margin = 10,
                     axis_text_size = base_size,
                     axis_title_family = subtitle_family,
                     axis_title_size = base_size,
                     axis_title_face = "plain",
                     axis_title_just = "cc",
                     plot_margin = margin(30,30,30,30),
                     grid_col = "#cccccc", grid = FALSE,
                     axis_col = "#000000", axis = TRUE, ticks = TRUE) {



ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)

ret <- ret + theme(legend.background=element_blank())
ret <- ret + theme(legend.key=element_blank())

# This defines the grid if it is active
if (inherits(grid, "character") | grid == TRUE) {

  ret <- ret + theme(panel.grid=element_line(color=grid_col, size=0.2))
  ret <- ret + theme(panel.grid.major=element_line(color=grid_col, size=0.2))
  ret <- ret + theme(panel.grid.minor=element_line(color=grid_col, size=0.15))

  if (inherits(grid, "character")) {
    if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid.major.x=element_blank())
    if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid.major.y=element_blank())
    if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.x=element_blank())
    if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid.minor.y=element_blank())
  }

} else {
  ret <- ret + theme(panel.grid=element_blank())
}

# This defines the axes if they are activated
if (inherits(axis, "character") | axis == TRUE) {
  ret <- ret + theme(axis.line=element_line(color="#2b2b2b", size=0.15))
  if (inherits(axis, "character")) {
    axis <- tolower(axis)
    if (regexpr("x", axis)[1] < 0) {
      ret <- ret + theme(axis.line.x=element_blank())
    } else {
      ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
    }
    if (regexpr("y", axis)[1] < 0) {
      ret <- ret + theme(axis.line.y=element_blank())
    } else {
      ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
    }
  } else {
    ret <- ret + theme(axis.line.x=element_line(color=axis_col, size=0.15))
    ret <- ret + theme(axis.line.y=element_line(color=axis_col, size=0.15))
  }
} else {
  ret <- ret + theme(axis.line=element_blank())
}

# Defines the axis ticks if they are activated

if (!ticks) {
  ret <- ret + theme(axis.ticks = element_blank())
  ret <- ret + theme(axis.ticks.x = element_blank())
  ret <- ret + theme(axis.ticks.y = element_blank())
} else {
  ret <- ret + theme(axis.ticks = element_line(size=0.15))
  ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
  ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
  ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
}

# Axis title justification
xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)


ret <- ret + theme(axis.text.x=element_text(size=axis_text_size, margin=margin(t=2)))
ret <- ret + theme(axis.text.y=element_text(size=axis_text_size, margin=margin(r=2)))
ret <- ret + theme(axis.title=element_text(size=axis_title_size, family=axis_title_family))
ret <- ret + theme(axis.title.x=element_text(hjust=xj, size=axis_title_size,
                                             family=axis_title_family, face=axis_title_face, margin = margin(t=8)))
ret <- ret + theme(axis.title.y=element_text(hjust=yj, size=axis_title_size,
                                             family=axis_title_family, face=axis_title_face, margin = margin(r=8)))
ret <- ret + theme(axis.title.y.right=element_text(hjust=yj, size=axis_title_size, angle=90,
                                                   family=axis_title_family, face=axis_title_face, margin = margin(l=8)))
ret <- ret + theme(strip.text=element_text(hjust=0, size=strip_text_size,
                                           face=strip_text_face, family=strip_text_family))
ret <- ret + theme(panel.spacing=grid::unit(2, "lines"))
ret <- ret + theme(plot.title=element_text(hjust=0, size=plot_title_size,
                                           margin=margin(b=plot_title_margin),
                                           family=plot_title_family, face=plot_title_face))
ret <- ret + theme(plot.subtitle=element_text(hjust=0, size=subtitle_size,
                                              margin=margin(b=subtitle_margin),
                                              family=subtitle_family, face=subtitle_face))
ret <- ret + theme(plot.caption=element_text(hjust=1, size=caption_size,
                                             margin=margin(t=caption_margin),
                                             family=caption_family, face=caption_face))
ret <- ret + theme(plot.margin=plot_margin)

ret <- ret + theme(plot.tag = element_text(size = plot_title_size, family = plot_title_family))

ret

}
