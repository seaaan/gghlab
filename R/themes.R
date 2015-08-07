#' Tilt x-axis labels.
#'
#' @param angle The angle text should be displayed at (0 is horizontal, 90 is
#' vertical)
#' @param hjust horizontal justification (0 to 1)
#' @param vjust vertical justification (0 to 1)
#'
#' @export
#'
#' @examples
#' mtcars$make <- sapply(rownames(mtcars), function(x) strsplit(x, " ")[[1]][1])
#' p <- ggplot(mtcars, aes(make, wt)) + geom_point()
#' p
#' p + tilt_x_labels()
tilt_x_labels <- function(angle = 45, hjust = 1, vjust = 1) {
   # sensible default for 90 degrees
   if (angle == 90 && missing(vjust)) {
      vjust <- 0.25
   }
   ggplot2::theme(axis.text.x =
         ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust))
}

#' Publication-ready theme.
#'
#' Provides sensible defaults for graphs intended for publication, including
#' using a white background, setting appropriate font sizes, removing white
#' space, and adjusting line widths.
#'
#' @export
#'
#' @examples
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p
#' p + theme_pub()
theme_pub <- function(trim_white_space = FALSE) {
   lineSize <- function() 0.5
   thinLineSize <- function() 0.25

   result <- ggplot2::theme(
      # backgrounds
      strip.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),

      # lines
      axis.line = ggplot2::element_line(color = "grey50", size = lineSize()),
      panel.grid.major = ggplot2::element_line(color = "grey90", size = thinLineSize()),
      panel.grid.minor = ggplot2::element_blank(),

      # fonts
      title = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(size = 10, face = "plain"),
      legend.text = ggplot2::element_text(size = 8, color = "grey50"),
      axis.text = ggplot2::element_text(size = 8)
   )
   if (trim_white_space) {
      return(result + theme(plot.margin = grid::unit(c(0, 0, 0, 0), "mm")))
   } else {
      return(result)
   }
}
