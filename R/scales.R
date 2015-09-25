#' Automatically choose color scale for categorical data.
#'
#' Uses color brewer qualitative Set1 (without the yellow) for data with up to
#' 8 levels and the default ggplot2 color scale otherwise.
#'
#' @inheritParams ggplot2::scale_color_manual
#'
#' @export
#'
#' @examples
#' d <- diamonds[sample(nrow(diamonds), 5000), ]
#' p <- ggplot(d, aes(carat, price))
#'
#' # fewer than 9 levels
#' p + geom_point(aes(color = cut(price, breaks = 4))) + auto_color()
#' p + geom_point(aes(color = cut(price, breaks = 8))) + auto_color()
#'
#' # more than 8 levels
#' p + geom_point(aes(color = cut(price, breaks = 9))) + auto_color()
#' p + geom_point(aes(color = cut(price, breaks = 50))) + auto_color(guide = FALSE)
auto_color <- function(...) {
   pal <- function(n) {
      if (n < 9) {
         # color brewer qualitative "9-class Set1" without terrible yellow
         c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a65628",
            "#f781bf", "#999999")
      } else {
         # default ggplot2
         scales::hue_pal()(n)
      }
   }
   discrete_scale("colour", "manual", pal, ...)
}
