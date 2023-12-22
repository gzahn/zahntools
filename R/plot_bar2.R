#' A flexible, informative barplot phyloseq data ... now without annoying outlines!
#'
#' There are many useful examples of phyloseq barplot graphics in the
#' \href{http://joey711.github.io/phyloseq/plot_bar-examples}{phyloseq online tutorials}.
#' This function wraps \code{ggplot2} plotting, and returns a \code{ggplot2}
#'  graphic object
#' that can be saved or further modified with additional layers, options, etc.
#' The main purpose of this function is to quickly and easily create informative
#' summary graphics of the differences in taxa abundance between samples in
#' an experiment.
#'
#' @usage plot_bar2(physeq, x="Sample", y="Abundance", fill=NULL,
#'  title=NULL, facet_grid=NULL)
#'
#' @param physeq (Required). An \code{\link{otu_table-class}} or
#'  \code{\link{phyloseq-class}}.
#'
#' @param x (Optional). Optional, but recommended, especially if your data
#'  is comprised of many samples. A character string.
#'  The variable in the melted-data that should be mapped to the x-axis.
#'  See \code{\link{psmelt}}, \code{\link{melt}},
#'  and \code{\link{ggplot}} for more details.
#'
#' @param y (Optional). A character string.
#'  The variable in the melted-data that should be mapped to the y-axis.
#'  Typically this will be \code{"Abundance"}, in order to
#'  quantitatively display the abundance values for each OTU/group.
#'  However, alternative variables could be used instead,
#'  producing a very different, though possibly still informative, plot.
#'  See \code{\link{psmelt}}, \code{\link{melt}},
#'  and \code{\link{ggplot}} for more details.
#'
#' @param fill (Optional). A character string. Indicates which sample variable
#'  should be used to map to the fill color of the bars.
#'  The default is \code{NULL}, resulting in a gray fill for all bar segments.
#'
#' @param facet_grid (Optional). A formula object.
#'  It should describe the faceting you want in exactly the same way as for
#'  \code{\link[ggplot2]{facet_grid}},
#'  and is ulitmately provided to \code{\link{ggplot}}2 graphics.
#'  The default is: \code{NULL}, resulting in no faceting.
#'
#' @param title (Optional). Default \code{NULL}. Character string.
#'  The main title for the graphic.
#'
#' @return A \code{\link[ggplot2]{ggplot}}2 graphic object -- rendered in the graphical device
#'  as the default \code{\link[base]{print}}/\code{\link[methods]{show}} method.
#'
#' @seealso
#'  \href{http://joey711.github.io/phyloseq/plot_bar-examples}{phyloseq online tutorials}.
#'
#'  \code{\link{psmelt}}
#'
#'  \code{\link{ggplot}}
#'
#'  \code{\link{qplot}}
#'
#'
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 element_text
#'
#' @export
#'
#' @examples
#' data("GlobalPatterns")
#' gp.ch = subset_taxa(GlobalPatterns, Phylum == "Chlamydiae")
#' plot_bar2(gp.ch)
#' plot_bar2(gp.ch, fill="Genus")
#' plot_bar2(gp.ch, x="SampleType", fill="Genus")
#' plot_bar2(gp.ch, "SampleType", fill="Genus", facet_grid=~Family)
#' # See additional examples in the plot_bar online tutorial. Link above.


plot_bar2 <- function (physeq, x = "Sample", y = "Abundance", fill = NULL,
          title = NULL, facet_grid = NULL,width = 0.9)
{
  mdf = psmelt(physeq)
  p = ggplot(mdf, aes_string(x = x, y = y, fill = fill))
  p = p + geom_bar(stat = "identity", position = "stack",width = width)
  p = p + theme(axis.text.x = element_text(angle = -90, hjust = 0))
  if (!is.null(facet_grid)) {
    p <- p + facet_grid(facet_grid)
  }
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  return(p)
}


