theme_ticks_frb <- function(){
  theme(axis.ticks.length = unit(-0.25, "cm"),
        axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.y = element_text(margin = unit(c(t = 0.5, r = 0.5, b = 0.5, l = 0.5), "cm")),
        axis.text.y.right = element_text(margin = unit(c(t = 0.5, r = 0.5, b = 0.5, l = 0.5), "cm"))
  )
}

##' FRB style y axis
##'
##' Technically this moves the y axis to the right and creates a
##' secondary axis on the other side that is identical to the first
##' (same ticks) but with empty string labels. The empty string labels
##' are important to creating the margin on the left side of the
##' chart.
##'
##' @return a ggplot object
##' @author Tom Allard
##'
frb_y <- function() {
  scale_y_continuous(sec.axis = dup_axis(labels = function(breaks) {
    rep("", length(breaks))
  }),
  position = "right")
}

##' FRB style hook box
##'
##' Geoms are used to add lines or points to a ggplot. This is a geom
##' that simply adds the hooks in the corners of the plot.
##'
##' Hadley Wickham helpfully suggested this approach, based on the
##' GeomSimplePoint example in the Extending ggplot2 guide.
##'
##' @return a ggplot geom
##' @author Scott Konzem
##'
geom_hook_box <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  
  # Derived from GeomSimplePoint at
  # https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html
  GeomHookBox <- ggproto("GeomHookBox", Geom,
                         #required_aes = c("x", "y"),
                         default_aes = aes(shape = 19, colour = "black"),
                         draw_key = draw_key_point,
                         
                         draw_panel = function(data, panel_scales, coord) {
                           coords <- coord$transform(data, panel_scales)
                           grid::segmentsGrob(
                             x0 = c(0, 0, .9, .9),
                             x1 = c(0.1, 0.1, 1, 1),
                             y0 = c(0, 1, 0, 1),
                             y1 = c(0, 1, 0, 1)
                            # gp = grid::gpar(col = coords$colour)
                           )
                         }
  )
  
  layer(
    geom = GeomHookBox, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

##' FRB color scale
##'
##' @return a ggplot object
##' @author Tom Allard
##' @export
##'
frb_colors <- function() {
  frb_palette <- c(
    "#00437C",
    "#BD530A",
    "#4F7F0A",
    "#663F74",
    "#0F6FB7",
    "#993333",
    "#367B87",
    "#565656"
  )
  ggplot2::scale_colour_manual(values = frb_palette)
}

##' FRB charting theme
##'
##' @param legend.position legend position
##' @param legend.justification legend justification
##' @return a gglot object
##' @author Tom Allard
##' @export
##'
theme_frb <- function(legend.position = c(.05, .85),
                      legend.justification = 0) {
  theme_stuff <- theme_classic() +
    theme(axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme(panel.grid = element_blank(),
          panel.border = element_blank()) +
    theme_ticks_frb() +
    theme(legend.title = element_blank(),
          legend.justification = legend.justification,
          legend.position = legend.position,
          legend.background = element_rect(fill = alpha("white", .1)))
  
  return(list(geom_vline(xintercept = Inf),  # Right y-axis line
              geom_vline(xintercept = -Inf), # Left y-axis line
              frb_y(),
              theme_stuff,
              geom_hook_box(),
              frb_colors(),
              xlab(NULL), ylab(NULL)))
  
}
theme_frb_2axis <- function(legend.position = c(.05, .85),
                      legend.justification = 0) {
  theme_stuff <- theme_classic() +
    theme(
      axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme(panel.grid = element_blank(),
          panel.border = element_blank()) +
    theme_ticks_frb() +
    theme(legend.title = element_blank(),
          legend.justification = legend.justification,
          legend.position = legend.position,
          legend.background = element_rect(fill = alpha("white", .1)))
  
  return(list(geom_vline(xintercept = Inf),  # Right y-axis line
              geom_vline(xintercept = -Inf), # Left y-axis line
              #frb_y(),
              theme_stuff,
              geom_hook_box(),
              frb_colors(),
              xlab(NULL), ylab(NULL)))
  
}
