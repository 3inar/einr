#' Theme for ggplot
#'
#' This is at the moment a small fix to ggthemes' theme_par(). The only change is
#' that legends take their font size from the cex parameter.
#'
#' @export
theme_einr <- function (base_size = par()$ps, base_family = par()$family)
{
  faces <- c("plain", "bold", "italic", "bold.italic")
  half_line <- base_size/2
  thm <- theme_foundation() %+replace%

    theme(
      line = element_line(colour = par()$fg, size = 0.5, lineend = par()$lend, linetype = par()$lty),
      rect = element_rect(fill = par()$bg, colour = par()$fg,  size = 0.5, linetype = par()$lty),
      text = element_text(colour = par()$fg,
                          face = faces[par()$font],
                          family = base_family,
                          size = base_size,
                          angle = 0,
                          margin = margin(),
                          vjust = par()$adj,
                          hjust = par()$adj,
                          lineheight = par()$lheight,
                          debug = FALSE),
      axis.title = element_text(size = rel(par()$cex.lab),  colour = par()$col.lab, face = faces[par()$font.lab]),
      axis.text = element_text(size = rel(par()$cex.axis), colour = par()$col.axis, face = faces[par()$font.axis]),
      axis.text.x = element_text(margin = margin(t = 0.8 *  half_line/2, b = 0.8 * half_line/2)),
      axis.text.y = element_text(margin = margin(r = 0.8 *  half_line/2, l = 0.8 * half_line/2)),
      axis.ticks = element_line(colour = par()$fg),
      legend.title = element_text(colour = par()$fg, size=rel(par()$cex)),
      legend.text = element_text(colour = par()$fg, size=rel(par()$cex)),
      legend.spacing = unit(0.2, "cm"),
      legend.key = element_rect(colour = NA),
      panel.spacing = unit(half_line, "pt"),
      panel.spacing.x = NULL,
      panel.spacing.y = NULL,
      panel.background = element_rect(fill = NA, colour = par()$col),
      panel.grid = element_blank(),
      plot.background = element_rect(colour = NA),
      plot.margin = unit(par()$mar, "lines"),
      plot.title = element_text(size = rel(par()$cex.main), face = faces[par()$font.main],  colour = par()$col.main, margin = margin(b = half_line * 1.2)),
      strip.text = element_text(size = rel(par()$cex.sub), face = faces[par()$font.sub], colour = par()$col.sub),
      strip.text.x = element_text(margin = margin(t = half_line,  b = half_line)),
      strip.text.y = element_text(margin = margin(l = half_line, r = half_line)), strip.background = element_rect(colour = NA))

  las <- par()$las
  if (las == 0) {
    thm <- thm + theme(axis.title.x = element_text(angle = 0),
                       axis.title.y = element_text(angle = 90))
  }
  else if (las == 1) {
    thm <- thm + theme(axis.title.x = element_text(angle = 0),
                       axis.title.y = element_text(angle = 0))
  }
  else if (las == 2) {
    thm <- thm + theme(axis.title.x = element_text(angle = 90),
                       axis.title.y = element_text(angle = 0))
  }
  else if (las == 3) {
    thm <- thm + theme(axis.title.x = element_text(angle = 90),
                       axis.title.y = element_text(angle = 90))
  }
  if (!is.na(par()$tck)) {
    thm <- thm + theme(axis.ticks.length = unit(-par()$tck,
                                                "snpc"))
  }
  else {
    thm <- thm + theme(axis.ticks.length = unit(-par()$tcl,
                                                "lines"))
  }
  if (par()$xaxt == "n") {
    thm <- thm + theme(axis.line.x = element_blank(), axis.text.x = element_blank(),
                       axis.ticks.x = element_blank())
  }
  if (par()$yaxt == "n") {
    thm <- thm + theme(axis.line.y = element_blank(), axis.text.y = element_blank(),
                       axis.ticks.y = element_blank())
  }
  thm
}