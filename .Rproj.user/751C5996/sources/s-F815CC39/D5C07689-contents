plot_lollipop <- function(graphdata, title_text, color_palette){

  plot <- ggplot2::ggplot(graphdata, aes(x=value, y=freq)) +
    ggplot2::geom_segment( aes(x = value, xend=value, y = 0, yend=freq), color= color_palette[1]) +
    ggplot2::coord_flip() +
    ggplot2::theme_void() +
    ggplot2::theme(axis.text.y = element_text(size = 12, color = "black", hjust = 1),
          legend.position = "none",
          plot.margin = unit(c(1,1,1,1), "cm"),
          plot.title = element_text(color = "black", size = 16)) +
    ggplot2::geom_label(aes(label=freq), color = "white" , fill = color_palette[1])

  plot <- cowplot::plot_grid(plot) +
    ggplot2::ggtitle(title_text) +
    ggplot2::theme(plot.title = element_text(color = "black", size = 16))

  return(plot)
}
