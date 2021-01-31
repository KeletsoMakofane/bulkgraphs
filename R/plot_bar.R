plot_bar <- function(graphdata, title_text, color_palette){

  plot <- ggplot2::ggplot(graphdata, aes(x = value)) +
    ggplot2::geom_bar(fill = color_palette[1]) +
    ggplot2::scale_fill_brewer(drop = FALSE) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::theme(axis.text.y = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 0, size = 12),
          legend.position = "none"
    )  +
    ggplot2::geom_label(
      aes(label=stat(count)),
      stat='count',
      nudge_y= 1,
      inherit.aes = TRUE
    ) +
    ggplot2::labs(title = title_text)  +
    ggplot2::theme(plot.title = element_text(color = "black", size = 16, hjust = 0.5),
          axis.text.x = element_text(angle = 90))

  return(plot)
}
