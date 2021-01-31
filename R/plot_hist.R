plot_hist <- function(graphdata, title_text, color_palette){

  plot.data <- graphdata %>%
    dplyr::mutate(value = as.numeric(value))

  plot <- ggplot2::ggplot(plot.data, aes(x = value)) +
    ggplot2::geom_histogram( fill = color_palette[1], color = color_palette[1]) +
    ggplot2::theme(axis.text.y = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 0, size = 12),
          legend.position = "none"
    )  +
    ggplot2::labs(title = title_text)  +
    ggplot2::theme(plot.title = element_text(color = "black", size = 16, hjust = 0.5))

  return(plot)
}
