plot_hist_time <- function(graphdata, title_text, color_palette){

  plot_data <- graphdata %>%
    dplyr::mutate(value = substr(value, 1, 8)) %>%
    dplyr::mutate(value = as.POSIXct(value, format = "%H:%M:%S"))

  plot <- ggplot2::ggplot(plot_data, aes(x = value)) +
    ggplot2::geom_histogram(fill = color_palette[1], color = color_palette[1]) + #bw = 60*30,
    ggplot2::scale_x_datetime(date_labels = "%H:%M", breaks = as.POSIXct(c("02:00","04:00" ,"06:00", "08:00", "10:00", "12:00", "14:00","16:00", "18:00"), format = "%H:%M")) +
    ggplot2::theme(axis.text.y = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, size = 12),
          legend.position = "none"
    )  +
    ggplot2::labs(title = title_text)  +
    ggplot2::theme(plot.title = element_text(color = "black", size = 16, hjust = 0.5))

  return(plot)
}
