plot_donut <- function(data, title_text, color_palette){

  plot_data <- data %>%
    dplyr::group_by(value) %>%
    dplyr::summarize(freq = n()) %>%
    dplyr::mutate(sumfreq = sum(freq)) %>%
    dplyr::mutate(prop = scales::percent(freq / sumfreq, accuracy = 1)) %>%
    dplyr::mutate(label = paste0(value, ": ", prop)) %>%
    dplyr::mutate(spacer = 3) %>%
    dplyr::mutate(spacer = factor(spacer, c(1,2,3), c(" ", "  ", "   ")))

  # recycle colors using modulo
  col.manual <- rep(NA, length(plot_data$value))

  for (i in seq_along(col.manual)){
    names(col.manual)[i] <-as.character(plot_data$value)[i]
    col.manual[i] <- color_palette[(i - 1) %% (length(color_palette)) + 1]
  }
  
  label.scale           <- paste0(as.character(plot_data$value), ": (", plot_data$freq, "/", plot_data$sumfreq, ")")
  names(label.scale)    <- as.character(plot_data$value)

  percentage.success <- plot_data$prop[1]

  plot <- ggplot2::ggplot(plot_data, aes(x = spacer, y = freq, fill = value)) +
    ggplot2::geom_bar(width = 1, stat = "identity", position="fill", size = 0) +
    ggplot2::coord_polar("y", start=0) +
    ggplot2::scale_x_discrete(drop=FALSE) +
    ggplot2::scale_fill_manual(name = "", values = col.manual, labels = label.scale) +
    ggplot2::theme_void() +
    ggplot2::labs(tag = percentage.success) +
    ggplot2::theme(plot.tag.position = c(0.5,0.55),
          legend.position = "bottom",
          plot.tag = element_text(size = 30, face = "bold", color = col.manual),
          legend.text = element_text(size = 14)) +
    ggplot2::guides(fill = guide_legend(direction = "vertical"))  +
    ggplot2::labs(title = title_text) +
    ggplot2::theme(plot.title = element_text(color = "black", size = 16, hjust = 0.5)) +
    ggplot2::guides(fill = guide_legend(nrow = 1, 
                               title.position="top"))


  return(plot)
}
