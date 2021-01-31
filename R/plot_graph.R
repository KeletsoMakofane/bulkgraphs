#' Plot Graph
#'
#' @param data Long data.frame or tiblle with columns: indicator, value
#' @param plot_var Variable to be plotted
#' @param plot_type Type of graph "bar", "donut", "hist", "hist.time", "lollipop"
#' @param mapping Mapping from values to display labels. Long data.frame or tiblle with columns: indicator, oldlabel, newlabel, levelorder
#' @param title_text Title of graph
#' @param color_palette Three-color color palette for graph
#'
#' @return
#' @export
#'
#' @examples
plot_graph <- function(data, plot_var, plot_type, mapping, title_text, color_palette){
  graphmapping <- mapping %>%
    dplyr::filter(indicator == plot_var) %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::select(-indicator) %>%
    group_by(oldlabel, newlabel) %>%
    summarize(levelorder = last(levelorder))

    data <- data %>%
      dplyr::filter(indicator == plot_var)

  if (plot_type == "lollipop"){
    graphdata <- data %>%
      dplyr::mutate(value = as.character(value)) %>%
      {.$value} %>%
      str_split_fixed(string = ., pattern = "\ ", n = 100) %>%
      data.frame() %>%
      tidyr::pivot_longer(cols = 1:100) %>%
      dplyr::filter(value != "") %>%
      dplyr::group_by(value) %>%
      dplyr::summarize(freq = n()) %>%
      dplyr::arrange(freq) %>%
      dplyr::left_join(graphmapping, by = c("value" = "oldlabel")) %>%
      dplyr::filter(!is.na(newlabel)) %>%
      dplyr::filter(newlabel != "") %>%
      dplyr::mutate(value = factor(newlabel, unique(newlabel)))
  }
  
  if (plot_type %in% c("bar", "donut")){
    graphdata <- data %>%
      dplyr::mutate(value = as.character(value)) %>%
      dplyr::left_join(graphmapping, by = c("value" = "oldlabel")) %>%
      dplyr::filter(!is.na(newlabel)) %>%
      dplyr::filter(newlabel != "") %>%
      dplyr::arrange(levelorder) %>%
      dplyr::mutate(value = factor(newlabel, unique(newlabel)))
  } 

  plot <- switch(plot_type,
                 "bar"         = plot_bar(         graphdata, title_text, color_palette),
                 "donut"       = plot_donut(       graphdata, title_text, color_palette),
                 "hist"        = plot_hist(        graphdata, title_text, color_palette),
                 "hist.time"   = plot_hist_time(   graphdata, title_text, color_palette),
                 "lollipop"    = plot_lollipop(    graphdata, title_text, color_palette)
                 )
  return(plot)
}
