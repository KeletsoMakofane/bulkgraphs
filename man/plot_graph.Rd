% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_graph.R
\name{plot_graph}
\alias{plot_graph}
\title{Plot Graph}
\usage{
plot_graph(data, plot_var, plot_type, mapping, title_text, color_palette)
}
\arguments{
\item{data}{Long data.frame or tiblle with columns: indicator, value}

\item{plot_var}{Variable to be plotted}

\item{plot_type}{Type of graph "bar", "donut", "hist", "hist.time", "lollipop"}

\item{mapping}{Mapping from values to display labels. Long data.frame or tiblle with columns: indicator, oldlabel, newlabel, levelorder}

\item{title_text}{Title of graph}

\item{color_palette}{Three-color color palette for graph}
}
\value{

}
\description{
Plot Graph
}
