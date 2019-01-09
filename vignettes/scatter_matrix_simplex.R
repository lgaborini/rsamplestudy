# Scatter matrix plot for data on a simplex
#

library(ggplot2)
requireNamespace('GGally')

scatter_plot_01 <- function(data, mapping, ...) {
   ggplot(data = data, mapping = mapping) +
      geom_point(...) +
      scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
      scale_y_continuous(limits = c(0,1), expand = c(0,0))
}

scatter_matrix_simplex <- function(df) {
   GGally::ggpairs(df, upper = list(continuous = scatter_plot_01), diag = 'blank', lower = 'blank', labeller = label_parsed) +
   theme_bw()
}
