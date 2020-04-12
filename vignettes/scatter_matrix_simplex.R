# Scatter matrix plot for data on a simplex
#

requireNamespace('ggplot2')
requireNamespace('GGally')

scatter_plot_01 <- function(data, mapping, ...) {
   ggplot2::ggplot(data = data, mapping = mapping) +
      ggplot2::geom_point(...) +
      ggplot2::scale_x_continuous(limits = c(0,1), expand = c(0,0)) +
      ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0,0))
}

scatter_matrix_simplex <- function(df) {
   GGally::ggpairs(
      df,
      upper = list(continuous = scatter_plot_01),
      diag = 'blank',
      lower = 'blank',
      labeller = ggplot2::label_parsed
   ) +
   ggplot2::theme_bw()
}
