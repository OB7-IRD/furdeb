#' @name ggplot_corr
#' @title Correlogram function
#' @description Correlogram function for both large/small sample size. This function if based on the working development of Kevin Liu. If you want to know, take a look on his website https://rh8liuqy.github.io/ACF_PACF_by_ggplot2.html.
#' @param data Time series object, {\link[base]{numeric}} or {\link[base]{matrix}} expected. A univariate or multivariate (not ccf) numeric time series object or a numeric vector or matrix.
#' @param lag_max {\link[base]{integer}} expected. Maximum lag at which to calculate the acf. By default 24.
#' @param ci {\link[base]{numeric}} expected. Value for the confidence interval. By default 0.95.
#' @param large_sample_size {\link[base]{logical}} expected. Specify here if you have a large sample. By default FALSE.
#' @return Return a list with two elements who contains plots, in ggplot format, of correlation and partial correlation.
#' @export
#' @importFrom dplyr lag select
#' @importFrom ggplot2 ggplot aes geom_area geom_col scale_x_continuous scale_y_continuous element_blank ggtitle theme_bw
ggplot_corr <- function(data,
                        lag_max = as.integer(24),
                        ci = as.numeric(0.95),
                        large_sample_size = FALSE,
                        ...) {
  list_acf <- acf(data,
                  lag.max = lag_max,
                  type = "correlation",
                  plot = FALSE)
  N <- as.numeric(list_acf$n.used)
  df1 <- data.frame(lag = list_acf$lag,
                    acf = list_acf$acf)
  df1$lag_acf <- dplyr::lag(df1$acf,
                            default = 0)
  df1$lag_acf[2] <- 0
  df1$lag_acf_cumsum <- cumsum((df1$lag_acf)^2)
  df1$acfstd <- sqrt(1/N * (1 + 2 * df1$lag_acf_cumsum))
  df1$acfstd[1] <- 0
  df1 <- dplyr::select(df1,
                       lag,
                       acf,
                       acfstd)
  list_pacf <- acf(data,
                   lag.max = lag_max,
                   type = "partial",
                   plot = FALSE)
  df2 <- data.frame(lag = list_pacf$lag,
                    pacf = list_pacf$acf)
  df2$pacfstd <- sqrt(1 / N)
  if (large_sample_size == TRUE) {
    plot_acf <- ggplot2::ggplot(data = df1,
                                ggplot2::aes(x = lag,
                                             y = acf)) +
      ggplot2::geom_area(ggplot2::aes(x = lag,
                                      y = qnorm((1 + ci) / 2) * acfstd),
                         fill = "#B9CFE7") +
      ggplot2::geom_area(ggplot2::aes(x = lag,
                                      y = -qnorm((1 + ci) / 2) * acfstd),
                         fill = "#B9CFE7") +
      ggplot2::geom_col(fill = "#4373B6",
                        width = 0.7) +
      ggplot2::scale_x_continuous(breaks = seq(0, max(df1$lag), 6)) +
      ggplot2::scale_y_continuous(name = ggplot2::element_blank(),
                                  limits = c(min(df1$acf,
                                                 df2$pacf),
                                             1)) +
      ggplot2::ggtitle("ACF") +
      ggplot2::theme_bw()
    plot_pacf <- ggplot2::ggplot(data = df2,
                                 ggplot2::aes(x = lag,
                                              y = pacf)) +
      ggplot2::geom_area(ggplot2::aes(x = lag,
                                      y = qnorm((1 + ci) / 2)* pacfstd),
                         fill = "#B9CFE7") +
      ggplot2::geom_area(ggplot2::aes(x = lag,
                                      y = - qnorm((1 + ci) / 2) * pacfstd),
                         fill = "#B9CFE7") +
      ggplot2::geom_col(fill = "#4373B6",
                        width = 0.7) +
      ggplot2::scale_x_continuous(breaks = seq(0, max(df2$lag,
                                                      na.rm = TRUE),
                                               6)) +
      ggplot2::scale_y_continuous(name = ggplot2::element_blank(),
                                  limits = c(min(df1$acf,
                                                 df2$pacf),
                                             1)) +
      ggplot2::ggtitle("PACF") +
      ggplot2::theme_bw()
  } else {
    plot_acf <- ggplot2::ggplot(data = df1,
                                ggplot2::aes(x = lag,
                                             y = acf)) +
      ggplot2::geom_col(fill = "#4373B6",
                        width = 0.7) +
      ggplot2::geom_hline(yintercept = qnorm((1 + ci) / 2) / sqrt(N),
                          colour = "sandybrown",
                          linetype = "dashed") +
      ggplot2::geom_hline(yintercept = - qnorm((1 + ci) / 2) / sqrt(N),
                          colour = "sandybrown",
                          linetype = "dashed") +
      ggplot2::scale_x_continuous(breaks = seq(0,
                                               max(df1$lag),
                                               6)) +
      ggplot2::scale_y_continuous(name = ggplot2::element_blank(),
                                  limits = c(min(df1$acf,
                                                 df2$pacf),
                                             1)) +
      ggplot2::ggtitle("ACF") +
      ggplot2::theme_bw()
    plot_pacf <- ggplot2::ggplot(data = df2,
                                 ggplot2::aes(x = lag,
                                              y = pacf)) +
      ggplot2::geom_col(fill = "#4373B6",
                        width = 0.7) +
      ggplot2::geom_hline(yintercept = qnorm((1 + ci) / 2) / sqrt(N),
                          colour = "sandybrown",
                          linetype = "dashed") +
      ggplot2::geom_hline(yintercept = - qnorm((1 + ci) / 2) / sqrt(N),
                          colour = "sandybrown",
                          linetype = "dashed") +
      ggplot2::scale_x_continuous(breaks = seq(0,
                                               max(df2$lag,
                                                   na.rm = TRUE),
                                               6)) +
      ggplot2::scale_y_continuous(name = ggplot2::element_blank(),
                                  limits = c(min(df1$acf,
                                                 df2$pacf),
                                             1)) +
      ggplot2::ggtitle("PACF") +
      ggplot2::theme_bw()
  }
  plots_acf_pacf <- list("acf" = plot_acf,
                         "pacf" = plot_pacf)
  return(plots_acf_pacf)
}
