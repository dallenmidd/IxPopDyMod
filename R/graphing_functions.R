#' Graph population size of each life stage over time
#'
#' @param out_N_df Tibble of population size per life stage per day
#' @param title Optional title to display on graph
#' @importFrom ggplot2 ggplot aes geom_point scale_size_manual
#'   scale_shape_manual geom_line scale_y_log10 geom_hline ggtitle
#' @return ggplot object
#' @export
graph_population_each_group <- function(out_N_df, title=NULL) {
  ggplot(out_N_df, aes(x = .data$day, y = .data$pop, color = .data$process,
                       shape = .data$age_group, group = .data$stage)) +
    geom_point(aes(size = infected)) +
    scale_size_manual(values = c(1, 3)) +
    scale_shape_manual(values = c(15:18)) +
    geom_line() +
    scale_y_log10(limits = c(-1, 1e+05), breaks = c(10, 100, 1000, 10000, 100000, 1000000)) +
    geom_hline(yintercept = 1000) +
    ggtitle(title)
}

#' Graph overall trend in population
#'
#' @details
#' See roughly whether population is increasing or decreasing.
#'
#' @param out_N_df Tibble of population size per life stage per day
#' @param title Optional title to display on graph
#'
#' @importFrom ggplot2 ggplot aes geom_path ylim ggtitle
#' @importFrom dplyr filter group_by summarise mutate lag
#'
#' @return ggplot object
#' @export
graph_population_overall_trend <- function(out_N_df, title=NULL) {
  out_N_df %>%
    filter(.data$age_group == 'a') %>%
    group_by(.data$day) %>%
    summarise(tot = sum(.data$pop)) %>%
    mutate(lambda = .data$tot/lag(.data$tot)) %>%
    filter(is.finite(.data$lambda) ) %>%
    ggplot(aes(.data$day, .data$lambda)) +
    geom_path() +
    ylim(0, 2) +
    ggtitle(title)
}


#' Calculate multiplicative growth rate of population
#'
#' @importFrom dplyr group_by summarise filter mutate
#'
#' @param out Model output data frame
#' @return Numeric vector of length one representing growth rate
#' @export
growth_rate <- function(out) {
  out %>%
    group_by(.data$day) %>%
    summarise(tot = sum(.data$pop)) %>%
    mutate(lambda = .data$tot/lag(.data$tot)) %>%
    filter(is.finite(.data$lambda), .data$lambda > 0) %>%
    summarise(lambda = exp(mean(log(.data$lambda)))) %>%
    as.numeric()
}


