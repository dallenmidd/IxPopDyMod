#' Graph population size of each life stage over time
#'
#' @param output Model output; a tibble
#' @importFrom ggplot2 ggplot aes geom_point scale_size_manual
#'   scale_shape_manual geom_line scale_y_log10 geom_hline ylab xlab labs
#' @return ggplot object
#'
#' @examples
#' out <- run(config_ex_1)
#' graph_population_each_group(out)
#'
#' @export
graph_population_each_group <- function(output) {
  p <- output %>%
    ggplot(
      aes(
        x = .data$day,
        y = .data$pop,
        group = .data$stage
      )
    ) +
    scale_y_log10() +
    geom_line() +
    geom_point() +
    ylab("Population") +
    xlab("Julian day")

  if (length(unique(output$age_group)) > 1) {
    p <- p +
      aes(shape = .data$age_group) +
      labs(shape = "Age group")
  }

  if (length(unique(output$infected)) == 2) {
    # 2 unique values: infected or uninfected
    p <- p +
      aes(size = .data$infected) +
      scale_size_manual(1, 3) +
      labs(size = "Infected")
  }

  if (length(unique(output$process)) > 1) {
    p <- p +
      aes(color = .data$process) +
      labs(color = "Process")
  }

  p
}

#' Graph overall trend in population
#'
#' @details
#' See roughly whether population is increasing or decreasing. Calculates and
#' plots the rate of change in number of adult ticks between consecutive days.
#'
#' @inheritParams graph_population_each_group
#'
#' @importFrom ggplot2 ggplot aes geom_path ylim ggtitle
#' @importFrom dplyr filter group_by summarise mutate lag
#'
#' @return ggplot object
#'
#' @examples
#'
#' # Make a new config that results in a population where some ticks remain
#' # in their life stage for multiple days.
#' my_config <- config_ex_1
#' my_config$parameters$param_value <- c(0.5, 0, 0.01, 0.95, 0.1, 0.8, 900, 0)
#' out <- run(my_config)
#' graph_population_overall_trend(out)
#'
#' @export
graph_population_overall_trend <- function(output) {
  output %>%
    filter(.data$age_group == "a") %>%
    group_by(.data$day, .add = TRUE) %>%
    summarise(tot = sum(.data$pop), .groups = "keep") %>%
    mutate(lambda = .data$tot / lag(.data$tot)) %>%
    filter(is.finite(.data$lambda)) %>%
    ggplot(aes(.data$day, .data$lambda)) +
    geom_path() +
    ylim(0, 2)
}


#' Calculate multiplicative growth rate of population
#'
#' @importFrom dplyr group_by summarise filter mutate
#'
#' @param out Model output data frame
#' @return Numeric vector of length one representing growth rate
#'
#' @examples
#' out <- run(config_ex_1)
#' growth_rate(out)
#'
#' @export
growth_rate <- function(out) {
  out %>%
    group_by(.data$day) %>%
    summarise(tot = sum(.data$pop)) %>%
    mutate(lambda = .data$tot / lag(.data$tot)) %>%
    filter(is.finite(.data$lambda), .data$lambda > 0) %>%
    summarise(lambda = exp(mean(log(.data$lambda)))) %>%
    as.numeric()
}
