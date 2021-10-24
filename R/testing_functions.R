#' Check that transitions form a cycle
#'
#' @details
#' Tests to ensure that there are no "dead-ends" in the life cycle by ensuring
#' that all non-mortality life stages occur in both the `from` and `to` fields
#' of transitions.
#'
#' Optionally, also outputs a graph of the life cycle for visually confirming
#' that transitions are as intended.
#'
#' @param life_stages Character vector of life stages.
#' @param tick_transitions Tick transitions tibble
#' @param graph Default `TRUE`, if `FALSE`, graph is not created.
#'
#' @importFrom dplyr pull filter select
#' @importFrom igraph graph_from_data_frame
#' @export
test_lifecycles <- function(life_stages, tick_transitions, graph = TRUE) {
  # check if all life_stages are in from and to in tick_transitions
  all_from <- tick_transitions %>% pull(.data$from) %>% unique() %>% sort()
  all_to <- tick_transitions %>%
    filter(.data$to %in% life_stages) %>%
    pull(.data$to) %>% unique() %>% sort()
  if (!all(all_from == all_to)) {
    stop('from and to stages in tick_transitions do not match')
  } else {
    print('all stages represented in from and to in transitions')
  }

  # think of all transitions as edges between two nodes (from, to)
  # check if the graph has a cycle (trail to itself) from each life stage
  # i.e., for each life stage, can I get back to that life_stage
  # TODO for now, just inspecting this visually
  if (graph) {
    g <- graph_from_data_frame(
      tick_transitions %>%
        select(.data$from, .data$to) %>%
        filter(.data$to %in% life_stages))

    plot(g)
  }
}


