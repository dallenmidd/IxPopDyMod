# testing_functions.R

#' Print parameters for a given transition
#'
#' @details
#' Print the parameters that are being grabbed through pattern matching and the
#' name of the function. Useful for debugging to determine whether correct
#' parameters are being selected for each transition.
#'
#' @param transition_row A row from the tick transitions tibble.
#' @param parameters Tick parameters tibble.
#'
#' @importFrom dplyr filter pull
#' @importFrom stringr str_detect
print_params <- function(transition_row, parameters) {

  params <- parameters %>%
    filter(str_detect(transition_row[['from']], .data$from),
           str_detect(transition_row[['to']], .data$to)) %>%
    pull(.data$param_value)

  names(params) <- parameters %>%
    filter(str_detect(transition_row[['from']], .data$from),
           str_detect(transition_row[['to']], .data$to)) %>%
    pull(.data$param_name)

  print(paste(transition_row[['transition_fun']],
              'from', transition_row[['from']],
              'to', transition_row[['to']]))
  print(params)
}

#' Print parameter names and values for each transition
#' @importFrom dplyr arrange
#' @param tick_transitions Tick transitions table
#' @param parameters Tick parameters table
#' @export
print_all_params <- function(tick_transitions, parameters) {
  funs <- tick_transitions %>%
    arrange(.data$transition_fun)

  for (i in 1:nrow(funs)) {
    print_params(funs[i,], parameters)
  }
}

# pretty printing of trans_matrix
print_trans_matrix <- function(trans_matrix) {
  print(ifelse(trans_matrix == 0, ".", trans_matrix %>% as.character() %>%
                 substr(0, 4)), quote = FALSE)
}

#' Calculate transition value for each transition
#'
#' @details
#' For use in testing. Much faster than running the whole model and useful for
#' seeing which functions are being problematic when the model breaks
#'
#' @param tick_transitions Tick transitions tibble
#' @param tick_params Tick parameters tibble
#' @param max_delay Numeric vector of length one. Determines the maximum
#'   number of days that a delayed transition can last.
#' @param host_comm Host community tibble.
#' @param weather Weather tibble.

#' @export
test_transitions <- function(tick_transitions, tick_params,
                             max_delay, host_comm, weather) {

  steps <- 300

  life_stages <- get_life_stages(tick_transitions)

  # initialize a population matrix with 10 of each tick life_stage on day 1
  N <- matrix(nrow = length(life_stages), ncol = steps, data = 0)
  N[,1] <- 10
  rownames(N) <- life_stages

  N_developing <- matrix(nrow = length(life_stages), ncol = steps, data = 0)
  rownames(N) <- life_stages

  # select which functions to test
  funs <- add_params_list(tick_transitions, tick_params)



  # loop through all the transition functions and calculate transition
  # probabilities

  # TODO could instead return/print a tibble using mutate to calculate
  # transition value as an additional column
  for (i in 1:nrow(funs)) {
    print(paste(funs[[i, 'from']], funs[[i, 'to']],
                funs[[i, 'transition_fun']]))
    print(get_transition_val(1, funs[i,], N, N_developing, max_delay,
                             life_stages, host_comm, weather))
    # transition_vals <- c(transition_vals,
    #                      get_transition_val(
    #                        1, funs[i,], N, N_developing, max_delay))
  }

  # transition_vals
}

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


