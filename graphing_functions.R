# graphing functions

graph_population_each_group <- function(out_N_df) {
  ggplot(out_N_df, aes(x = day, y = pop, color = process, shape = age_group, group = stage)) + 
    geom_point(aes(size = infected)) +
    scale_size_manual(values = c(1, 3)) + 
    scale_shape_manual(values = c(15:18)) + 
    geom_line() + 
    scale_y_log10(limits = c(-1, 1e+05), breaks = c(10, 100, 1000, 10000, 100000, 1000000)) +
    geom_hline(yintercept = 1000)
}

# just see roughly whether population is increasing or decreasing
graph_population_overall_trend <- function(out_N_df) {
  out_N_df %>% 
    filter(age_group == 'a') %>% 
    group_by(day) %>% 
    summarise(tot = sum(pop)) %>% 
    mutate(lambda = tot/lag(tot)) %>%
    filter(is.finite(lambda) ) %>% 
    ggplot(aes(day,lambda)) + 
    geom_path() + 
    ylim(0, 2)
}
