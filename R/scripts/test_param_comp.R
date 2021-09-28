# Ixodes population dynamics model
# Dave Allen and Myles Stokowski
# compare multiple parameters test

devtools::load_all(export_all = FALSE)

config_path <- 'inputs/improved_ixodes_scapularis/config.yml'

cfg <- read_config(config_path)

cfg$steps <- as.integer(365*3)

out_1 <- run(cfg)

graph_population_each_group(out_1)
graph_population_overall_trend(out_1)

# change number of eggs
cfg_list <- vary_param(cfg = cfg, param_row = 58, values = c(1000,2000,5000))

many_out <- lapply(cfg_list, run)

require(cowplot)
p1 <- graph_population_each_group(many_out[[1]])
p2 <- graph_population_each_group(many_out[[2]])
p3 <- graph_population_each_group(many_out[[3]])

plot_grid(p1, p2, p3)


# make host finding harder
cfg_list <- vary_param(cfg = cfg,
                       from = "q..",
                       to = "a..",
                       param_name = 'a',
                       values = c(1e-4, 2e-4, 3e-4, 4e-4) )

many_out <- lapply(cfg_list, run)
p1 <- graph_population_each_group(many_out[[1]])
p2 <- graph_population_each_group(many_out[[2]])
p3 <- graph_population_each_group(many_out[[3]])
p4 <- graph_population_each_group(many_out[[4]])

plot_grid(p1, p2, p3, p4)

# change number of eggs and make host finding harder
cfg_list <- vary_many_params(
  cfg = cfg,
  param_rows = c(58, 24),
  values_list = list(
    c(1000,2000,5000), # values for number of eggs (row 58)
    c(1e-4, 2e-4, 3e-4, 4e-4) # values for host finding (row 24)
  )
)

# check that it modified the parameter values we were expecting
unname(sapply(cfg_list,
              function(x) x$parameters[c(58, 24), 'param_value']))

# check that output is modified as expected
many_out <- run_all_configs(cfg_list, parallel = TRUE)
plots <- lapply(many_out, graph_population_each_group)
do.call(cowplot::plot_grid, plots)

