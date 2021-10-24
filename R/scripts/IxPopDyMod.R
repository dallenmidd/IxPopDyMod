# Ixodes population dynamics model
# Dave Allen and Myles Stokowski

devtools::load_all(export_all = FALSE)

config_path <- 'inputs/improved_ixodes_scapularis/config.yml'

cfg <- read_config(config_path)

out <- run(cfg)

graph_population_each_group(out)
graph_population_overall_trend(out)
