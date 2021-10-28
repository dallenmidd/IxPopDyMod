# create broken configs and make sure the errors get caught
# all calls to validate_config() should throw an error unless otherwise noted

devtools::load_all()
base_config <- read_config('inputs/ogden_config.yml')

reset <- function() broken <<- base_config
test <- function() validate_config(broken)

# steps
reset()
broken$steps <- c(1,1)
test()

reset()
broken$steps <- 0
test()

# max_delay
reset()
broken$max_delay <- c(1,1)
test()

reset()
broken$max_delay <- 364
test()

# column types and names
reset()
broken$weather$tmean <- as.integer(broken$weather$tmean)
test() # no error

reset()
broken$weather$tmean <- as.character(broken$weather$tmean)
test()

reset()
names(broken$host_comm)[2] <- 'unexpected_name'
test()

# sort params by host_spp
reset()
broken$parameters[1,] <- base_config$parameters[4,]
broken$parameters[4,] <- base_config$parameters[1,]
test()

# initial population
reset()
broken$initial_population <- unname(broken$initial_population)
test()

reset()
names(broken$initial_population) <- 'not_a_life_stage'
test()

reset()
broken$initial_population[1] <- 0
test()

reset()
broken$initial_population <- c('r_a' = 1, '__e' = -1)
test()

# parameter pattern matching
reset()
broken$parameters[1,1] <- 'not_a_valid_pattern'
broken$parameters[1,2] <- 'not_a_valid_pattern'
broken$parameters[3,2] <- 'not_a_valid_pattern'
broken$parameters[4,1] <- 'not_a_valid_pattern'
test()

# functions exist
reset()
broken$transitions[1,3] <- 'not_a_valid_function'
test()

# missing and extra params
reset()
broken$parameters[7, 3] <- ''
test()

# predictor is valid
reset()
broken$transitions[1, 'pred1'] <- 'not_a_valid_predictor_value'
test()

# missing days
reset()
broken$weather[1,2] <- 2
test()

reset()
broken$weather[1:10, 2] <- 2
test()

# host spp days
reset()
broken$host_comm[7001, ] <- broken$host_comm[1,]
test()

# transition values are valid
reset()
broken$parameters[7,'param_value'] <- -1
broken$parameters[16, 'param_value'] <- -1
test()


