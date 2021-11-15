# test_constant_inputs.R

devtools::load_all()

# test with Ogden config

ogden_original <- ogden2005
ogden_original$steps <- 1000

out <- run(ogden_original)

ogden_constant <- ogden_original
# ogden_constant$weather <- ogden_constant$weather[1,1]
ogden_constant$host_comm <- ogden_constant$host_comm[1:2, 2:3]

out2 <- run(ogden_constant)

all(out == out2)

# test with our config
cfg <- read_config('vignettes/improved_ixodes_scapularis/config.yml')

cfg$steps <- 1000
cfg_constant <- cfg
cfg_constant$host_comm <- cfg_constant$host_comm[1:3, 2:3]

out3 <- run(cfg)
out4 <- run(cfg_constant)

all(out3 == out4)
# then, checkout the released version and verify that results are still the
# same when run with the non-constant inputs
