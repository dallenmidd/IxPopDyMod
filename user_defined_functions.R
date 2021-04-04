# user_defined_functions.R

# 02 functional forms for transition probabilities
## DA Note: this might want to be moved to a new file, maybe part of input?
## DA Note: in the simple example can we show a simpiler verison of a host comm?

expo_fun <- function(x, y, a, b) ifelse(x>0, a*x^b, 0)
constant_fun <- function(x, y, a) a

# first part is chance they find a host and second part is success of feeding on them
find_n_feed <- function(x, y, a, pref, feed_success)
  (1 - (1-a)^(sum(x * pref)/sum(pref))) *   sum(x * pref * feed_success/sum(x*pref))

# product of binomial and briere functions
# (prob of finding a host) * (prob of active questing)
feed_fun <- function(x, y, a, pref, q, tmin, tmax)
  (1 - (1-a)^(sum(x * pref))) * ifelse(y>tmin & y<tmax, q*y*(y-tmin)*sqrt(tmax-y), 0)

# Return the probability that a feeding tick becomes infected or uninfected engorged
# Since density dependent mortality is subtracted later, in this function we assume
# that all feeding ticks feed successfully and become engorged
infect_fun <- function(x, y, from_infected, to_infected, host_rc, pref)
  sum(ifelse(rep(from_infected, n_host_spp),
             1, # stay infected
             (ifelse(rep(to_infected, n_host_spp),
                     host_rc, # become infected
                     1 - host_rc))) * # stay uninfected
        (x * pref) / sum(x * pref)) # chance a tick is feeding on each host type


# density dependent mortality
# x = host_den, y = number of feeding ticks
density_fun <- function(x, y, a, b, c, pref)
  sum((a + (b * log((c + y * pref * x / sum(pref * x)) / x))) * pref * x / sum(pref * x))
