Modifying the input parameter and transition function files we've been using in order to match
Ogden as closely as possible.

Running list of differences I've identified between Ogden and this implementation:
- Ogden has density dependent reduction in fecundity of egg-laying females
- Ogden handles host finding differently
  - host finding probability from Ogden is weekly, and diff for adults vs larvae and nymphs. Weekly prob is (.0089 * 200 ^ .515) = 0.13 for larvae and nymphs, (0.06 * 20 ^ .515) = 0.2806608 for adults. We've been using daily probability of 4e-4... my probability math is rusty, not sure how we'd convert. 
  - Ogden uses Fig 3 curve to determine prob of active questing, vs our briere function approach
- input weather data... I poked around a bit online for the weather data from the statio in Ontario they use, but no luck