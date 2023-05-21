# output test

    Code
      print.default(new_life_cycle(transition_example_a(), transition_example_b()))
    Output
      [[1]]
      ** A transition
      ** a -> b 
      Transition type: probability
      Predictors: x = list(pred = "temp", first_day_only = TRUE), y = list(pred = "host_density", first_day_only = TRUE)
      Parameters: a = 1
      Function: (x, y, a) a
      
      [[2]]
      ** A transition
      ** b -> a 
      Transition type: probability
      Predictors: x = list(pred = "temp", first_day_only = TRUE), y = list(pred = "host_density", first_day_only = TRUE)
      Parameters: a = 1
      Function: (x, y, a) a
      
      attr(,"class")
      [1] "life_cycle"

