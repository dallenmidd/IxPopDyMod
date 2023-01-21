# output test with NULL predictors

    Code
      new_config(cycle = life_cycle(transition("a", "b", f1, "probability"),
      transition("b", "a", f2, "probability")), preds = NULL, initial_population = c(
        a = 1L, b = 0L), steps = 10L, max_delay = 365L)
    Output
      $cycle
      [[1]]
      $from
      [1] "a"
      
      $to
      [1] "b"
      
      $transition_type
      [1] "probability"
      
      $mortality_type
      NULL
      
      $fun
      function() 0.1
      <environment: R_EmptyEnv>
      attr(,"class")
      [1] "transition_function"
      
      $predictors
      NULL
      
      $parameters
      list()
      attr(,"class")
      [1] "parameters"
      
      attr(,"class")
      [1] "transition"
      
      [[2]]
      $from
      [1] "b"
      
      $to
      [1] "a"
      
      $transition_type
      [1] "probability"
      
      $mortality_type
      NULL
      
      $fun
      function() 10
      <environment: R_EmptyEnv>
      attr(,"class")
      [1] "transition_function"
      
      $predictors
      NULL
      
      $parameters
      list()
      attr(,"class")
      [1] "parameters"
      
      attr(,"class")
      [1] "transition"
      
      attr(,"class")
      [1] "life_cycle"
      
      $initial_population
      a b 
      1 0 
      
      $preds
      NULL
      
      $steps
      [1] 10
      
      $max_delay
      [1] 365
      
      attr(,"class")
      [1] "config"

# output test with non-NULL predictors

    Code
      new_config(cycle = life_cycle(transition("a", "b", f1, "probability"),
      transition("b", "a", f2, "probability")), preds = predictors(data.frame(pred = "temp",
        pred_subcategory = NA, j_day = NA, value = 1)), initial_population = c(a = 1L,
        b = 0L), steps = 10L, max_delay = 365L)
    Output
      $cycle
      [[1]]
      $from
      [1] "a"
      
      $to
      [1] "b"
      
      $transition_type
      [1] "probability"
      
      $mortality_type
      NULL
      
      $fun
      function() 0.1
      <environment: R_EmptyEnv>
      attr(,"class")
      [1] "transition_function"
      
      $predictors
      NULL
      
      $parameters
      list()
      attr(,"class")
      [1] "parameters"
      
      attr(,"class")
      [1] "transition"
      
      [[2]]
      $from
      [1] "b"
      
      $to
      [1] "a"
      
      $transition_type
      [1] "probability"
      
      $mortality_type
      NULL
      
      $fun
      function() 10
      <environment: R_EmptyEnv>
      attr(,"class")
      [1] "transition_function"
      
      $predictors
      NULL
      
      $parameters
      list()
      attr(,"class")
      [1] "parameters"
      
      attr(,"class")
      [1] "transition"
      
      attr(,"class")
      [1] "life_cycle"
      
      $initial_population
      a b 
      1 0 
      
      $preds
        pred pred_subcategory j_day value
      1 temp               NA    NA     1
      
      $steps
      [1] 10
      
      $max_delay
      [1] 365
      
      attr(,"class")
      [1] "config"

