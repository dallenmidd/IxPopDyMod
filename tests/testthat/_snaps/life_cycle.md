# output test

    Code
      print.default(new_life_cycle(transition_example_a(), transition_example_b()))
    Output
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
      function(x, y, a) a
      <environment: R_EmptyEnv>
      attr(,"class")
      [1] "transition_function"
      
      $predictors
      $predictors$x
      ** A predictor_spec_node
      ** pred:           "temp"
      ** first_day_only: TRUE
      
      $predictors$y
      ** A predictor_spec_node
      ** pred:           "host_density"
      ** first_day_only: TRUE
      
      
      $parameters
      $a
      [1] 1
      
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
      function(x, y, a) a
      <environment: R_EmptyEnv>
      attr(,"class")
      [1] "transition_function"
      
      $predictors
      $predictors$x
      ** A predictor_spec_node
      ** pred:           "temp"
      ** first_day_only: TRUE
      
      $predictors$y
      ** A predictor_spec_node
      ** pred:           "host_density"
      ** first_day_only: TRUE
      
      
      $parameters
      $a
      [1] 1
      
      attr(,"class")
      [1] "parameters"
      
      attr(,"class")
      [1] "transition"
      
      attr(,"class")
      [1] "life_cycle"

