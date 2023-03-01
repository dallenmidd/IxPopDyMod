# produces expected output with valid input

    Code
      new_transition(from = "a", to = "b", transition_type = "probability",
        mortality_type = NULL, fun = new_transition_function(constant_fun),
        predictors = NULL, parameters = new_parameters(a = 1))
    Output
      $from
      [1] "a"
      
      $to
      [1] "b"
      
      $transition_type
      [1] "probability"
      
      $mortality_type
      NULL
      
      $fun
      function(a) a
      <environment: namespace:IxPopDyMod>
      attr(,"class")
      [1] "transition_function"
      
      $predictors
      NULL
      
      $parameters
      $a
      [1] 1
      
      attr(,"class")
      [1] "parameters"
      
      attr(,"class")
      [1] "transition"

# works with defaults

    Code
      transition(from = "a", to = "b", fun = f, transition_type = "probability")
    Output
      $from
      [1] "a"
      
      $to
      [1] "b"
      
      $transition_type
      [1] "probability"
      
      $mortality_type
      NULL
      
      $fun
      function() NULL
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

