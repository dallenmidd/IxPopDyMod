# produces expected output with valid input

    Code
      new_transition(from = "a", to = "b", transition_type = "probability",
        mortality_type = NULL, fun = new_transition_function(constant_fun),
        predictors = "", parameters = new_parameters())
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
      function(x, y, a) a
      <environment: namespace:IxPopDyMod>
      attr(,"class")
      [1] "transition_function"
      
      $predictors
      [1] ""
      
      $parameters
      list()
      attr(,"class")
      [1] "parameters"
      
      attr(,"class")
      [1] "transition"

