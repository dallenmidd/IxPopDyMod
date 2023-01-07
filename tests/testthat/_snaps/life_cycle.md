# output test

    Code
      new_life_cycle(list(transition_example_a(), transition_example_b()))
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
      <environment: namespace:IxPopDyMod>
      attr(,"class")
      [1] "transition_function"
      
      $predictors
                   x              y 
              "temp" "host_density" 
      
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
      <environment: namespace:IxPopDyMod>
      attr(,"class")
      [1] "transition_function"
      
      $predictors
                   x              y 
              "temp" "host_density" 
      
      $parameters
      $a
      [1] 1
      
      attr(,"class")
      [1] "parameters"
      
      attr(,"class")
      [1] "transition"
      
      attr(,"class")
      [1] "life_cycle"

