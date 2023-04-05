# produces expected output with valid input

    Code
      trans[names(trans) != "fun"]
    Output
      $from
      [1] "a"
      
      $to
      [1] "b"
      
      $transition_type
      [1] "probability"
      
      $mortality_type
      NULL
      
      $predictors
      NULL
      
      $parameters
      $a
      [1] 1
      
      attr(,"class")
      [1] "parameters"
      

# works with defaults

    Code
      transition(from = "a", to = "b", fun = f, transition_type = "probability")
    Output
      ** A transition
      ** a -> b 
      Transition type: probability
      Parameters: 
      Function: () NULL

