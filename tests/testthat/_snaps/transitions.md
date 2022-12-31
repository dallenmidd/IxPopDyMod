# produces expected output with valid input

    Code
      new_transition(from = "a", to = "b", transition_type = new_transition_type(
        "probability"), mortality_type = new_mortality_type(NA), fun = constant_fun,
      predictors = "", parameters = new_parameters())
    Output
      $from
      [1] "a"
      
      $to
      [1] "b"
      
      $transition_type
      [1] "probability"
      attr(,"class")
      [1] "transition_type"
      
      $mortality_type
      [1] NA
      attr(,"class")
      [1] "mortality_type"
      
      $fun
      function(x, y, a) a
      <environment: namespace:IxPopDyMod>
      
      $predictors
      [1] ""
      
      $parameters
      list()
      attr(,"class")
      [1] "parameters"
      
      attr(,"class")
      [1] "transition"

# works with allowed input

    Code
      new_transition_type("probability")
    Output
      [1] "probability"
      attr(,"class")
      [1] "transition_type"

---

    Code
      new_mortality_type("per_day")
    Output
      [1] "per_day"
      attr(,"class")
      [1] "mortality_type"

