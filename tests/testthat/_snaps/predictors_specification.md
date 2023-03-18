# output test

    Code
      predictors_spec(a = new_predictors_spec_node(name = "temp", first_day_only = FALSE),
      b = new_predictors_spec_node(name = "host_den", first_day_only = TRUE))
    Output
      $a
      $name
      [1] "temp"
      
      $first_day_only
      [1] FALSE
      
      attr(,"class")
      [1] "predictors_spec_node"
      
      $b
      $name
      [1] "host_den"
      
      $first_day_only
      [1] TRUE
      
      attr(,"class")
      [1] "predictors_spec_node"
      
      attr(,"class")
      [1] "predictors_spec"

