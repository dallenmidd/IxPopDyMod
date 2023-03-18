# output test

    Code
      new_predictors_specification(a = new_predictors_specification_node(name = "temp",
        first_day_only = FALSE), b = new_predictors_specification_node(name = "host_den",
        first_day_only = TRUE))
    Output
      $a
      $name
      [1] "temp"
      
      $first_day_only
      [1] FALSE
      
      attr(,"class")
      [1] "predictors_specification_node"
      
      $b
      $name
      [1] "host_den"
      
      $first_day_only
      [1] TRUE
      
      attr(,"class")
      [1] "predictors_specification_node"
      
      attr(,"class")
      [1] "predictors_specification"

