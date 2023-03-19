# output test

    Code
      predictors_spec(a = new_predictors_spec_node(pred = "temp", first_day_only = FALSE),
      b = new_predictors_spec_node(pred = "host_den", first_day_only = TRUE))
    Output
      $a
      ** A predictor_spec_node
      ** pred:           "temp"
      ** first_day_only: FALSE
      
      $b
      ** A predictor_spec_node
      ** pred:           "host_den"
      ** first_day_only: TRUE
      
      attr(,"class")
      [1] "predictors_spec"

