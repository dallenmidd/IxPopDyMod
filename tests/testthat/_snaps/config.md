# output test with NULL predictors

    Code
      new_config(cycle = life_cycle(transition("a", "b", f1, "probability"),
      transition("b", "a", f2, "probability")), preds = NULL, initial_population = c(
        a = 1L, b = 0L), steps = 10L, max_duration = 365L)
    Output
      $cycle
      ** A life cycle
      ** Number of transitions: 2
      ** Unique life stages: a, b
      1. a -> b 
      2. b -> a 
      
      $initial_population
      a b 
      1 0 
      
      $preds
      NULL
      
      $steps
      [1] 10
      
      $max_duration
      [1] 365
      
      attr(,"class")
      [1] "config"

# output test with non-NULL predictors

    Code
      new_config(cycle = life_cycle(transition("a", "b", f1, "probability"),
      transition("b", "a", f2, "probability")), preds = predictors(data.frame(pred = "temp",
        pred_subcategory = NA, j_day = NA, value = 1)), initial_population = c(a = 1L,
        b = 0L), steps = 10L, max_duration = 365L)
    Output
      $cycle
      ** A life cycle
      ** Number of transitions: 2
      ** Unique life stages: a, b
      1. a -> b 
      2. b -> a 
      
      $initial_population
      a b 
      1 0 
      
      $preds
        pred pred_subcategory j_day value
      1 temp               NA    NA     1
      
      $steps
      [1] 10
      
      $max_duration
      [1] 365
      
      attr(,"class")
      [1] "config"

