# `gen_trans_matrix()` works with `ogden2005`

    Code
      gen_trans_matrix(time = 150, population = population, developing_population = population,
        tick_transitions = transitions_with_parameters, predictors = ogden2005$
          predictors)
    Output
           __e e_l e_n         a_l         a_n h_l       q_l e_a r_a        a_a
      __e    0   0   0 0.000000000 0.000000000   0 0.0000000   0   0 0.00000000
      e_l    0   0   0 0.000000000 0.000000000   0 0.0000000   0   0 0.00000000
      e_n    0   0   0 0.000000000 0.000000000   0 0.0000000   0   0 0.00000000
      a_l    0   0   0 0.000000000 0.000000000   0 0.0000000   0   0 0.00000000
      a_n    0   0   0 0.000000000 0.000000000   0 0.0000000   0   0 0.00000000
      h_l    0   0   0 0.000000000 0.000000000   0 0.0000000   0   0 0.00000000
      q_l    0   0   0 0.002187281 0.000000000   0 0.9918127   0   0 0.00000000
      e_a    0   0   0 0.000000000 0.000000000   0 0.0000000   0   0 0.00000000
      r_a 3000   0   0 0.000000000 0.000000000   0 0.0000000   0   0 0.00000000
      a_a    0   0   0 0.000000000 0.000000000   0 0.0000000   0   0 0.00000000
      q_a    0   0   0 0.000000000 0.000000000   0 0.0000000   0   0 0.08988336
      q_n    0   0   0 0.000000000 0.002187281   0 0.0000000   0   0 0.00000000
                q_a       q_n
      __e 0.0000000 0.0000000
      e_l 0.0000000 0.0000000
      e_n 0.0000000 0.0000000
      a_l 0.0000000 0.0000000
      a_n 0.0000000 0.0000000
      h_l 0.0000000 0.0000000
      q_l 0.0000000 0.0000000
      e_a 0.0000000 0.0000000
      r_a 0.0000000 0.0000000
      a_a 0.0000000 0.0000000
      q_a 0.9041166 0.0000000
      q_n 0.0000000 0.9918127

# model output for `config_ex_1` stays the same

    Code
      run(config_ex_1)
    Output
      # A tibble: 116 x 6
           day stage     pop age_group process infected
         <int> <chr>   <dbl> <chr>     <chr>   <lgl>   
       1     1 __e         0 e         _       FALSE   
       2     1 __l         0 l         _       FALSE   
       3     1 __n         0 n         _       FALSE   
       4     1 __a      1000 a         _       FALSE   
       5     2 __e   1000000 e         _       FALSE   
       6     2 __l         0 l         _       FALSE   
       7     2 __n         0 n         _       FALSE   
       8     2 __a         0 a         _       FALSE   
       9     3 __e         0 e         _       FALSE   
      10     3 __l   1000000 l         _       FALSE   
      # ... with 106 more rows

# model output for `config_ex_2` stays the same

    Code
      run(config_ex_2)
    Output
      [1] "day 100"
      [1] "day 200"
      [1] "day 300"
      # A tibble: 1,460 x 6
           day stage     pop age_group process infected
         <int> <chr>   <dbl> <chr>     <chr>   <lgl>   
       1     1 __e        0  e         _       FALSE   
       2     1 __l        0  l         _       FALSE   
       3     1 __n        0  n         _       FALSE   
       4     1 __a     1000  a         _       FALSE   
       5     2 __e   489304. e         _       FALSE   
       6     2 __l        0  l         _       FALSE   
       7     2 __n        0  n         _       FALSE   
       8     2 __a        0  a         _       FALSE   
       9     3 __e        0  e         _       FALSE   
      10     3 __l   484411. l         _       FALSE   
      # ... with 1,450 more rows

# empty_delay_array snapshot

    Code
      empty_delay_array(c("a", "b"), 1, 1)
    Output
      , , 1
      
        a b
      a 0 0
      b 0 0
      
      , , 2
      
        a b
      a 0 0
      b 0 0
      

# empty_population_matrix snapshot

    Code
      empty_population_matrix(c("a", "b"), 3)
    Output
        [,1] [,2] [,3]
      a    0    0    0
      b    0    0    0

# set_initial_population snapshot

    Code
      set_initial_population(population, c(b = 10))
    Output
        [,1] [,2] [,3]
      a    0    0    0
      b   10    0    0

# model output for ogden config stays the same

    Code
      run(cfg)
    Output
      [1] "day 100"
      [1] "day 200"
      [1] "day 300"
      # A tibble: 4,380 x 6
           day stage   pop age_group process infected
         <int> <chr> <dbl> <chr>     <chr>   <lgl>   
       1     1 __e       0 e         _       FALSE   
       2     1 e_l       0 l         e       FALSE   
       3     1 e_n       0 n         e       FALSE   
       4     1 a_l       0 l         a       FALSE   
       5     1 a_n       0 n         a       FALSE   
       6     1 h_l       0 l         h       FALSE   
       7     1 q_l       0 l         q       FALSE   
       8     1 e_a       0 a         e       FALSE   
       9     1 r_a       0 a         r       FALSE   
      10     1 a_a       0 a         a       FALSE   
      # ... with 4,370 more rows

