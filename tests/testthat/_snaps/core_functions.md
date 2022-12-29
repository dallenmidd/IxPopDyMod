# `gen_trans_matrix()` works with `ogden2005`

    Code
      gen_trans_matrix(time = 150, N = population, N_developing = population,
        life_stages = life_stages, tick_transitions = transitions_with_parameters,
        predictors = ogden2005$predictors)
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
    Warning <lifecycle_warning_deprecated>
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"day"` instead of `.data$day`
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
    Warning <lifecycle_warning_deprecated>
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"day"` instead of `.data$day`
    Output
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

