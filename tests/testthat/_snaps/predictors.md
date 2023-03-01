# output test

    Code
      predictors(data.frame(pred = c(rep("host_den", 4), "temp"), pred_subcategory = c(
        "mouse", "mouse", "deer", "deer", NA), j_day = c(1, 2, 1, 2, NA), value = 1:5))
    Output
            pred pred_subcategory j_day value
      1     temp             <NA>    NA     5
      2 host_den             deer     1     3
      3 host_den            mouse     1     1
      4 host_den             deer     2     4
      5 host_den            mouse     2     2

