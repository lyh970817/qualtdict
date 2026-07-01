# matrix item-level rows render in stable order

    Code
      compact_response_column_render(rendered)
    Output
      # A tibble: 4 x 5
        response_column_id question       item    level label
        <chr>              <chr>          <chr>   <chr> <chr>
      1 QID1_x1            Rate each item Apples  1     Low  
      2 QID1_x1            Rate each item Apples  2     High 
      3 QID1_x2            Rate each item Bananas 1     Low  
      4 QID1_x2            Rate each item Bananas 2     High 

# slider items render as one response column per item

    Code
      compact_response_column_render(rendered)
    Output
      # A tibble: 3 x 5
        response_column_id question       item  level label
        <chr>              <chr>          <chr> <chr> <chr>
      1 QID1_1             Rate each item <NA>  1     One  
      2 QID1_2             Rate each item <NA>  2     Two  
      3 QID1_3             Rate each item <NA>  3     Three

