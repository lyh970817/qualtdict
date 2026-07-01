# render_response_columns preserves row-aligned output shape

    Code
      compact_response_column_render(rendered)
    Output
      # A tibble: 4 x 5
        response_column_id question   item  level  label     
        <chr>              <chr>      <chr> <chr>  <chr>     
      1 QID1               Choose one <NA>  1      Yes       
      2 QID1               Choose one <NA>  2      No        
      3 QID1               Choose one <NA>  3      Other     
      4 QID1_3_TEXT        Choose one <NA>  3_TEXT Other_TEXT

# render_response_columns preserves loop-prefixed base IDs

    Code
      compact_response_column_render(rendered)
    Output
      # A tibble: 3 x 5
        response_column_id question                            item  level  label     
        <chr>              <chr>                               <chr> <chr>  <chr>     
      1 x1_QID2            Explain your ${lm://Field/1} answer <NA>  1      Selected  
      2 x1_QID2            Explain your ${lm://Field/1} answer <NA>  2      Other     
      3 x1_QID2_2_TEXT     Explain your ${lm://Field/1} answer <NA>  2_TEXT Other_TEXT

