# MC text-entry choices render current response column IDs

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

# MC independent columns preserve recode suffix behavior

    Code
      compact_response_column_render(rendered)
    Output
      # A tibble: 5 x 5
        response_column_id question              item  level label   
        <chr>              <chr>                 <chr> <chr> <chr>   
      1 QID126879611_1     Select all that apply <NA>  1     Choice 1
      2 QID126879611_2     Select all that apply <NA>  2     Choice 2
      3 QID126879611_3     Select all that apply <NA>  3     Choice 3
      4 QID126879611_4     Select all that apply <NA>  4     Choice 4
      5 QID126879611_6     Select all that apply <NA>  6     Choice 6

