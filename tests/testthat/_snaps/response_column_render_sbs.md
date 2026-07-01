# SBS mixed columns preserve row metadata alignment

    Code
      compact_response_column_render(rendered)
    Output
      # A tibble: 18 x 5
         response_column_id question                     item            level  label
         <chr>              <chr>                        <chr>           <chr>  <chr>
       1 QID2#1_2_1         Side by side Text column     Second row      1_TEXT Text 
       2 QID2#1_4_1         Side by side Text column     Fourth row      1_TEXT Text 
       3 QID2#1_4_TEXT      Side by side Text column     Fourth row_TEXT TEXT   TEXT 
       4 QID2#1_9_1         Side by side Text column     Ninth row       1_TEXT Text 
       5 QID2#2_2           Side by side Single column A Second row      1      Yes  
       6 QID2#2_2           Side by side Single column A Second row      0      No   
       7 QID2#2_4           Side by side Single column A Fourth row      1      Yes  
       8 QID2#2_4           Side by side Single column A Fourth row      0      No   
       9 QID2#2_4_TEXT      Side by side Single column A Fourth row_TEXT TEXT   TEXT 
      10 QID2#2_9           Side by side Single column A Ninth row       1      Yes  
      11 QID2#2_9           Side by side Single column A Ninth row       0      No   
      12 QID2#3_2           Side by side Single column B Second row      1      Yes  
      13 QID2#3_2           Side by side Single column B Second row      0      No   
      14 QID2#3_4           Side by side Single column B Fourth row      1      Yes  
      15 QID2#3_4           Side by side Single column B Fourth row      0      No   
      16 QID2#3_4_TEXT      Side by side Single column B Fourth row_TEXT TEXT   TEXT 
      17 QID2#3_9           Side by side Single column B Ninth row       1      Yes  
      18 QID2#3_9           Side by side Single column B Ninth row       0      No   

# SBS carried-forward rows use subquestion response column IDs

    Code
      compact_response_column_render(rendered)
    Output
      # A tibble: 3 x 5
        response_column_id question     item       level label
        <chr>              <chr>        <chr>      <chr> <chr>
      1 QID2_x1            Side by side First row  <NA>  <NA> 
      2 QID2_x2            Side by side Second row <NA>  <NA> 
      3 QID2_x3            Side by side Third row  <NA>  <NA> 

