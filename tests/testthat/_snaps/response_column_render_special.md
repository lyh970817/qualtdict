# Timing fixed columns preserve current response column IDs

    Code
      compact_response_column_render(rendered)
    Output
      # A tibble: 4 x 5
        response_column_id question item  level label
        <chr>              <chr>    <chr> <chr> <chr>
      1 QID1_FIRST_CLICK   Timing   <NA>  <NA>  <NA> 
      2 QID1_LAST_CLICK    Timing   <NA>  <NA>  <NA> 
      3 QID1_PAGE_SUBMIT   Timing   <NA>  <NA>  <NA> 
      4 QID1_CLICK_COUNT   Timing   <NA>  <NA>  <NA> 

# FileUpload fixed columns preserve current response column IDs

    Code
      compact_response_column_render(rendered)
    Output
      # A tibble: 4 x 5
        response_column_id question      item  level label
        <chr>              <chr>         <chr> <chr> <chr>
      1 QID1_FILE_ID       Upload a file <NA>  <NA>  <NA> 
      2 QID1_FILE_NAME     Upload a file <NA>  <NA>  <NA> 
      3 QID1_FILE_SIZE     Upload a file <NA>  <NA>  <NA> 
      4 QID1_FILE_TYPE     Upload a file <NA>  <NA>  <NA> 

# Draw signature uses file-upload fixed response column IDs

    Code
      compact_response_column_render(rendered)
    Output
      # A tibble: 4 x 5
        response_column_id question  item  level label
        <chr>              <chr>     <chr> <chr> <chr>
      1 QID1_FILE_ID       Sign here <NA>  <NA>  <NA> 
      2 QID1_FILE_NAME     Sign here <NA>  <NA>  <NA> 
      3 QID1_FILE_SIZE     Sign here <NA>  <NA>  <NA> 
      4 QID1_FILE_TYPE     Sign here <NA>  <NA>  <NA> 

# unsupported shapes warn and fall back to Base Response Column ID

    Code
      compact_response_column_render(rendered)
    Output
      # A tibble: 1 x 5
        response_column_id question   item  level label
        <chr>              <chr>      <chr> <chr> <chr>
      1 x1_QID1            Choose one <NA>  1     Yes  

# PGR drag-and-drop no-columns warns and falls back

    Code
      compact_response_column_render(rendered)
    Output
      # A tibble: 1 x 5
        response_column_id question   item  level label
        <chr>              <chr>      <chr> <chr> <chr>
      1 x1_QID1            Drag items <NA>  <NA>  <NA> 

