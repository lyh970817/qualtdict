# Loop-expanded Question Facts have compact contract summary

    Code
      compact_loop_question_facts(expanded)
    Output
      # A tibble: 2 x 8
        qid   looping_qid looping_option looping_prefix looping_question question_text
        <chr> <chr>       <chr>          <chr>          <chr>            <chr>        
      1 QID2  QID1        Apples         x1             Compare Apples ~ Compare {} w~
      2 QID2  QID1        Bananas        x2             Compare Bananas~ Compare {} w~
      # i 2 more variables: base_response_column_id <chr>, looping <lgl>

