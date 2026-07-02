looped_question_facts <- function(question_facts) {
  question_facts[
    vapply(question_facts, function(x) isTRUE(x$looping), logical(1))
  ]
}

compact_loop_question_fact <- function(question_fact) {
  list(
    qid = question_fact$qid %||% NA_character_,
    looping_qid = question_fact$looping_qid %||% NA_character_,
    looping_option = question_fact$looping_option %||% NA_character_,
    looping_prefix = question_fact$looping_prefix %||% NA_character_,
    looping_question = question_fact$looping_question %||% NA_character_,
    question_text = question_fact$question_text %||% NA_character_,
    base_response_column_id = question_fact$base_response_column_id %||%
      NA_character_,
    looping = isTRUE(question_fact$looping)
  )
}

compact_loop_question_facts <- function(question_facts) {
  tibble::as_tibble(
    purrr::map_dfr(
      looped_question_facts(question_facts),
      compact_loop_question_fact
    )
  )
}

looped_dictionary_rows <- function(dict, qid) {
  dict[identical_na(dict$qid, qid), ]
}

synthetic_unresolved_source_loop_and_merge_raw_metadata <- function() {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata()
  raw_metadata$description$blocks$BL_LOOP$Options$LoopingOptions$Static <- list(
    missing_a = list(`1` = ""),
    missing_b = list(`1` = "")
  )
  raw_metadata
}

synthetic_absent_source_static_loop_and_merge_raw_metadata <- function() {
  raw_metadata <- synthetic_loop_and_merge_raw_metadata()
  raw_metadata$metadata$questions$QID1 <- NULL
  raw_metadata$description$questions$QID1 <- NULL
  raw_metadata$description$blocks$BL_SOURCE <- NULL
  raw_metadata$description$blocks$BL_LOOP$Options$LoopingOptions$Static <- list(
    `1` = list(`1` = "Apples", `2` = "Red fruit"),
    `2` = list(`1` = "Bananas", `2` = "Yellow fruit")
  )
  raw_metadata
}

synthetic_absent_source_unusable_static_loop_and_merge_raw_metadata <-
  function() {
    raw_metadata <- synthetic_absent_source_static_loop_and_merge_raw_metadata()
    raw_metadata$description$blocks$BL_LOOP$Options$LoopingOptions$Static <-
      NULL
    raw_metadata
  }

identical_na <- function(x, y) {
  !is.na(x) & x == y
}
