#' Fetch raw Qualtrics metadata for Variable Dictionary generation
#'
#' This keeps the Qualtrics API calls separate from the package-owned metadata
#' model so synthetic fixtures can exercise dictionary generation without live
#' API access.
#'
#' @keywords internal
#' @noRd
fetch_dictionary_metadata <- function(surveyID) {
  mt <- metadata2(
    surveyID,
    c(
      "questions",
      "metadata",
      "blocks",
      "responsecounts",
      "flow",
      "comments"
    )
  )

  mt_d <- fetch_description2(
    surveyID,
    c(
      "questions",
      "metadata",
      "blocks",
      "flow"
    )
  )

  new_raw_qualtrics_metadata(surveyID, mt, mt_d)
}

#' Build a raw Qualtrics metadata bundle
#'
#' Tests can construct this object from synthetic Qualtrics-shaped lists and
#' feed it into `normalise_qualtrics_metadata()`.
#'
#' @keywords internal
#' @noRd
new_raw_qualtrics_metadata <- function(surveyID, metadata, description) {
  structure(
    list(
      surveyID = surveyID,
      survey_name = as.character(metadata$metadata$name),
      metadata = metadata,
      description = description
    ),
    class = c("qualtdict_raw_metadata", "list")
  )
}

#' Normalise raw Qualtrics metadata
#'
#' The normalised metadata model is internal and question-level for this tracer
#' bullet. It owns the merged question, block, loop, and content-type metadata
#' that dictionary row generation consumes. Future slices can extend this
#' object with Loop Option details and Unsupported Structure Findings without
#' changing `dict_generate()`.
#'
#' @keywords internal
#' @noRd
normalise_qualtrics_metadata <- function(raw_metadata) {
  questions <- normalise_qualtrics_questions(
    raw_metadata$metadata,
    raw_metadata$description
  )
  unsupported_structure_findings <- unsupported_loop_structure_findings(
    questions
  )

  structure(
    list(
      surveyID = raw_metadata$surveyID,
      survey_name = raw_metadata$survey_name,
      questions = questions,
      unsupported_structure_findings = unsupported_structure_findings
    ),
    class = c("qualtdict_normalised_metadata", "list")
  )
}

#' Empty Unsupported Structure Findings table
#'
#' Unsupported Structure Findings are produced during normalisation and travel
#' with generated Variable Dictionaries as an attribute.
#'
#' @keywords internal
#' @noRd
empty_unsupported_structure_findings <- function() {
  tibble(
    qid = character(),
    type = character(),
    selector = character(),
    sub_selector = character(),
    finding = character(),
    details = character()
  )
}

#' Return Unsupported Structure Findings
#'
#' Unsupported Structure Findings describe Qualtrics structures that qualtdict
#' could not fully represent while normalising survey metadata. They are
#' attached to generated Variable Dictionaries and can also be read from the
#' internal normalised metadata object.
#'
#' @param x A Variable Dictionary from [dict_generate()] or an internal
#'   normalised metadata object.
#'
#' @return A data frame of Unsupported Structure Findings.
#' @export
unsupported_structure_findings <- function(x) {
  findings <- attr(x, "unsupported_structure_findings", exact = TRUE)

  if (is.null(findings) &&
    is.list(x) &&
    "unsupported_structure_findings" %in% names(x)) {
    findings <- x$unsupported_structure_findings
  }

  if (is.null(findings)) {
    return(empty_unsupported_structure_findings())
  }

  findings
}

#' Classify raw response columns that are not question dictionary rows
#' @keywords internal
#' @noRd
classify_raw_response_columns <- function(raw_columns) {
  classification <- rep(NA_character_, length(raw_columns))
  details <- rep(NA_character_, length(raw_columns))

  scoring <- grepl("^SC_", raw_columns) | grepl("_SCORE$", raw_columns)
  classification[scoring] <- "scoring"
  details[scoring] <-
    "Qualtrics scoring column; not represented as a question dictionary row."

  embedded_data <- is.na(classification) &
    grepl("^[A-Za-z][A-Za-z0-9_]*$", raw_columns) &
    !grepl("^QID[0-9]+", raw_columns)
  classification[embedded_data] <- "embedded_data"
  details[embedded_data] <-
    "Embedded-data or user-defined metadata column; not represented as a question dictionary row."

  tibble(
    raw_column = raw_columns,
    classification = classification,
    details = details
  )
}

#' Detect unsupported Loop and Merge shapes
#' @keywords internal
#' @noRd
unsupported_loop_structure_findings <- function(questions) {
  findings <- imap(questions, function(question, qid) {
    looping_qid <- scalar_character(question$looping_qid)
    if (is.na(looping_qid)) {
      return(NULL)
    }

    unsupported <- list()

    if (!looping_qid %in% names(questions)) {
      unsupported <- c(unsupported, list(unsupported_structure_finding(
        qid = qid,
        question = question,
        finding = "missing_loop_source",
        details = paste0(
          "Loop and Merge source question `",
          looping_qid,
          "` was not found in survey metadata."
        )
      )))
    }

    loop_fields <- loop_field_numbers(question$questionText)
    unsupported_fields <- setdiff(loop_fields, "1")
    if (length(unsupported_fields) > 0) {
      unsupported <- c(unsupported, list(unsupported_structure_finding(
        qid = qid,
        question = question,
        finding = "unsupported_loop_field",
        details = paste0(
          "Only `${lm://Field/1}` Loop and Merge placeholders are supported; ",
          "found field(s) ",
          paste(unique(unsupported_fields), collapse = ", "),
          "."
        )
      )))
    }

    unsupported
  }) %>%
    unlist(recursive = FALSE) %>%
    bind_rows()

  if (nrow(findings) == 0) {
    return(empty_unsupported_structure_findings())
  }

  findings
}

#' Build one Unsupported Structure Finding row
#' @keywords internal
#' @noRd
unsupported_structure_finding <- function(qid, question, finding, details) {
  question_type <- question$questionType

  tibble(
    qid = qid,
    type = scalar_character(question_type$type),
    selector = scalar_character(question_type$selector),
    sub_selector = scalar_character(question_type$subSelector),
    finding = finding,
    details = details
  )
}

#' Extract Loop and Merge field numbers from question text
#' @keywords internal
#' @noRd
loop_field_numbers <- function(question_text) {
  if (is.null(question_text) || is.na(question_text)) {
    return(character())
  }

  matches <- str_match_all(
    question_text,
    "\\$\\{lm://Field/([0-9]+)\\}"
  )[[1]]

  if (nrow(matches) == 0) {
    return(character())
  }

  matches[, 2]
}

#' Coerce a possibly missing scalar to character
#' @keywords internal
#' @noRd
scalar_character <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(NA_character_)
  }

  as.character(x[[1]])
}

#' Merge raw question, block, and content-type metadata
#' @keywords internal
#' @noRd
normalise_qualtrics_questions <- function(mt, mt_d) {
  blocks <- mt_d$block
  block_meta <- map(blocks, function(block) {
    list(
      description = block$Description,
      qid = unlist(map(block$BlockElements, "QuestionID")),
      looping_prefix = names(block$Options$LoopingOptions$Static),
      looping_qid = block$Options$LoopingOptions$QID
    )
  }) %>%
    map(function(block) {
      map(block$qid, ~ list(
        qid = .x,
        description = block$description,
        looping_prefix = block$looping_prefix,
        looping_qid = block$looping_qid
      ))
    }) %>%
    # Use 'c' to combine multiple lists into one list
    # Previously the lists are nested in block and then QID
    do.call(c, .) %>%
    setNames(map_chr(., ~ .x$qid))

  question_meta <- map(
    mt$questions, `[`,
    c(
      "questionName",
      "questionType",
      "questionText",
      "blocks",
      "columns",
      "choices",
      "subQuestions"
    )
  )

  content_type_meta <- mt_d$question %>%
    map("Validation") %>%
    map("Settings") %>%
    map("ContentType") %>%
    map(null_na) %>%
    map(str_remove, "Valid")

  # Order the metadatas by QID name and use only those in question_meta so that
  # the questions match.
  qids <- names(question_meta)
  question_meta <- question_meta[qids] %>%
    order_name()

  block_meta <- block_meta[qids] %>%
    order_name()

  content_type_meta <- content_type_meta[qids] %>%
    order_name()

  question_meta <- map2(question_meta, block_meta, function(x, y) {
    x["block"] <- y["description"]
    x["looping_prefix"] <- y["looping_prefix"]
    x["looping_qid"] <- y["looping_qid"]
    x
  }) %>%
    map2(content_type_meta, function(x, y) {
      x["content_type"] <- y
      x
    })

  structure(question_meta, class = c("qualtdict_normalised_questions", "list"))
}

#' Generate dictionary rows from normalised metadata
#'
#' This renders the current Variable Dictionary schema plus tracer-bullet
#' identity columns. The internal normalised metadata model is allowed to
#' evolve; this function is the adapter that keeps `dict_generate()` small.
#'
#' @keywords internal
#' @noRd
variable_dictionary_from_normalised_metadata <- function(normalised_metadata,
                                                         use_semantic_name,
                                                         block_pattern,
                                                         block_sep,
                                                         semantic_name_preprocess) {
  question_meta <- normalised_metadata$questions
  unsupported_qids <- normalised_metadata$unsupported_structure_findings$qid
  question_meta <- question_meta[!names(question_meta) %in% unsupported_qids]
  if (length(question_meta) == 0) {
    return(empty_variable_dictionary_from_normalised_metadata(
      normalised_metadata,
      use_semantic_name = use_semantic_name
    ))
  }

  json <- imap(question_meta, function(qjson, qid) {
    # Clean the &nbsp; level/label fields (empty on Qualtrics). If there is only
    # one nbsp, the question is a title and does not need cleaning.
    nbsps <- map(qjson$choices, "description") == "&nbsp;"
    if (length(nbsps) != 1) {
      qjson$choices <- qjson$choices[!nbsps]
    }

    question_name <- qjson$questionName
    type <- qjson$questionType$type
    question <- qjson$questionText
    selector <- qjson$questionType$selector
    block <- qjson$block
    content_type <- qjson$content_type

    # If no subquestion or choice, treat the length as 1.
    sub_q_len <- length(qjson$subQuestions) %>% ifelse(. > 0, ., 1)
    level_len <- length(qjson$choices) %>% ifelse(. > 0, ., 1)

    # The rep_level function works on lists for dealing with SBS questions. For
    # consistency we convert to lists for non-SBS questions.
    level <- map(qjson$choices, "recode") %>%
      unlist_nm() %>%
      list()

    label <- map(qjson$choices, "description") %>%
      unlist_nm() %>%
      list()

    has_text <- which(map_lgl(qjson$choices, ~ "textEntry" %in% names(.x)))
    if (length(has_text) > 0) {
      # Add text level and labels directly after the non-text level.
      level <- add_text(level, has_text)
      label <- add_text(label, has_text)
    }

    item <- unlist(map(qjson$subQuestions, "choiceText"))
    sub_selector <- qjson$questionType$subSelector

    has_text_sub <- which(map_lgl(
      qjson$subQuestions,
      ~ "textEntry" %in% names(.x)
    ))
    if (length(has_text_sub) > 0) {
      item <- unlist(add_text(item, has_text_sub))
      sub_q_len <- sub_q_len + length(has_text_sub)
    }

    if (type == "SBS") {
      # Get number of levels in each column.
      level_len <- map(qjson$columns, "choices") %>% map_dbl(length)
      col_len <- length(qjson$columns)
      col_type <- map_chr(qjson$columns, ~ .x$questionType$selector)
      attr(col_type, "sub_selector") <-
        map_chr(qjson$columns, ~ scalar_character(.x$questionType$subSelector))
      if (col_len != 0) {
        # Zero length columns means it's a carried forward question.
        top_question <- qjson$questionText
        question <- map(qjson$columns, "questionText") %>%
          map2(length(item), rep) %>%
          map2(level_len, ~ rep_item(.x, item, .y) %>% unlist) %>%
          unlist() %>%
          paste(top_question, ., sep = " ")

        level <- map(qjson$columns, "choices") %>%
          map(~ map_chr(.x, "recode")) %>%
          map2(col_type, function(level, type) {
            if (type == "TE") {
              level <- paste(level, "TEXT", sep = "_")
            }
            level
          })

        label <- map(qjson$columns, "choices") %>%
          map(~ map_chr(.x, "description"))

        item <- unlist(map(qjson$subQuestions, "description"))
        item <- unlist(add_text(item, has_text_sub))
      }
    }

    response_column_id <- qid_recode(qid,
      col_len = col_len, col_type = col_type,
      item = item, level = level, label = label,
      choice_len = level_len,
      type = type, selector = selector,
      sub_selector = sub_selector, is_qid = TRUE
    )

    question_name <- rep(question_name, length(response_column_id))

    list(
      qid = rep(qid, length(response_column_id)),
      response_column_id = response_column_id,
      question_name = null_na(question_name),
      block = block,
      question = question,
      looping_question = NA,
      item = rep_item(item, item, level_len) %>% null_na(),
      level = rep_level(level, item) %>% null_na(),
      label = rep_level(label, item) %>% null_na(),
      type = type,
      selector = selector,
      content_type = content_type,
      sub_selector = null_na(sub_selector),
      looping_option = NA,
      looping = all(!is.null(qjson$looping_qid))
    )
  }) %>%
    discard(is.null) %>%
    rep_loop(question_meta) %>%
    to_dataframe() %>%
    convert_html()

  json$looping <- as.logical(json$looping)

  if (use_semantic_name) {
    json <- semantic_name_gen(
      json,
      normalised_metadata$surveyID,
      block_pattern,
      block_sep,
      semantic_name_preprocess
    )
  }

  # Remove duplicated question text in item. This is useful in generating easy
  # names.
  json$item[json$item == json$question] <- NA
  json$qid <- unname(json$qid)
  json$response_column_id <- unname(json$response_column_id)
  json$question_name <- unname(json$question_name)
  if (use_semantic_name && !"semantic_name" %in% names(json)) {
    json$semantic_name <- NA_character_
  }
  if (!use_semantic_name) {
    json$variable_name <- json$question_name
  }
  json$variable_name <- unname(json$variable_name)
  json$loop_option <- json$looping_option

  # Add questions with Loop and Merge placeholders replaced with labels.
  looping_questions <- json$looping_question
  json$question[!is.na(looping_questions)] <-
    looping_questions[!is.na(looping_questions)]

  attr(json, "survey_name") <- normalised_metadata$survey_name
  attr(json, "surveyID") <- normalised_metadata$surveyID
  attr(json, "unsupported_structure_findings") <-
    normalised_metadata$unsupported_structure_findings
  json <- repair_variable_dictionary_names(json)

  json
}

#' Empty Variable Dictionary from normalised metadata
#' @keywords internal
#' @noRd
empty_variable_dictionary_from_normalised_metadata <- function(
    normalised_metadata,
    use_semantic_name = FALSE) {
  json <- tibble(
    qid = character(),
    response_column_id = character(),
    question_name = character(),
    variable_name = character(),
    block = character(),
    question = character(),
    looping_question = character(),
    item = character(),
    level = character(),
    label = character(),
    type = character(),
    selector = character(),
    content_type = character(),
    sub_selector = character(),
    looping_option = character(),
    looping = logical(),
    loop_option = character()
  )
  if (use_semantic_name) {
    json <- json[c(
      "qid", "response_column_id", "question_name", "variable_name",
      "block", "question", "looping_question", "item", "level", "label",
      "type", "selector", "content_type", "sub_selector", "looping_option",
      "looping", "loop_option"
    )]
    json$semantic_name <- character()
    json <- json[c(
      "qid", "response_column_id", "question_name", "semantic_name",
      "variable_name", "block", "question", "looping_question", "item",
      "level", "label", "type", "selector", "content_type", "sub_selector",
      "looping_option", "looping", "loop_option"
    )]
  }

  attr(json, "survey_name") <- normalised_metadata$survey_name
  attr(json, "surveyID") <- normalised_metadata$surveyID
  attr(json, "unsupported_structure_findings") <-
    normalised_metadata$unsupported_structure_findings
  attr(json, "variable_name_findings") <- empty_variable_name_findings()

  json
}

#' Make Variable Dictionary names export-safe and unique
#' @keywords internal
#' @noRd
repair_variable_names <- function(candidates) {
  make.unique(repair_variable_name_base(candidates))
}

#' Make Variable Dictionary names export-safe without uniqueness repair
#' @keywords internal
#' @noRd
repair_variable_name_base <- function(candidates) {
  repaired <- str_replace_all(candidates, "[^0-9A-Za-z_\\.]", "_")
  repaired <- str_replace_all(repaired, "_+", "_")
  repaired <- str_replace_all(repaired, "^_+|_+$", "")
  repaired[is.na(repaired) | repaired == ""] <- "variable"
  make.names(repaired, unique = FALSE)
}

#' Empty repaired-name Validation Findings table
#' @keywords internal
#' @noRd
empty_variable_name_findings <- function() {
  tibble(
    response_column_id = character(),
    original_candidate = character(),
    variable_name = character(),
    reason = character()
  )
}

#' Repair final Variable Dictionary names and attach Validation Findings
#' @keywords internal
#' @noRd
repair_variable_dictionary_names <- function(dict) {
  response_column_id <- unique(dict[["response_column_id"]])
  original_candidates <- vapply(response_column_id, function(id) {
    dict[["variable_name"]][match(id, dict[["response_column_id"]])]
  }, character(1))
  repaired_candidates <- repair_variable_names(original_candidates)
  reasons <- map2_chr(original_candidates, repaired_candidates,
    variable_name_repair_reason
  )

  findings <- tibble(
    response_column_id = response_column_id,
    original_candidate = original_candidates,
    variable_name = repaired_candidates,
    reason = reasons
  ) %>%
    filter(!is.na(.data$reason))

  name_map <- setNames(repaired_candidates, response_column_id)
  dict[["variable_name"]] <-
    unname(name_map[dict[["response_column_id"]]])

  attr(dict, "variable_name_findings") <- findings
  dict
}

#' Explain a Variable Name repair
#' @keywords internal
#' @noRd
variable_name_repair_reason <- function(original_candidate,
                                        repaired_candidate) {
  base_repair <- repair_variable_name_base(original_candidate)

  reasons <- character()
  if (!identical(original_candidate, base_repair)) {
    reasons <- c(reasons, "unsafe")
  }
  if (!identical(base_repair, repaired_candidate)) {
    reasons <- c(reasons, "duplicate")
  }

  if (length(reasons) == 0) {
    return(NA_character_)
  }

  paste(reasons, collapse = ";")
}
