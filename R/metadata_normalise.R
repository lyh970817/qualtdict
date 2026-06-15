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

#' Build one package-owned normalised question fact
#' @keywords internal
#' @noRd
normalise_question_fact <- function(qid, question, block, content_type) {
  question_name <- scalar_character(question$questionName)
  question_text <- scalar_character(question$questionText)
  question_type <- question_fact_question_type(question)
  survey_block <- scalar_character(block$description)
  response_choices <- normalise_response_choices(question$choices)
  response_items <- normalise_response_items(question$subQuestions)
  column_facts <- normalise_column_facts(question$columns)
  looping_prefix <- block$looping_prefix
  looping_qid <- block$looping_qid

  structure(
    list(
      qid = qid,
      question_name = question_name,
      question_text = question_text,
      question_type = question_type,
      survey_block = survey_block,
      content_type = content_type,
      response_choices = response_choices,
      response_items = response_items,
      column_facts = column_facts,
      looping_prefix = looping_prefix,
      looping_qid = looping_qid
    ),
    class = c("qualtdict_normalised_question", "list")
  )
}

#' Build package-owned response choice facts
#' @keywords internal
#' @noRd
normalise_response_choices <- function(choices) {
  imap(choices, function(choice, choice_id) {
    label <- scalar_character(choice$label %||% choice$description)
    text_entry <- "text_entry" %in% names(choice) ||
      "textEntry" %in% names(choice)

    list(
      choice_id = choice_id,
      level = scalar_character(choice$level %||% choice$recode),
      label = label,
      text_entry = text_entry,
      recode = scalar_character(choice$level %||% choice$recode),
      description = label,
      textEntry = if (text_entry) TRUE else NULL
    )
  })
}

#' Build package-owned response item facts
#' @keywords internal
#' @noRd
normalise_response_items <- function(items) {
  imap(items, function(item, item_id) {
    item_text <- scalar_character(item$item_text %||% item$choiceText)
    item_label <- scalar_character(item$item_label %||% item$description)
    text_entry <- "text_entry" %in% names(item) ||
      "textEntry" %in% names(item)

    list(
      item_id = item_id,
      item_text = item_text,
      item_label = item_label,
      text_entry = text_entry,
      recode = scalar_character(item$level %||% item$recode),
      choiceText = item_text,
      description = item_label,
      textEntry = if (text_entry) TRUE else NULL
    )
  })
}

#' Build package-owned SBS column facts
#' @keywords internal
#' @noRd
normalise_column_facts <- function(columns) {
  imap(columns, function(column, column_id) {
    question_type <- question_fact_question_type(column)

    list(
      column_id = column_id,
      question_text = scalar_character(column$question_text %||%
        column$questionText),
      question_type = question_type,
      response_choices = normalise_response_choices(column$choices)
    )
  })
}

#' Return first non-NULL value
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' Return a question fact field with optional legacy fallback
#' @keywords internal
#' @noRd
question_fact_value <- function(question, owned_name, legacy_name = NULL) {
  value <- question[[owned_name]]
  if (is.null(value) && !is.null(legacy_name)) {
    value <- question[[legacy_name]]
  }

  value
}

#' Return package-owned question type facts
#' @keywords internal
#' @noRd
question_fact_question_type <- function(question) {
  question_type <- question_fact_value(question, "question_type", "questionType")
  if (is.null(question_type)) {
    return(list(type = NULL, selector = NULL, sub_selector = NULL))
  }

  sub_selector <- question_type$sub_selector
  if (is.null(sub_selector)) {
    sub_selector <- question_type$subSelector
  }

  list(
    type = scalar_character(question_type$type),
    selector = scalar_character(question_type$selector),
    sub_selector = if (is.null(sub_selector) ||
      (length(sub_selector) == 1 && is.na(sub_selector))) {
        NULL
      } else {
        scalar_character(sub_selector)
      }
  )
}

#' Return a package-owned question name
#' @keywords internal
#' @noRd
question_fact_question_name <- function(question) {
  question_fact_value(question, "question_name", "questionName")
}

#' Return a package-owned question text
#' @keywords internal
#' @noRd
question_fact_question_text <- function(question) {
  question_fact_value(question, "question_text", "questionText")
}

#' Return a package-owned survey block
#' @keywords internal
#' @noRd
question_fact_survey_block <- function(question) {
  question_fact_value(question, "survey_block", "block")
}

#' Return package-owned response choices
#' @keywords internal
#' @noRd
question_fact_response_choices <- function(question) {
  question_fact_value(question, "response_choices", "choices")
}

#' Return package-owned response items
#' @keywords internal
#' @noRd
question_fact_response_items <- function(question) {
  question_fact_value(question, "response_items", "subQuestions")
}

#' Return package-owned column facts
#' @keywords internal
#' @noRd
question_fact_column_facts <- function(question) {
  question_fact_value(question, "column_facts", "columns")
}

#' Return package-owned Loop and Merge prefix facts
#' @keywords internal
#' @noRd
question_fact_looping_prefix <- function(question) {
  question_fact_value(question, "looping_prefix")
}

#' Return package-owned Loop and Merge source facts
#' @keywords internal
#' @noRd
question_fact_looping_qid <- function(question) {
  question_fact_value(question, "looping_qid")
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

  text_analysis <- is.na(classification) &
    grepl("^(.+_)?QID[0-9]+(_[0-9]+)?_TEXT_.+", raw_columns)
  classification[text_analysis] <- "text_analysis"
  details[text_analysis] <-
    "Qualtrics text-analysis sidecar column; not represented as a question dictionary row."

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
    looping_qid <- scalar_character(question_fact_looping_qid(question))
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
  question_type <- question_fact_question_type(question)

  tibble(
    qid = qid,
    type = scalar_character(question_type$type),
    selector = scalar_character(question_type$selector),
    sub_selector = scalar_character(question_type$sub_selector),
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

  question_meta <- imap(question_meta, function(question, qid) {
    block <- block_meta[[qid]]
    if (is.null(block)) {
      block <- list(
        description = NA_character_,
        looping_prefix = character(),
        looping_qid = NA_character_
      )
    }

    normalise_question_fact(
      qid = qid,
      question = question,
      block = block,
      content_type = content_type_meta[[qid]]
    )
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
    question_type <- question_fact_question_type(qjson)
    question_name <- question_fact_question_name(qjson)
    type <- question_type$type
    selector <- question_type$selector
    block <- question_fact_survey_block(qjson)
    content_type <- qjson$content_type
    sub_selector <- question_type$sub_selector
    response_columns <- render_response_columns(qjson, qid)

    question_name <- rep(
      question_name,
      length(response_columns$response_column_id)
    )

    list(
      qid = rep(qid, length(response_columns$response_column_id)),
      response_column_id = response_columns$response_column_id,
      question_name = null_na(question_name),
      block = block,
      question = response_columns$question,
      looping_question = NA,
      item = response_columns$item,
      level = response_columns$level,
      label = response_columns$label,
      type = type,
      selector = selector,
      content_type = content_type,
      sub_selector = null_na(sub_selector),
      looping_option = NA,
      looping = !is.na(scalar_character(question_fact_looping_qid(qjson)))
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
