#' Download metadata in json format and recode into dictionary format
#' @keywords internal
#' @noRd
recode_json <- function(surveyID,
                        use_semantic_name,
                        block_pattern,
                        block_sep,
                        semantic_name_preprocess) {
  survey_metadata <- fetch_dictionary_metadata(surveyID)
  normalised_metadata <- normalise_qualtrics_metadata(survey_metadata)

  variable_dictionary_from_normalised_metadata(
    normalised_metadata,
    use_semantic_name = use_semantic_name,
    block_pattern = block_pattern,
    block_sep = block_sep,
    semantic_name_preprocess = semantic_name_preprocess
  )
}

add_text <- function(x, has_text, label = FALSE) {
  x <- unlist(x)
  if (!is.null(x)) {
    for (i in seq_along(has_text)) {
      pos <- has_text[i] + (i - 1)
      text <- names(x)[pos]
      text_nm <- x[pos]
      x <- append(x,
        paste(text_nm, sep = "_", "TEXT"),
        after = pos
      )

      # Required for sub
      names(x)[pos + 1] <- paste(text, sep = "_", "TEXT")
    }
    return(list(x))
  }
}

rep_qid <- function(qid, item, choice_len) {
  if (is.null(item)) {
    return(rep(qid, times = choice_len))
  }
  map2(qid, names(item), function(id, nam) {
    if (!grepl("TEXT", nam)) {
      return(rep(id, each = choice_len))
    } else {
      return(id)
    }
  }) %>%
    unlist()
}

rep_item <- function(x, item, choice_len) {
  map(choice_len, function(c) {
    map2(item, x, function(itm, x) {
      if (!grepl("TEXT", itm)) {
        return(rep(x, each = c))
      } else {
        return(x)
      }
    })%>%
      unlist()
  }) }

rep_level <- function(level, item) {
  if (is.null(item)) {
    return(unlist(level))
  }

  map(level, function(l) {
    imap(item, function(itm, nam) {
      if (!grepl("TEXT", nam)) {
        return(l)
      } else {
        return("TEXT")
      }
    }) %>%
    unlist(recursive = FALSE)
  }) 
}

rep_loop <- function(x, question_meta) {
  looping_qids_meta <- unlist(map(question_meta, "looping_qid"))
  imap(x, function(qmeta, name) {
    if (qmeta$looping) {
      looping_qid <- looping_qids_meta[name]
      looping_qmeta <- x[[looping_qid]]
      static_prefixes <- unlist(
        question_meta[[name]][["looping_prefix"]],
        use.names = FALSE
      )

      # Get loop option and prefixes (names) generated
      # in `recode_json` (remove _TEXT)
      if (looping_qmeta[["type"]] == "Matrix") {
        loop_options <- unlist(looping_qmeta[["item"]])
        loop_prefixes <- sub(
          paste0("^", looping_qids_meta[name], "_"),
          "",
          unlist(looping_qmeta[["qid"]])
        )
        keep <- !grepl("_TEXT", loop_prefixes) & !duplicated(loop_prefixes)
        loop_options <- loop_options[keep]
        names(loop_options) <- loop_prefixes[keep]
      } else {
        loop_options <- map(looping_qmeta[["label"]], ~ map_chr(.x, 1)) %>%
          unlist() %>%
          discard(grepl("_TEXT", names(.)))

        static_loop_options <- loop_options_from_static_choices(
          question_meta[[looping_qid]][["choices"]],
          static_prefixes
        )
        if (!is.null(static_loop_options)) {
          loop_options <- static_loop_options
        }
      }

      imap(loop_options, function(option, prefix) {
        if ("response_column_id" %in% names(qmeta)) {
          qmeta[["response_column_id"]] <-
            unname(paste(prefix, qmeta[["response_column_id"]], sep = "_"))
          qmeta[["response_column_id"]] <- loop_response_column_id(
            qmeta[["response_column_id"]]
          )
        } else if ("qid" %in% names(qmeta)) {
          qmeta[["qid"]] <- unname(paste(prefix, qmeta[["qid"]], sep = "_"))
          qmeta[["qid"]] <- loop_response_column_id(qmeta[["qid"]])
        }
        if ("variable_name" %in% names(qmeta)) {
          qmeta[["variable_name"]] <-
            unname(paste(prefix, qmeta[["variable_name"]], sep = "."))
        } else if ("name" %in% names(qmeta)) {
          qmeta[["name"]] <- unname(paste(prefix, qmeta[["name"]], sep = "."))
        }
        # What about second loop (field 2))?
        qmeta[["looping_question"]] <-
          gsub("\\$\\{lm://Field/1\\}", option, qmeta[["question"]])
        qmeta[["question"]] <-
          gsub("\\$\\{lm://Field/1\\}", "{}", qmeta[["question"]])
        # To use in Semantic Name generation.
        qmeta[["looping_option"]] <- option
        return(qmeta)
      })
    } else {
      return(list(qmeta))
    }
  }) %>%
    unlist(recursive = FALSE)
}

loop_response_column_id <- function(response_column_id) {
  str_replace(
    response_column_id,
    "^([^_]+_QID[0-9]+)_[^_]+_TEXT$",
    "\\1_TEXT"
  )
}

loop_options_from_static_choices <- function(choices, static_prefixes) {
  if (is.null(choices) ||
    length(static_prefixes) == 0 ||
    !all(static_prefixes %in% names(choices))) {
    return(NULL)
  }

  loop_options <- vapply(static_prefixes, function(prefix) {
    choice <- choices[[prefix]]
    option <- choice[["description"]]
    if (is.null(option) || is.na(option) || option == "") {
      option <- choice[["choiceText"]]
    }
    as.character(option)
  }, character(1))
  names(loop_options) <- static_prefixes
  loop_options
}

to_dataframe <- function(json) {
  map_df(json, function(qmeta) {
    map_df(qmeta, unlist)
  })
}
