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
