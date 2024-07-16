questiontext_qid <- function(qid,
                             col_len,
                             item,
                             level,
                             label,
                             choice_len,
                             col_type) {
  qid
}

add_text_mc <- function(new_qid, level) {
  text_pos <- grep("TEXT", level)
  if (!is.null(text_pos)) {
    new_qid[text_pos] <- paste(new_qid, names(level), sep = "_")[text_pos]
  }
  new_qid
}

suf_level_qid <- function(qid,
                          col_len,
                          item,
                          level,
                          label,
                          choice_len,
                          col_type) {
  add_text_mc(paste(qid, level, sep = "_"), level)
}

suf_nmlabel_qid <- function(qid,
                            col_len,
                            item,
                            level,
                            label,
                            choice_len,
                            col_type) {
  # Same as level?
  paste(qid, names(label), sep = "_")
}

suf_text_qid <- function(qid,
                         col_len,
                         item,
                         level,
                         label,
                         choice_len,
                         col_type) {
  paste(qid, "TEXT", sep = "_")
}

rep_level_qid <- function(qid,
                          col_len,
                          item,
                          level,
                          label,
                          choice_len,
                          col_type) {
  add_text_mc(rep(qid, length(level)), level)
}

suf_item_suf_level_qid <- function(qid,
                                   col_len,
                                   item,
                                   level,
                                   label,
                                   choice_len,
                                   col_type) {
  paste_narm(qid, names(item), sep = "_") %>%
    map(paste, level, sep = "_") %>%
    unlist()
}

suf_level_suf_item_qid <- function(qid,
                                   col_len,
                                   item,
                                   level,
                                   label,
                                   choice_len,
                                   col_type) {
  paste_narm(qid, level, sep = "_") %>%
    map(paste, names(item), sep = "_") %>%
    unlist()
}

suf_item_rep_level_qid <- function(qid,
                                   col_len,
                                   item,
                                   level,
                                   label,
                                   choice_len,
                                   col_type) {
  paste_narm(qid, names(item), sep = "_") %>%
    rep_qid(item, choice_len)
}

item_or_level_qid <- function(qid,
                              col_len,
                              item,
                              level,
                              label,
                              choice_len,
                              col_type) {
  if (is.null(item)) {
    suf_level_qid(
      qid,
      col_len,
      item,
      level,
      label,
      choice_len,
      col_type
    )
  } else {
    suf_item_rep_level_qid(
      qid,
      col_len,
      item,
      level,
      label,
      choice_len,
      col_type
    )
  }
}

text <- function(qid,
                 col_len,
                 item,
                 level,
                 label,
                 choice_len) {
  paste(qid, "TEXT", sep = "_")
}

sbs_qid <- function(qid,
                    col_len,
                    item,
                    level,
                    label,
                    choice_len,
                    col_type) {
  if (length(col_type) == 0) {
    # Carried forward question
    return(qid)
  }

  names(choice_len) <- col_type
  paste(qid, sep = "#", seq(col_len)) %>%
    map2(length(item), rep) %>%
    map(paste, names(item), sep = "_") %>%
    map2(choice_len, ~ rep_item(.x, item, .y) %>% unlist()) %>%
    map2(col_type, function(qids, type) {
      if (type == "TE") {
        qids <- paste(qids, 1:choice_len[type], sep = "_") %>%
          str_replace("_TEXT_\\d$", "_TEXT")
        return(qids)
      } else {
        return(qids)
      }
    }) %>%
    unlist()
}

timing_qid <- function(qid,
                       col_len,
                       item,
                       level,
                       label,
                       choice_len,
                       col_type) {
  paste0(qid, c(
    "_FIRST_CLICK", "_LAST_CLICK", "_PAGE_SUBMIT",
    "_CLICK_COUNT"
  ))
}

file_upload_qid <- function(qid,
                            col_len,
                            item,
                            level,
                            label,
                            choice_len,
                            col_type) {
  paste0(qid, c(
    "_FILE_ID", "_FILE_NAME", "_FILE_SIZE", "_FILE_TYPE"
  ))
}

not_applicable_qid <- function(qid,
                               col_len,
                               item,
                               level,
                               label,
                               choice_len,
                               col_type) {
  warn_msg <- paste0(
    qid, " is an unsupported type of question."
  )
  warning(warn_msg)
  qid
}

qid_recode <- function(qid,
                       col_len,
                       item,
                       level,
                       label,
                       choice_len,
                       col_type,
                       type,
                       selector,
                       sub_selector,
                       is_qid) {

  recode_list <- list(
    MC =
      list(
        MACOL = list(TX = suf_level_qid),
        MAVR = list(TX = suf_level_qid),
        MAHR = list(TX = suf_level_qid),
        MSB = suf_level_qid,
        SAVR = list(TX = rep_level_qid),
        SACOL = list(TX = rep_level_qid),
        DL = rep_level_qid,
        SAHR = list(TX = rep_level_qid),
        SB = rep_level_qid,
        NPS = rep_level_qid
      ),
    Matrix =
      list(
        Likert = list(
          MultipleAnswer = suf_item_suf_level_qid,
          DL = suf_item_rep_level_qid,
          SingleAnswer = item_or_level_qid,
          DND = item_or_level_qid,
          SACV = item_or_level_qid,
          SACH = item_or_level_qid,
          SACCOL = item_or_level_qid
        ),
        TE = list(
          Short = suf_item_suf_level_qid,
          Medium = suf_item_suf_level_qid,
          Long = suf_item_suf_level_qid
        ),
        Profile = list(
          SingleAnswer = suf_item_rep_level_qid,
          DL = suf_item_rep_level_qid
        ),
        Bipolar = suf_item_rep_level_qid,
        RO = suf_item_suf_level_qid,
        MaxDiff = suf_item_rep_level_qid,
        CS = list(WTB = suf_item_suf_level_qid)
      ),
    Slider = list(
      HSLIDER = suf_item_rep_level_qid,
      HBAR = suf_level_qid,
      STAR = suf_level_qid
    ),
    CS = list(
      HR = list(TX = item_or_level_qid),
      VRTL = list(TX = item_or_level_qid),
      HBAR = item_or_level_qid,
      HSLIDER = item_or_level_qid
    ),
    TE = list(
      FORM = suf_nmlabel_qid,
      SL = suf_text_qid,
      ML = suf_text_qid,
      ESTB = suf_text_qid
    ),
    SBS = list(SBSMatrix = sbs_qid),
    Timing = list(PageTimer = timing_qid),
    SS = list(TA = rep_level_qid),
    FileUpload = list(FileUpload = file_upload_qid),
    PGR = list(DragAndDrop = list(NoColumns = not_applicable_qid)),
    DD = list(DL = suf_item_rep_level_qid),
    Draw = list(Signature = file_upload_qid),
    HL = list(Text = suf_level_suf_item_qid),
    Meta = list(Browser = not_applicable_qid),
    DB = list(
      TB = questiontext_qid,
      PTB = questiontext_qid,
      FLB = questiontext_qid,
      GRB = list(
        WTXB = questiontext_qid,
        WOTXB = questiontext_qid
      )
    )
  )

  if (type != "SBS") {
    level <- level[[1]]
    label <- label[[1]]
  }

  if (!is.null(selector)) {
    if (!is.null(sub_selector)) {
      recode_func <- recode_list[[type]][[selector]][[sub_selector]]
    } else {
      recode_func <- recode_list[[type]][[selector]]
    }
  } else {
    recode_func <- recode_list[[type]]
  }

  if (is.null(recode_func)) {
    recode_func <- not_applicable_qid
  }

  new_qid <- recode_func(
    qid,
    col_len,
    item,
    level,
    label,
    choice_len,
    col_type
  )

  return(new_qid)
}
