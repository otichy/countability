#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(shiny)
  library(lme4)
  library(ggplot2)
  library(DT)
  library(readxl)
})

options(shiny.maxRequestSize = 200 * 1024^2)

default_autoreload_pattern <- paste0(
  "(^|.*[/\\\\])(",
  "app\\.R|global\\.R|server\\.R|ui\\.R|",
  "R[/\\\\].*\\.R|modules[/\\\\].*\\.R|",
  "www[/\\\\].*\\.(html?|js|css|png|jpe?g|gif|svg)",
  ")$"
)

options(shiny.autoreload.pattern = default_autoreload_pattern)

detect_sep <- function(path) {
  first_line <- readLines(path, n = 1, warn = FALSE)
  semicolons <- lengths(regmatches(first_line, gregexpr(";", first_line, fixed = TRUE)))
  commas <- lengths(regmatches(first_line, gregexpr(",", first_line, fixed = TRUE)))
  if (semicolons >= commas) ";" else ","
}

read_text_utf8 <- function(path) {
  size <- file.info(path)$size
  raw <- readBin(path, what = "raw", n = size)
  text <- rawToChar(raw)
  text <- sub("^\ufeff", "", text, useBytes = TRUE)
  enc2utf8(text)
}

parse_delimited_text <- function(text, sep = ";") {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  rows <- list()
  current_row <- character(0)
  current_field <- character(0)
  in_quotes <- FALSE
  i <- 1L
  n <- length(chars)

  flush_field <- function() {
    current_row <<- c(current_row, paste(current_field, collapse = ""))
    current_field <<- character(0)
  }

  flush_row <- function() {
    if (length(current_row) == 0 && length(current_field) == 0) return(invisible(NULL))
    flush_field()
    rows[[length(rows) + 1L]] <<- current_row
    current_row <<- character(0)
  }

  while (i <= n) {
    ch <- chars[[i]]

    if (in_quotes) {
      if (identical(ch, "\\") && i < n) {
        next_ch <- chars[[i + 1L]]
        if (next_ch %in% c("\\", "\"", "n", "r", "t")) {
          current_field <- c(
            current_field,
            switch(next_ch, "n" = "\n", "r" = "\r", "t" = "\t", next_ch)
          )
          i <- i + 2L
          next
        }
      }

      if (identical(ch, "\"")) {
        next_ch <- if (i < n) chars[[i + 1L]] else ""
        if (identical(next_ch, "\"")) {
          current_field <- c(current_field, "\"")
          i <- i + 2L
          next
        }
        if (i == n || next_ch %in% c(sep, "\n", "\r")) {
          in_quotes <- FALSE
          i <- i + 1L
          next
        }
      }

      current_field <- c(current_field, ch)
      i <- i + 1L
      next
    }

    if (identical(ch, "\"")) {
      in_quotes <- TRUE
      i <- i + 1L
      next
    }
    if (identical(ch, sep)) {
      flush_field()
      i <- i + 1L
      next
    }
    if (identical(ch, "\r")) {
      flush_row()
      if (i < n && identical(chars[[i + 1L]], "\n")) {
        i <- i + 2L
      } else {
        i <- i + 1L
      }
      next
    }
    if (identical(ch, "\n")) {
      flush_row()
      i <- i + 1L
      next
    }

    current_field <- c(current_field, ch)
    i <- i + 1L
  }

  if (length(current_field) > 0 || length(current_row) > 0) flush_row()
  if (length(rows) == 0) return(data.frame())

  header <- rows[[1]]
  n_cols <- length(header)
  normalize_row <- function(x) {
    if (length(x) < n_cols) {
      c(x, rep("", n_cols - length(x)))
    } else {
      x[seq_len(n_cols)]
    }
  }

  body <- lapply(rows[-1], normalize_row)
  if (length(body) == 0) {
    out <- as.data.frame(
      stats::setNames(replicate(n_cols, character(0), simplify = FALSE), header),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    mat <- do.call(rbind, body)
    out <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
    names(out) <- header
  }

  out[] <- lapply(out, function(col) type.convert(col, as.is = TRUE))
  out
}

read_dataset <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    sep <- detect_sep(path)
    if (requireNamespace("data.table", quietly = TRUE)) {
      return(data.table::fread(
        path,
        sep = sep,
        data.table = FALSE,
        encoding = "UTF-8",
        na.strings = "NA",
        showProgress = FALSE
      ))
    }
    return(utils::read.csv(
      path,
      sep = sep,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8-BOM",
      na.strings = "NA",
      comment.char = ""
    ))
  }
  if (ext %in% c("xlsx", "xls")) {
    return(as.data.frame(read_excel(path)))
  }
  stop("Unsupported file type. Please use CSV or XLSX.")
}

encode_filter_tokens <- function(x) {
  x_chr <- as.character(x)
  out <- paste0("value::", x_chr)
  out[is.na(x)] <- "__NA__"
  out[!is.na(x) & x_chr == ""] <- "__EMPTY__"
  out
}

format_filter_labels <- function(x) {
  out <- as.character(x)
  out[is.na(x)] <- "<NA>"
  out[!is.na(x) & out == ""] <- "<empty>"
  out
}

filter_choice_vector <- function(x) {
  if (length(x) == 0) return(setNames(character(0), character(0)))

  tbl <- filter_choice_table(x)
  out <- tbl$token
  names(out) <- tbl$label
  out
}

filter_choice_table <- function(x) {
  if (length(x) == 0) {
    return(data.frame(
      token = character(0),
      label = character(0),
      n = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  tokens <- encode_filter_tokens(x)
  labels <- format_filter_labels(x)
  keep <- !duplicated(tokens)
  out <- data.frame(
    token = tokens[keep],
    label = labels[keep],
    stringsAsFactors = FALSE
  )
  out$n <- as.integer(tabulate(match(tokens, out$token), nbins = nrow(out)))
  out[order(-out$n, out$label), , drop = FALSE]
}

filter_can_collapse_to_other <- function(x) {
  is.factor(x) || is.character(x)
}

filter_choice_vector_with_counts <- function(x) {
  tbl <- filter_choice_table(x)
  if (!is.data.frame(tbl) || nrow(tbl) == 0) return(setNames(character(0), character(0)))

  out <- tbl$token
  names(out) <- sprintf("%s (%s)", tbl$label, format(tbl$n, big.mark = ","))
  out
}

filter_effect_counts <- function(x, mode = "exclude", values = character(0), collapse_to_other = FALSE) {
  mode <- as.character(mode %||% "exclude")
  if (!mode %in% c("include", "exclude")) mode <- "exclude"
  values <- unique(as.character(values %||% character(0)))

  total_n <- length(x)
  if (length(values) == 0) {
    return(list(
      total_n = total_n,
      selected_n = 0L,
      kept_n = total_n,
      other_n = 0L,
      filtered_out_n = 0L
    ))
  }

  matches <- encode_filter_tokens(x) %in% values
  matches[is.na(matches)] <- FALSE
  selected_n <- sum(matches)

  if (isTRUE(collapse_to_other) && filter_can_collapse_to_other(x)) {
    other_n <- if (identical(mode, "include")) total_n - selected_n else selected_n
    return(list(
      total_n = total_n,
      selected_n = selected_n,
      kept_n = total_n,
      other_n = other_n,
      filtered_out_n = 0L
    ))
  }

  if (identical(mode, "include")) {
    return(list(
      total_n = total_n,
      selected_n = selected_n,
      kept_n = selected_n,
      other_n = 0L,
      filtered_out_n = total_n - selected_n
    ))
  }

  list(
    total_n = total_n,
    selected_n = selected_n,
    kept_n = total_n - selected_n,
    other_n = 0L,
    filtered_out_n = selected_n
  )
}

format_filter_effect_counts <- function(counts) {
  sprintf(
    "kept %s; other %s; filtered out %s",
    format(counts$kept_n %||% 0, big.mark = ","),
    format(counts$other_n %||% 0, big.mark = ","),
    format(counts$filtered_out_n %||% 0, big.mark = ",")
  )
}

filter_replace_spec <- function(spec) {
  pattern <- as.character(spec$search_pattern %||% "")
  replacement <- as.character(spec$replacement %||% "")
  list(
    search_pattern = pattern,
    replacement = replacement,
    ignore_case = isTRUE(spec$ignore_case),
    active = nzchar(pattern)
  )
}

filter_regex_is_valid <- function(pattern) {
  pattern <- as.character(pattern %||% "")
  if (!nzchar(pattern)) return(TRUE)
  tryCatch({
    grepl(pattern, "", perl = TRUE)
    TRUE
  }, error = function(e) FALSE)
}

apply_filter_replacement <- function(x, spec) {
  repl <- filter_replace_spec(spec)
  if (!isTRUE(repl$active) || !filter_regex_is_valid(repl$search_pattern)) return(x)

  out <- as.character(x)
  missing <- is.na(out)
  out[!missing] <- gsub(
    repl$search_pattern,
    repl$replacement,
    out[!missing],
    perl = TRUE,
    ignore.case = isTRUE(repl$ignore_case)
  )
  out[missing] <- NA_character_
  out
}

filter_replacement_counts <- function(x, spec) {
  repl <- filter_replace_spec(spec)
  total_n <- length(x)
  if (!isTRUE(repl$active) || !filter_regex_is_valid(repl$search_pattern)) {
    return(list(total_n = total_n, matched_n = 0L, distinct_before = length(unique(encode_filter_tokens(x))), distinct_after = length(unique(encode_filter_tokens(x)))))
  }

  x_chr <- as.character(x)
  missing <- is.na(x_chr)
  matches <- rep(FALSE, length(x_chr))
  matches[!missing] <- grepl(
    repl$search_pattern,
    x_chr[!missing],
    perl = TRUE,
    ignore.case = isTRUE(repl$ignore_case)
  )
  recoded <- apply_filter_replacement(x, spec)
  list(
    total_n = total_n,
    matched_n = sum(matches),
    distinct_before = length(unique(encode_filter_tokens(x))),
    distinct_after = length(unique(encode_filter_tokens(recoded)))
  )
}

normalize_dataset_filters <- function(filters) {
  if (!is.list(filters) || length(filters) == 0) return(list())

  out <- list()
  for (col in names(filters)) {
    spec <- filters[[col]]
    if (!is.list(spec)) next

    mode <- as.character(spec$mode %||% "exclude")
    if (!mode %in% c("include", "exclude")) mode <- "exclude"

    values <- unique(as.character(spec$values %||% character(0)))

    collapse_to_other <- isTRUE(spec$collapse_to_other)
    replacement <- filter_replace_spec(spec)
    if (length(values) == 0 && !isTRUE(replacement$active)) next

    out[[col]] <- list(
      mode = mode,
      values = values,
      collapse_to_other = collapse_to_other,
      search_pattern = replacement$search_pattern,
      replacement = replacement$replacement,
      ignore_case = isTRUE(replacement$ignore_case)
    )
  }

  out
}

apply_dataset_filters <- function(df, filters) {
  filters <- normalize_dataset_filters(filters)
  if (length(filters) == 0) return(df)

  out <- df
  for (col in names(filters)) {
    if (!col %in% names(out)) next

    spec <- filters[[col]]
    out[[col]] <- apply_filter_replacement(out[[col]], spec)

    if (length(spec$values) > 0 && isTRUE(spec$collapse_to_other) && filter_can_collapse_to_other(out[[col]])) {
      matches <- encode_filter_tokens(out[[col]]) %in% spec$values
      matches[is.na(matches)] <- FALSE
      recoded <- as.character(out[[col]])
      if (identical(spec$mode, "include")) {
        recoded[!matches] <- "other"
      } else {
        recoded[matches] <- "other"
      }
      out[[col]] <- recoded
    } else if (length(spec$values) > 0) {
      matches <- encode_filter_tokens(out[[col]]) %in% spec$values
      keep <- if (identical(spec$mode, "include")) matches else !matches
      keep[is.na(keep)] <- FALSE
      out <- out[keep, , drop = FALSE]
    }
  }

  out
}

describe_dataset_filters <- function(df, filters, max_values = 4L) {
  filters <- normalize_dataset_filters(filters)
  if (length(filters) == 0) return(character(0))

  vapply(names(filters), function(col) {
    if (!col %in% names(df)) return(sprintf("%s: filter unavailable", col))

    spec <- filters[[col]]
    replaced_col <- apply_filter_replacement(df[[col]], spec)
    choice_tbl <- filter_choice_table(replaced_col)
    idx <- match(spec$values, choice_tbl$token)
    labels <- ifelse(
      !is.na(idx),
      sprintf("%s (%s)", choice_tbl$label[idx], format(choice_tbl$n[idx], big.mark = ",")),
      spec$values
    )

    if (length(labels) > max_values) {
      labels <- c(labels[seq_len(max_values)], sprintf("+%d more", length(labels) - max_values))
    }

    parts <- character(0)
    if (length(spec$values) > 0) {
      action <- if (isTRUE(spec$collapse_to_other) && identical(spec$mode, "include")) {
        "keep selected and collapse the rest to other"
      } else if (isTRUE(spec$collapse_to_other)) {
        "collapse selected to other"
      } else if (identical(spec$mode, "include")) {
        "keep only"
      } else {
        "exclude"
      }
      counts <- filter_effect_counts(
        x = replaced_col,
        mode = spec$mode,
        values = spec$values,
        collapse_to_other = spec$collapse_to_other
      )
      parts <- c(parts, sprintf(
        "%s %s [%s]",
        action,
        paste(labels, collapse = ", "),
        format_filter_effect_counts(counts)
      ))
    }
    if (nzchar(spec$search_pattern %||% "")) {
      repl_counts <- filter_replacement_counts(df[[col]], spec)
      parts <- c(parts, sprintf(
        "first replace /%s/ with %s [%s rows; distinct values %s -> %s]",
        spec$search_pattern,
        if (nzchar(spec$replacement %||% "")) spec$replacement else "<empty>",
        format(repl_counts$matched_n, big.mark = ","),
        format(repl_counts$distinct_before, big.mark = ","),
        format(repl_counts$distinct_after, big.mark = ",")
      ))
    }
    sprintf("%s: %s", col, paste(parts, collapse = "; "))
  }, character(1), USE.NAMES = FALSE)
}

describe_dataset_preprocessing <- function(source_df, filtered_df, filters, max_values = 6L) {
  filters <- normalize_dataset_filters(filters)
  if (length(filters) == 0) return(character(0))

  filter_lines <- describe_dataset_filters(source_df, filters, max_values = max_values)
  out <- c(
    sprintf("Dataset preprocessing before model fitting: %s.", paste(filter_lines, collapse = "; "))
  )

  source_n <- if (is.data.frame(source_df)) nrow(source_df) else NA_integer_
  filtered_n <- if (is.data.frame(filtered_df)) nrow(filtered_df) else NA_integer_
  if (!is.na(source_n) && !is.na(filtered_n)) {
    if (filtered_n == source_n) {
      out <- c(out, sprintf("These changes retained all %s rows.", format(filtered_n, big.mark = ",")))
    } else {
      out <- c(
        out,
        sprintf(
          "These changes reduced the dataset from %s to %s rows.",
          format(source_n, big.mark = ","),
          format(filtered_n, big.mark = ",")
        )
      )
    }
  }

  out
}

`%||%` <- function(x, y) if (is.null(x)) y else x

make_interaction_choices <- function(fixed_effects) {
  fx <- unique(fixed_effects)
  if (length(fx) < 2) return(character(0))
  pairs <- combn(fx, 2, simplify = FALSE)
  vapply(pairs, function(x) paste(x, collapse = ":"), character(1))
}

build_formula <- function(fixed_effects, interaction_terms, random_effects, response_info = NULL) {
  if (length(fixed_effects) == 0) stop("Select at least one fixed effect.")

  fixed_main <- unique(fixed_effects)
  fixed_interactions <- unique(interaction_terms)
  fixed_part <- paste(c(fixed_main, fixed_interactions), collapse = " + ")

  random_part <- character(0)
  if (length(random_effects) > 0) {
    random_part <- sprintf("(1 | %s)", random_effects)
  }

  rhs <- paste(c(fixed_part, random_part), collapse = " + ")
  response_info <- normalize_response_info(response_info)
  lhs <- if (identical(response_info$family, "multinomial")) {
    "response_value"
  } else {
    "cbind(plural_successes, plural_failures)"
  }
  as.formula(paste(lhs, "~", rhs))
}

is_integer_like <- function(x, tol = 1e-8) {
  !is.na(x) & is.finite(x) & abs(x - round(x)) <= tol
}

list_response_candidates <- function(df) {
  as_numeric_col <- function(name) {
    if (!name %in% names(df)) return(NULL)
    suppressWarnings(as.numeric(df[[name]]))
  }

  as_text_col <- function(name) {
    if (!name %in% names(df)) return(NULL)
    if (is.numeric(df[[name]])) return(NULL)
    vals <- trimws(as.character(df[[name]]))
    vals[is.na(vals)] <- ""
    vals
  }

  binary_ok <- function(x) {
    vals <- x[!is.na(x)]
    length(vals) > 0 && all(vals %in% c(0, 1))
  }

  count_ok <- function(x) {
    vals <- x[!is.na(x)]
    length(vals) > 0 && all(vals >= 0 & is_integer_like(vals))
  }

  make_binary_spec <- function(col) {
    vals <- as_numeric_col(col)
    if (is.null(vals) || !binary_ok(vals)) return(NULL)

    if (col %in% c("plural", "pl")) {
      success_label <- "plural"
      failure_label <- "singular"
      label <- sprintf("Plural (`%s`, binary 0/1)", col)
    } else if (col == "sg") {
      success_label <- "singular"
      failure_label <- "plural"
      label <- "Singular (`sg`, binary 0/1)"
    } else {
      success_label <- sprintf("%s = 1", col)
      failure_label <- sprintf("%s = 0", col)
      label <- sprintf("%s (binary 0/1)", col)
    }

    list(
      key = paste0("binary::", col),
      family = "binomial",
      mode = "binary_rows",
      label = label,
      outcome_cols = col,
      binary_col = col,
      success_col = col,
      success_label = success_label,
      failure_label = failure_label
    )
  }

  make_binary_text_spec <- function(col) {
    vals <- as_text_col(col)
    if (is.null(vals)) return(NULL)

    nonmissing <- vals[nzchar(vals)]
    levels <- sort(unique(nonmissing))
    if (length(levels) != 2) return(NULL)

    list(
      key = paste0("binary_text::", col),
      family = "binomial",
      mode = "binary_text_rows",
      label = sprintf("%s (%s vs %s)", col, levels[[2]], levels[[1]]),
      outcome_cols = col,
      binary_col = col,
      binary_levels = levels,
      success_col = col,
      success_label = levels[[2]],
      failure_label = levels[[1]]
    )
  }

  categorical_ok <- function(col, vals) {
    nonmissing <- vals[nzchar(vals)]
    if (length(nonmissing) == 0) return(FALSE)

    levels <- unique(nonmissing)
    n_levels <- length(levels)
    if (n_levels < 3) return(FALSE)
    if (identical(col, "q_lemma")) {
      return(n_levels <= 250L)
    }

    n_obs <- length(nonmissing)
    n_levels <= 20L && (n_levels / n_obs) <= 0.3
  }

  make_categorical_spec <- function(col) {
    vals <- as_text_col(col)
    if (is.null(vals) || !categorical_ok(col, vals)) return(NULL)

    levels <- sort(unique(vals[nzchar(vals)]))
    list(
      key = paste0("categorical::", col),
      family = "multinomial",
      mode = "categorical_rows",
      label = sprintf("%s (multinomial, %d levels)", col, length(levels)),
      outcome_cols = col,
      response_col = col,
      outcome_levels = levels,
      reference_level = levels[[1]]
    )
  }

  candidates <- list()

  sg_vals <- as_numeric_col("sg")
  pl_vals <- as_numeric_col("pl")
  if (!is.null(sg_vals) && !is.null(pl_vals) && count_ok(sg_vals) && count_ok(pl_vals)) {
    totals <- sg_vals + pl_vals
    valid_totals <- totals[!is.na(totals)]
    if (length(valid_totals) > 0 && any(valid_totals > 1)) {
      candidates[[length(candidates) + 1]] <- list(
        key = "counts::pl::sg",
        family = "binomial",
        mode = "grouped_counts",
        label = "Plural (`pl`) vs singular (`sg`) grouped counts",
        outcome_cols = c("sg", "pl"),
        success_col = "pl",
        failure_col = "sg",
        success_label = "plural",
        failure_label = "singular"
      )
    }
  }

  preferred_binary <- c("plural", "pl", "sg")
  binary_cols <- unique(c(preferred_binary, setdiff(names(df), preferred_binary)))
  binary_cols <- setdiff(binary_cols, c("plural_successes", "plural_failures"))
  for (col in binary_cols) {
    spec <- make_binary_spec(col)
    if (!is.null(spec)) candidates[[length(candidates) + 1]] <- spec
  }

  preferred_binary_text <- unique(c("q_lemma", setdiff(names(df), "q_lemma")))
  for (col in preferred_binary_text) {
    spec <- make_binary_text_spec(col)
    if (!is.null(spec)) candidates[[length(candidates) + 1]] <- spec
  }

  preferred_categorical <- unique(c("q_lemma", setdiff(names(df), "q_lemma")))
  for (col in preferred_categorical) {
    spec <- make_categorical_spec(col)
    if (!is.null(spec)) candidates[[length(candidates) + 1]] <- spec
  }

  candidates
}

get_response_spec <- function(df, response_key = NULL) {
  candidates <- list_response_candidates(df)
  if (length(candidates) == 0) {
    stop("Dataset must contain a supported response column: binary 0/1, integer `sg`/`pl` counts, or an eligible categorical column such as `q_lemma`.")
  }

  keys <- vapply(candidates, `[[`, character(1), "key")
  if (!is.null(response_key) && nzchar(response_key) && response_key %in% keys) {
    return(candidates[[match(response_key, keys)]])
  }
  candidates[[1]]
}

response_level_choices <- function(response) {
  if (is.null(response)) return(character(0))

  if (identical(response$family, "multinomial")) {
    return(as.character(response$outcome_levels %||% character(0)))
  }

  levels <- c(response$failure_label %||% "0", response$success_label %||% "1")
  unique(as.character(levels[nzchar(as.character(levels))]))
}

resolve_response_reference <- function(response, response_reference_level = NULL) {
  levels <- response_level_choices(response)
  if (length(levels) == 0) return(NULL)

  ref <- as.character(
    response_reference_level %||%
      response$reference_level %||%
      levels[[1]]
  )
  if (!length(ref) || is.na(ref) || !nzchar(ref) || !ref %in% levels) {
    ref <- levels[[1]]
  }
  ref
}

resolve_binomial_outcomes <- function(response, response_reference_level = NULL) {
  levels <- response_level_choices(response)
  if (length(levels) < 2) {
    levels <- c(response$failure_label %||% "0", response$success_label %||% "1")
  }
  reference <- resolve_response_reference(response, response_reference_level) %||% levels[[1]]
  success_levels <- setdiff(levels, reference)
  success <- if (length(success_levels) > 0) success_levels[[1]] else levels[[2]]

  list(
    reference_label = reference,
    success_label = success,
    failure_label = reference,
    levels = c(reference, success)
  )
}

summarize_response_data <- function(df, response_key = NULL, response_reference_level = NULL) {
  response <- get_response_spec(df, response_key)

  if (identical(response$family, "multinomial")) {
    y <- trimws(as.character(df[[response$response_col]]))
    y[is.na(y)] <- ""
    valid <- nzchar(y)
    counts <- sort(table(y[valid]), decreasing = TRUE)
    level_count_table <- data.frame(
      outcome = names(counts),
      n = as.integer(counts),
      stringsAsFactors = FALSE,
      row.names = NULL
    )

    resolved_reference <- resolve_response_reference(response, response_reference_level)
    if (!resolved_reference %in% level_count_table$outcome) {
      resolved_reference <- response$reference_level %||% (response$outcome_levels[[1]] %||% NULL)
    }

    return(list(
      response_key = response$key,
      family = response$family,
      mode = response$mode,
      label = response$label,
      outcome_cols = response$outcome_cols,
      response_col = response$response_col,
      usable_rows = sum(valid),
      total_tokens = sum(valid),
      n_levels = nrow(level_count_table),
      level_count_table = level_count_table,
      reference_level = resolved_reference
    ))
  }

  binomial_outcomes <- resolve_binomial_outcomes(response, response_reference_level)

  if (identical(response$mode, "binary_rows")) {
    y <- suppressWarnings(as.integer(df[[response$binary_col]]))
    valid <- !is.na(y) & y %in% c(0L, 1L)
    raw_failure_n <- sum(y[valid] == 0L)
    raw_success_n <- sum(y[valid] == 1L)
    if (identical(binomial_outcomes$reference_label, response$success_label %||% "1")) {
      failure_n <- raw_success_n
      success_n <- raw_failure_n
    } else {
      failure_n <- raw_failure_n
      success_n <- raw_success_n
    }
    usable_rows <- sum(valid)
  } else if (identical(response$mode, "binary_text_rows")) {
    y <- trimws(as.character(df[[response$binary_col]]))
    y[is.na(y)] <- ""
    valid <- y %in% (response$binary_levels %||% character(0))
    failure_n <- sum(y[valid] == (binomial_outcomes$failure_label %||% ""))
    success_n <- sum(y[valid] == (binomial_outcomes$success_label %||% ""))
    usable_rows <- sum(valid)
  } else {
    failure_vals <- suppressWarnings(as.numeric(df[[response$failure_col]]))
    success_vals <- suppressWarnings(as.numeric(df[[response$success_col]]))
    valid <- !is.na(failure_vals) & !is.na(success_vals) &
      failure_vals >= 0 & success_vals >= 0 &
      is_integer_like(failure_vals) & is_integer_like(success_vals) &
      (failure_vals + success_vals) > 0
    if (identical(binomial_outcomes$reference_label, response$success_label %||% response$success_col)) {
      failure_n <- sum(success_vals[valid])
      success_n <- sum(failure_vals[valid])
    } else {
      failure_n <- sum(failure_vals[valid])
      success_n <- sum(success_vals[valid])
    }
    usable_rows <- sum(valid)
  }

  list(
    response_key = response$key,
    family = response$family %||% "binomial",
    mode = response$mode,
    label = response$label,
    outcome_cols = response$outcome_cols,
    success_label = binomial_outcomes$success_label,
    failure_label = binomial_outcomes$failure_label,
    reference_level = binomial_outcomes$reference_label,
    outcome_levels = binomial_outcomes$levels,
    usable_rows = usable_rows,
    success_n = success_n,
    failure_n = failure_n,
    total_tokens = success_n + failure_n
  )
}

normalize_response_info <- function(info) {
  if (is.null(info)) return(NULL)
  if (is.null(info$family)) {
    info$family <- if (
      identical(info$mode, "categorical_rows") ||
      !is.null(info$n_levels) ||
      !is.null(info$level_count_table)
    ) {
      "multinomial"
    } else {
      "binomial"
    }
  }
  if (identical(info$family, "multinomial")) {
    if (is.null(info$label)) info$label <- "Categorical response"
    if (is.null(info$mode)) info$mode <- "categorical_rows"
    if (is.null(info$level_count_table) && !is.null(info$level_counts)) {
      info$level_count_table <- data.frame(
        outcome = names(info$level_counts),
        n = as.numeric(info$level_counts),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    }
    if (!is.null(info$level_count_table)) {
      keep <- c("outcome", "n")
      info$level_count_table <- info$level_count_table[, keep[keep %in% names(info$level_count_table)], drop = FALSE]
      if (all(c("outcome", "n") %in% names(info$level_count_table))) {
        info$level_count_table$outcome <- as.character(info$level_count_table$outcome)
        info$level_count_table$n <- as.numeric(info$level_count_table$n)
      }
    }
    if (is.null(info$n_levels) && is.data.frame(info$level_count_table)) info$n_levels <- nrow(info$level_count_table)
    if (is.null(info$total_tokens) && is.data.frame(info$level_count_table)) info$total_tokens <- sum(info$level_count_table$n, na.rm = TRUE)
    return(info)
  }
  if (is.null(info$success_label)) info$success_label <- "plural"
  if (is.null(info$failure_label)) info$failure_label <- "singular"
  if (is.null(info$reference_level)) info$reference_level <- info$failure_label
  if (is.null(info$outcome_levels)) info$outcome_levels <- c(info$failure_label, info$success_label)
  if (is.null(info$success_n) && !is.null(info$plural_n)) info$success_n <- info$plural_n
  if (is.null(info$failure_n) && !is.null(info$singular_n)) info$failure_n <- info$singular_n
  if (is.null(info$label)) info$label <- "Plural (`pl`/`plural`)"
  if (is.null(info$mode)) info$mode <- "binary_rows"
  info
}

response_is_multinomial <- function(info) {
  identical((normalize_response_info(info) %||% list())$family, "multinomial")
}

response_count_table <- function(info) {
  info <- normalize_response_info(info)
  if (is.null(info)) return(data.frame())

  if (identical(info$family, "multinomial")) {
    out <- info$level_count_table %||% data.frame(outcome = character(0), n = numeric(0))
    if (!is.data.frame(out) || !all(c("outcome", "n") %in% names(out))) {
      return(data.frame(outcome = character(0), n = numeric(0)))
    }
    out$outcome <- as.character(out$outcome)
    out$n <- as.numeric(out$n)
    out <- out[order(out$n, decreasing = TRUE), , drop = FALSE]
    rownames(out) <- NULL
    return(out)
  }

  data.frame(
    outcome = c(info$failure_label %||% "0", info$success_label %||% "1"),
    n = c(as.numeric(info$failure_n %||% 0), as.numeric(info$success_n %||% 0)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

response_count_lines <- function(info, max_levels = 10L) {
  info <- normalize_response_info(info)
  if (is.null(info)) return(character(0))

  if (identical(info$family, "multinomial")) {
    counts <- response_count_table(info)
    lines <- c(
      sprintf("Outcome levels: %s", format(info$n_levels %||% nrow(counts), big.mark = ",")),
      sprintf("Reference outcome: %s", info$reference_level %||% "not set")
    )
    if (nrow(counts) > 0) {
      top_n <- max(1L, as.integer(max_levels %||% 10L))
      top_counts <- head(counts, top_n)
      lines <- c(
        lines,
        "Top outcome counts:",
        vapply(seq_len(nrow(top_counts)), function(i) {
          sprintf(" - %s: %s", top_counts$outcome[[i]], format(top_counts$n[[i]], big.mark = ","))
        }, character(1))
      )
      if (nrow(counts) > nrow(top_counts)) {
        lines <- c(lines, sprintf(" - ... %d more levels", nrow(counts) - nrow(top_counts)))
      }
    }
    lines <- c(lines, sprintf("Total tokens: %s", format(info$total_tokens %||% 0, big.mark = ",")))
    return(lines)
  }

  c(
    sprintf("Reference outcome: %s", info$reference_level %||% info$failure_label %||% "failure"),
    sprintf("%s tokens: %s", info$failure_label %||% "failure", format(info$failure_n %||% 0, big.mark = ",")),
    sprintf("%s tokens: %s", info$success_label %||% "success", format(info$success_n %||% 0, big.mark = ",")),
    sprintf("Total tokens: %s", format(info$total_tokens %||% 0, big.mark = ","))
  )
}

multinomial_random_effect_sparsity_report <- function(
  model_df,
  random_effects = character(0),
  min_group_levels = 2L
) {
  random_effects <- unique(random_effects %||% character(0))
  if (length(random_effects) == 0 || !"response_value" %in% names(model_df)) {
    return(data.frame())
  }

  min_group_levels <- suppressWarnings(as.integer(min_group_levels %||% 2L))
  if (is.na(min_group_levels) || min_group_levels < 2L) min_group_levels <- 2L

  report <- data.frame(
    outcome = character(0),
    tokens = numeric(0),
    stringsAsFactors = FALSE
  )
  for (rv in random_effects) {
    report[[rv]] <- integer(0)
  }

  weights <- if (".case_weight" %in% names(model_df)) {
    suppressWarnings(as.numeric(model_df$.case_weight))
  } else {
    rep(1, nrow(model_df))
  }
  weights[is.na(weights)] <- 0

  outcome_levels <- levels(model_df$response_value)
  if (length(outcome_levels) == 0) {
    outcome_levels <- sort(unique(as.character(model_df$response_value)))
  }

  for (outcome in outcome_levels) {
    idx <- !is.na(model_df$response_value) & as.character(model_df$response_value) == outcome
    if (!any(idx)) next

    token_n <- sum(weights[idx], na.rm = TRUE)
    row <- data.frame(
      outcome = as.character(outcome),
      tokens = as.numeric(token_n),
      stringsAsFactors = FALSE
    )
    for (rv in random_effects) {
      group_vals <- as.character(model_df[[rv]][idx])
      group_vals <- group_vals[!is.na(group_vals) & nzchar(group_vals)]
      row[[rv]] <- length(unique(group_vals))
    }

    has_sparse_group <- any(vapply(random_effects, function(rv) row[[rv]][[1]] < min_group_levels, logical(1)))
    if (token_n < min_group_levels || has_sparse_group) {
      report <- rbind(report, row)
    }
  }

  if (nrow(report) == 0) return(report)
  report <- report[order(report$tokens, report$outcome), , drop = FALSE]
  rownames(report) <- NULL
  report
}

format_multinomial_random_effect_sparsity <- function(
  report,
  random_effects = character(0),
  max_levels = 8L
) {
  if (!is.data.frame(report) || nrow(report) == 0) return("")

  random_effects <- intersect(unique(random_effects %||% character(0)), names(report))
  max_levels <- suppressWarnings(as.integer(max_levels %||% 8L))
  if (is.na(max_levels) || max_levels < 1L) max_levels <- 8L

  shown <- head(report, max_levels)
  detail <- vapply(seq_len(nrow(shown)), function(i) {
    bits <- c(
      sprintf("tokens = %s", format(shown$tokens[[i]], big.mark = ",")),
      vapply(random_effects, function(rv) {
        sprintf("%s levels = %s", rv, format(shown[[rv]][[i]], big.mark = ","))
      }, character(1))
    )
    sprintf("%s (%s)", shown$outcome[[i]], paste(bits, collapse = "; "))
  }, character(1))
  more <- if (nrow(report) > nrow(shown)) {
    sprintf("; ... %d more", nrow(report) - nrow(shown))
  } else {
    ""
  }

  sprintf(
    paste(
      "This multinomial random-intercept model is under-supported for some outcome levels.",
      "Each outcome level should appear in at least 2 levels of every random-effect grouping variable.",
      "Problematic outcome levels: %s%s.",
      "Collapse or filter these outcome levels into `other`, or remove the corresponding random intercept."
    ),
    paste(detail, collapse = ", "),
    more
  )
}

fit_backend_choices <- function(response_info = NULL) {
  if (response_is_multinomial(response_info)) {
    c("mclogit::mblogit (multinomial; random intercepts supported)" = "multinom")
  } else {
    c(
      "lme4::glmer (mixed model)" = "glmer",
      "fastglm::fastglm (fixed-effects GLM)" = "fastglm"
    )
  }
}

coerce_fit_backend <- function(backend = NULL, response_info = NULL) {
  choices <- unname(fit_backend_choices(response_info))
  if (length(choices) == 0) return("")
  if (!is.null(backend) && nzchar(backend) && backend %in% choices) backend else choices[[1]]
}

model_backend_label <- function(
  backend = "glmer",
  fastglm_method = NULL,
  multinom_method = NULL,
  multinom_catcov = NULL
) {
  if (identical(backend, "multinom")) {
    method_label <- as.character(multinom_method %||% "PQL")
    catcov_label <- as.character(multinom_catcov %||% "single")
    return(sprintf("mclogit::mblogit (%s, catCov = %s)", method_label, catcov_label))
  }
  if (identical(backend, "fastglm")) {
    method_label <- switch(
      as.character(fastglm_method %||% "3"),
      "2" = "LLT Cholesky",
      "3" = "LDLT Cholesky",
      paste("method", fastglm_method)
    )
    return(sprintf("fastglm::fastglm (%s)", method_label))
  }
  "lme4::glmer"
}

is_mixed_model <- function(model) {
  inherits(model, "merMod")
}

is_mblogit_model <- function(model) {
  inherits(model, "mblogit")
}

get_model_formula <- function(model) {
  stored_formula <- attr(model, "app_formula", exact = TRUE)
  if (!is.null(stored_formula)) return(stored_formula)
  tryCatch(formula(model), error = function(e) NULL)
}

get_fixed_formula <- function(model) {
  frm <- get_model_formula(model)
  if (is.null(frm)) stop("Model formula unavailable.")
  if (!is_mixed_model(model)) return(frm)
  if (requireNamespace("reformulas", quietly = TRUE)) {
    reformulas::nobars(frm)
  } else {
    lme4::nobars(frm)
  }
}

get_model_coefficients <- function(model) {
  if (is_mixed_model(model)) {
    return(fixef(model))
  }
  stats::coef(model)
}

compute_fastglm_vcov <- function(model, x) {
  weights <- suppressWarnings(as.numeric(model$weights))
  if (
    !is.matrix(x) ||
    length(weights) != nrow(x) ||
    any(!is.finite(weights)) ||
    any(weights < 0)
  ) {
    return(NULL)
  }

  out <- tryCatch(
    solve(crossprod(x, x * weights)),
    error = function(e) NULL
  )
  if (is.null(out) || any(!is.finite(out))) return(NULL)

  dispersion <- suppressWarnings(as.numeric(model$dispersion %||% 1))
  if (length(dispersion) != 1L || !is.finite(dispersion) || dispersion <= 0) dispersion <- 1
  out <- out * dispersion
  dimnames(out) <- list(colnames(x), colnames(x))
  out
}

get_model_vcov <- function(model, model_df = NULL) {
  stored_vcov <- attr(model, "app_vcov", exact = TRUE)
  if (is.matrix(stored_vcov) && all(is.finite(stored_vcov))) {
    return(stored_vcov)
  }

  if (inherits(model, "fastglm") && is.data.frame(model_df)) {
    fixed_formula <- get_fixed_formula(model)
    fixed_terms <- delete.response(terms(fixed_formula))
    x <- model.matrix(fixed_terms, model_df)
    reconstructed_vcov <- compute_fastglm_vcov(model, x)
    if (!is.null(reconstructed_vcov)) return(reconstructed_vcov)
  }

  tryCatch(
    as.matrix(vcov(model)),
    error = function(e) {
      coef_table <- tryCatch(as.matrix(coef(summary(model))), error = function(inner) NULL)
      if (is.null(coef_table) || !"Std. Error" %in% colnames(coef_table)) stop(e)
      se <- coef_table[, "Std. Error"]
      out <- diag(se^2, nrow = length(se))
      dimnames(out) <- list(rownames(coef_table), rownames(coef_table))
      out
    }
  )
}

fit_fastglm_model <- function(frm, model_df, method = 3L, maxit = 100L) {
  if (!requireNamespace("fastglm", quietly = TRUE)) {
    stop("Package `fastglm` is not installed. Install it or switch back to `lme4::glmer`.")
  }

  fixed_formula <- lme4::nobars(frm)
  fixed_terms <- delete.response(terms(fixed_formula))
  x <- model.matrix(fixed_terms, model_df)
  y <- cbind(model_df$plural_successes, model_df$plural_failures)

  model <- fastglm::fastglm(
    x = x,
    y = y,
    family = binomial(link = "logit"),
    method = as.integer(method),
    maxit = as.integer(maxit)
  )
  attr(model, "app_formula") <- frm
  attr(model, "app_vcov") <- compute_fastglm_vcov(model, x)
  model
}

make_mblogit_random_spec <- function(random_effects = character(0)) {
  random_effects <- unique(random_effects %||% character(0))
  if (length(random_effects) == 0) return(NULL)
  random_formulas <- lapply(random_effects, function(rv) {
    as.formula(paste("~ 1 |", rv))
  })
  if (length(random_formulas) == 1L) random_formulas[[1]] else random_formulas
}

fit_multinom_model <- function(
  frm,
  model_df,
  random_effects = character(0),
  method = "PQL",
  catCov = "single",
  maxit = 25L
) {
  if (!requireNamespace("mclogit", quietly = TRUE)) {
    stop("Package `mclogit` is not installed. Install it to fit multinomial responses.")
  }
  if (!"response_value" %in% names(model_df)) {
    stop("Prepared multinomial data are missing `response_value`.")
  }
  if (!".case_weight" %in% names(model_df)) {
    model_df$.case_weight <- 1
  }

  random_effects <- unique(random_effects %||% character(0))
  method <- toupper(as.character(method %||% "PQL"))
  if (!method %in% c("PQL", "MQL")) method <- "PQL"
  catCov <- as.character(catCov %||% "single")
  if (!catCov %in% c("free", "diagonal", "single")) catCov <- "single"
  maxit <- suppressWarnings(as.integer(maxit %||% 25L))
  if (is.na(maxit) || maxit < 1L) maxit <- 25L

  fixed_formula <- if (length(random_effects) > 0) {
    if (requireNamespace("reformulas", quietly = TRUE)) {
      reformulas::nobars(frm)
    } else {
      lme4::nobars(frm)
    }
  } else {
    frm
  }
  control <- if (length(random_effects) > 0) {
    mclogit::mmclogit.control(
      maxit = as.integer(maxit),
      trace = FALSE,
      trace.inner = FALSE
    )
  } else {
    mclogit::mclogit.control(
      maxit = as.integer(maxit),
      trace = FALSE
    )
  }

  fit_args <- list(
    formula = fixed_formula,
    data = model_df,
    weights = model_df$.case_weight,
    control = control,
    estimator = "ML"
  )
  if (length(random_effects) > 0) {
    fit_args$random <- make_mblogit_random_spec(random_effects)
    fit_args$method <- method
    fit_args$catCov <- catCov
  }

  model <- tryCatch(
    do.call(mclogit::mblogit, fit_args),
    error = function(e) {
      msg <- conditionMessage(e)
      if (
        identical(method, "MQL") &&
        grepl("computationally singular", msg, ignore.case = TRUE)
      ) {
        stop(
          paste(
            "The multinomial fit failed under `mclogit` with the `MQL` approximation because the working system became computationally singular.",
            "`MQL` is the faster but less stable approximation in this app and can fail for interaction-heavy multinomial mixed models even when `PQL` succeeds.",
            "Try `PQL`, remove or simplify interaction terms, or simplify the random-effects structure."
          ),
          call. = FALSE
        )
      }
      stop(e)
    }
  )
  attr(model, "app_formula") <- fixed_formula
  model
}

prepare_data <- function(df, opts) {
  response <- get_response_spec(df, opts$response_key %||% NULL)

  if (identical(response$family, "multinomial")) {
    y <- trimws(as.character(df[[response$response_col]]))
    y[is.na(y)] <- ""
    response_levels <- response$outcome_levels %||% sort(unique(y[nzchar(y)]))
    response_reference_level <- as.character(
      opts$response_reference_level %||%
        response$reference_level %||%
        (response_levels[[1]] %||% NULL)
    )
    if (!response_reference_level %in% response_levels) {
      response_reference_level <- response_levels[[1]] %||% NULL
    }
    if (!is.null(response_reference_level) && nzchar(response_reference_level)) {
      response_levels <- c(response_reference_level, setdiff(response_levels, response_reference_level))
    }
    df$response_value <- factor(y, levels = response_levels)
    valid_outcome <- !is.na(df$response_value) & nzchar(y)
  } else if (identical(response$mode, "binary_rows")) {
    binomial_outcomes <- resolve_binomial_outcomes(response, opts$response_reference_level %||% NULL)
    y <- suppressWarnings(as.integer(df[[response$binary_col]]))
    valid_outcome <- !is.na(y) & y %in% c(0L, 1L)
    raw_success_label <- response$success_label %||% "1"
    if (identical(binomial_outcomes$reference_label, raw_success_label)) {
      df$plural_successes <- as.integer(y == 0L)
      df$plural_failures <- as.integer(y == 1L)
    } else {
      df$plural_successes <- as.integer(y == 1L)
      df$plural_failures <- as.integer(y == 0L)
    }
  } else if (identical(response$mode, "binary_text_rows")) {
    binomial_outcomes <- resolve_binomial_outcomes(response, opts$response_reference_level %||% NULL)
    y <- trimws(as.character(df[[response$binary_col]]))
    y[is.na(y)] <- ""
    failure_level <- binomial_outcomes$failure_label %||% (response$binary_levels[[1]] %||% "")
    success_level <- binomial_outcomes$success_label %||% (response$binary_levels[[2]] %||% "")
    df$plural_successes <- as.integer(y == success_level)
    df$plural_failures <- as.integer(y == failure_level)
    valid_outcome <- y %in% c(failure_level, success_level)
  } else {
    binomial_outcomes <- resolve_binomial_outcomes(response, opts$response_reference_level %||% NULL)
    failure_vals <- suppressWarnings(as.numeric(df[[response$failure_col]]))
    success_vals <- suppressWarnings(as.numeric(df[[response$success_col]]))
    if (identical(binomial_outcomes$reference_label, response$success_label %||% response$success_col)) {
      df$plural_successes <- as.integer(round(failure_vals))
      df$plural_failures <- as.integer(round(success_vals))
    } else {
      df$plural_successes <- as.integer(round(success_vals))
      df$plural_failures <- as.integer(round(failure_vals))
    }
    valid_outcome <- !is.na(failure_vals) & !is.na(success_vals) &
      failure_vals >= 0 & success_vals >= 0 &
      is_integer_like(failure_vals) & is_integer_like(success_vals) &
      (failure_vals + success_vals) > 0
  }

  if (length(opts$fixed_effects) == 0) stop("Select at least one fixed effect.")

  overlap <- intersect(opts$fixed_effects, opts$random_effects)
  if (length(overlap) > 0) {
    stop(sprintf(
      "Variable(s) cannot be both fixed and random effects in this app: %s",
      paste(overlap, collapse = ", ")
    ))
  }

  required <- unique(c(opts$fixed_effects, opts$random_effects))

  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required columns for selected options: %s", paste(missing_cols, collapse = ", ")))
  }

  observed_keep_cols <- if (identical(response$family, "multinomial")) {
    unique(c(
      "response_value",
      required,
      if ("lemma" %in% names(df)) "lemma"
    ))
  } else {
    unique(c(
      "plural_successes",
      "plural_failures",
      required,
      if ("lemma" %in% names(df)) "lemma"
    ))
  }
  observed_prediction_data <- df[, observed_keep_cols, drop = FALSE]
  observed_prediction_data <- observed_prediction_data[valid_outcome, , drop = FALSE]
  for (col in required) {
    observed_prediction_data <- observed_prediction_data[
      !is.na(observed_prediction_data[[col]]) & observed_prediction_data[[col]] != "",
      ,
      drop = FALSE
    ]
  }
  observed_lemma_col <- NULL
  observed_lemma_candidates <- unique(c(
    opts$random_effects,
    "lemma"
  ))
  observed_lemma_candidates <- observed_lemma_candidates[observed_lemma_candidates %in% names(observed_prediction_data)]
  if (length(observed_lemma_candidates) > 0) {
    observed_lemma_col <- observed_lemma_candidates[[1]]
    observed_prediction_data$.observed_lemma <- as.character(observed_prediction_data[[observed_lemma_col]])
    observed_prediction_data$.observed_lemma[
      is.na(observed_prediction_data$.observed_lemma) |
        !nzchar(observed_prediction_data$.observed_lemma)
    ] <- NA_character_
  }

  keep_cols <- if (identical(response$family, "multinomial")) {
    unique(c("response_value", required))
  } else {
    unique(c("plural_successes", "plural_failures", required))
  }
  model_df <- df[, keep_cols, drop = FALSE]

  model_df <- model_df[valid_outcome, , drop = FALSE]
  for (col in required) {
    model_df <- model_df[!is.na(model_df[[col]]) & model_df[[col]] != "", , drop = FALSE]
  }

  for (col in opts$fixed_effects) {
    if (!is.numeric(model_df[[col]])) {
      model_df[[col]] <- factor(
        model_df[[col]],
        levels = sort(unique(as.character(model_df[[col]])))
      )
    }
  }
  for (col in opts$random_effects) {
    model_df[[col]] <- factor(
      model_df[[col]],
      levels = sort(unique(as.character(model_df[[col]])))
    )
  }

  input_rows <- nrow(model_df)
  grouping_cols <- unique(c(
    opts$fixed_effects,
    opts$random_effects,
    if (identical(response$family, "multinomial")) "response_value"
  ))
  if (input_rows > 1 && length(grouping_cols) > 0) {
    if (identical(response$family, "multinomial")) {
      model_df$.case_weight <- 1L
      model_df <- aggregate(
        x = model_df[".case_weight"],
        by = model_df[grouping_cols],
        FUN = sum
      )
      model_df <- model_df[, c(grouping_cols, ".case_weight"), drop = FALSE]
    } else {
      model_df <- aggregate(
        x = model_df[c("plural_successes", "plural_failures")],
        by = model_df[grouping_cols],
        FUN = sum
      )
      model_df <- model_df[, c(grouping_cols, "plural_successes", "plural_failures"), drop = FALSE]
    }
  } else if (identical(response$family, "multinomial")) {
    model_df$.case_weight <- 1L
  }

  model_df <- droplevels(model_df)
  if (identical(response$family, "multinomial")) {
    observed_prediction_data$.case_weight <- 1L
    observed_prediction_data <- observed_prediction_data[, unique(c(
      opts$fixed_effects,
      "response_value",
      ".case_weight",
      if (".observed_lemma" %in% names(observed_prediction_data)) ".observed_lemma",
      if ("lemma" %in% names(observed_prediction_data)) "lemma"
    )), drop = FALSE]
  } else {
    observed_prediction_data <- observed_prediction_data[
      (observed_prediction_data$plural_successes + observed_prediction_data$plural_failures) > 0,
      ,
      drop = FALSE
    ]
    observed_prediction_data$.obs_total <-
      observed_prediction_data$plural_successes + observed_prediction_data$plural_failures
    observed_prediction_data$.obs_prob <-
      observed_prediction_data$plural_successes / observed_prediction_data$.obs_total
    observed_prediction_data <- observed_prediction_data[, unique(c(
      opts$fixed_effects,
      "plural_successes",
      "plural_failures",
      ".obs_total",
      ".obs_prob",
      if (".observed_lemma" %in% names(observed_prediction_data)) ".observed_lemma",
      if ("lemma" %in% names(observed_prediction_data)) "lemma"
    )), drop = FALSE]
  }
  resolved_reference_levels <- list()
  for (col in opts$fixed_effects) {
    if (!col %in% names(model_df) || !is.factor(model_df[[col]])) next

    col_levels <- levels(model_df[[col]])
    if (length(col_levels) == 0) next
    ref <- opts$reference_levels[[col]] %||% col_levels[1]
    if (!ref %in% col_levels) ref <- col_levels[1]
    resolved_reference_levels[[col]] <- ref

    if (length(col_levels) > 1) {
      stats::contrasts(model_df[[col]]) <- make_treatment_contrasts_with_labels(
        levels = col_levels,
        ref = ref
      )
    }
  }

  attr(model_df, "reference_levels") <- resolved_reference_levels
  for (col in opts$fixed_effects) {
    if (!col %in% names(observed_prediction_data) || !col %in% names(model_df)) next

    if (is.factor(model_df[[col]])) {
      observed_prediction_data[[col]] <- factor(
        as.character(observed_prediction_data[[col]]),
        levels = levels(model_df[[col]])
      )
    } else if (is.numeric(model_df[[col]])) {
      observed_prediction_data[[col]] <- suppressWarnings(as.numeric(observed_prediction_data[[col]]))
    }
  }
  attr(model_df, "observed_prediction_data") <- observed_prediction_data
  if (identical(response$family, "multinomial")) {
    level_count_table <- if (nrow(model_df) == 0) {
      data.frame(outcome = character(0), n = numeric(0), stringsAsFactors = FALSE)
    } else {
      counts <- aggregate(
        x = model_df[".case_weight"],
        by = list(outcome = as.character(model_df$response_value)),
        FUN = sum
      )
      names(counts)[names(counts) == ".case_weight"] <- "n"
      counts[order(counts$n, decreasing = TRUE), , drop = FALSE]
    }

    attr(model_df, "response_info") <- list(
      response_key = response$key,
      family = response$family,
      mode = response$mode,
      label = response$label,
      outcome_cols = response$outcome_cols,
      response_col = response$response_col,
      input_rows = input_rows,
      modeled_rows = nrow(model_df),
      total_tokens = sum(model_df$.case_weight),
      n_levels = nlevels(model_df$response_value),
      level_count_table = level_count_table,
      reference_level = response_reference_level %||% (levels(model_df$response_value)[1] %||% NULL)
    )
  } else {
    attr(model_df, "response_info") <- list(
      response_key = response$key,
      family = response$family %||% "binomial",
      mode = response$mode,
      label = response$label,
      outcome_cols = response$outcome_cols,
      success_label = binomial_outcomes$success_label,
      failure_label = binomial_outcomes$failure_label,
      reference_level = binomial_outcomes$reference_label,
      outcome_levels = binomial_outcomes$levels,
      input_rows = input_rows,
      modeled_rows = nrow(model_df),
      success_n = sum(model_df$plural_successes),
      failure_n = sum(model_df$plural_failures),
      total_tokens = sum(model_df$plural_failures + model_df$plural_successes)
    )
  }
  model_df
}

compute_fixed_effects <- function(model) {
  if (is_mblogit_model(model)) {
    reference_outcome <- rownames(model$D)[1] %||% NA_character_
    summ <- tryCatch(summary(model), error = function(e) e)

    if (!inherits(summ, "error")) {
      coef_tbl <- as.matrix(summ$coefficients)
      row_ids <- rownames(coef_tbl) %||% paste0("outcome_", seq_len(nrow(coef_tbl)))
      estimate <- coef_tbl[, "Estimate"]
      std_error <- coef_tbl[, "Std. Error"]
      z_value <- coef_tbl[, "z value"]
      p_value <- coef_tbl[, "Pr(>|z|)"]
      crit <- qnorm(0.975)
      ci_low <- estimate - crit * std_error
      ci_high <- estimate + crit * std_error

      return(data.frame(
        outcome = sub("~.*$", "", row_ids),
        reference_outcome = reference_outcome,
        term = sub("^[^~]+~", "", row_ids),
        estimate = as.numeric(estimate),
        std_error = as.numeric(std_error),
        z_value = as.numeric(z_value),
        p_value = as.numeric(p_value),
        ci_low = as.numeric(ci_low),
        ci_high = as.numeric(ci_high),
        odds_ratio = exp(as.numeric(estimate)),
        or_ci_low = exp(as.numeric(ci_low)),
        or_ci_high = exp(as.numeric(ci_high)),
        check.names = FALSE,
        row.names = NULL
      ))
    }

    beta <- stats::coef(model)
    row_ids <- names(beta) %||% paste0("coef_", seq_along(beta))
    estimate <- as.numeric(beta)
    out <- data.frame(
      outcome = ifelse(grepl("~", row_ids, fixed = TRUE), sub("~.*$", "", row_ids), reference_outcome),
      reference_outcome = reference_outcome,
      term = ifelse(grepl("~", row_ids, fixed = TRUE), sub("^[^~]+~", "", row_ids), row_ids),
      estimate = estimate,
      std_error = NA_real_,
      z_value = NA_real_,
      p_value = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      odds_ratio = exp(estimate),
      or_ci_low = NA_real_,
      or_ci_high = NA_real_,
      check.names = FALSE,
      row.names = NULL
    )
    attr(out, "warning") <- paste(
      "mclogit returned coefficient estimates, but covariance-based statistics were unavailable:",
      conditionMessage(summ)
    )
    return(out)
  }

  beta <- get_model_coefficients(model)
  vcv <- get_model_vcov(model)
  se <- sqrt(diag(vcv))
  z_val <- beta / se
  p_val <- 2 * pnorm(abs(z_val), lower.tail = FALSE)
  crit <- qnorm(0.975)
  ci_low <- beta - crit * se
  ci_high <- beta + crit * se

  data.frame(
    term = names(beta),
    estimate = as.numeric(beta),
    std_error = as.numeric(se),
    z_value = as.numeric(z_val),
    p_value = as.numeric(p_val),
    ci_low = as.numeric(ci_low),
    ci_high = as.numeric(ci_high),
    odds_ratio = exp(as.numeric(beta)),
    or_ci_low = exp(as.numeric(ci_low)),
    or_ci_high = exp(as.numeric(ci_high)),
    check.names = FALSE
  )
}

significance_stars <- function(p) {
  ifelse(
    is.na(p), "",
    ifelse(
      p < 0.001, "***",
      ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))
    )
  )
}

format_p_value <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("< 0.001")
  sprintf("= %.3f", p)
}

format_num <- function(x, digits = 2) {
  if (is.na(x)) return("NA")
  formatC(as.numeric(x), format = "f", digits = digits)
}

format_factor_term_suffix <- function(level_label) {
  paste0("-", level_label)
}

make_treatment_contrasts_with_labels <- function(levels, ref) {
  if (length(levels) <= 1) return(NULL)

  ref_idx <- match(ref, levels)
  if (is.na(ref_idx)) ref_idx <- 1L

  out <- stats::contr.treatment(n = levels, base = ref_idx)
  colnames(out) <- format_factor_term_suffix(colnames(out))
  out
}

factor_term_entries <- function(x, variable, reference = NULL) {
  if (!is.factor(x)) x <- factor(x)

  level_values <- levels(x)
  if (length(level_values) == 0) return(list())

  ref <- reference %||% (level_values[1] %||% "")
  if (!ref %in% level_values) ref <- level_values[1] %||% ""

  nonref_levels <- setdiff(level_values, ref)
  if (length(nonref_levels) == 0) return(list())

  raw_suffixes <- format_factor_term_suffix(nonref_levels)
  contrast_matrix <- tryCatch(stats::contrasts(x), error = function(e) NULL)
  if (!is.null(contrast_matrix) && ncol(contrast_matrix) == length(nonref_levels)) {
    raw_suffixes <- colnames(contrast_matrix)
  }
  if (length(raw_suffixes) != length(nonref_levels) || any(is.na(raw_suffixes)) || any(!nzchar(raw_suffixes))) {
    raw_suffixes <- format_factor_term_suffix(nonref_levels)
  }

  out <- vector("list", length(nonref_levels))
  for (i in seq_along(nonref_levels)) {
    level_label <- nonref_levels[[i]]
    raw_term <- paste0(variable, raw_suffixes[[i]])
    display_term <- paste0(variable, format_factor_term_suffix(level_label))

    out[[i]] <- list(
      raw_term = raw_term,
      display_term = display_term,
      variable = variable,
      variable_label = variable,
      type = "factor",
      level = level_label,
      reference = ref,
      label = sprintf("%s = %s (vs %s)", variable, level_label, ref)
    )
  }

  names(out) <- vapply(out, `[[`, character(1), "raw_term")
  out
}

main_effect_term_dictionary <- function(model_df, fixed_effects) {
  dict <- list()

  for (v in fixed_effects) {
    if (!v %in% names(model_df)) next

    x <- model_df[[v]]
    if (is.numeric(x)) {
      dict[[v]] <- list(
        raw_term = v,
        display_term = v,
        variable = v,
        variable_label = v,
        type = "numeric",
        label = sprintf("a one-unit increase in %s", v)
      )
      next
    }

    if (!is.factor(x)) x <- factor(x)
    ref <- (attr(model_df, "reference_levels") %||% list())[[v]] %||% (levels(x)[1] %||% "")
    entries <- factor_term_entries(x, variable = v, reference = ref)
    for (entry_name in names(entries)) {
      entry <- entries[[entry_name]]
      dict[[entry_name]] <- entry
      if (!entry$display_term %in% names(dict)) {
        dict[[entry$display_term]] <- entry
      }
    }
  }

  dict
}

describe_term_components <- function(term, term_dict) {
  pieces <- strsplit(term, ":", fixed = TRUE)[[1]]
  lapply(pieces, function(piece) {
    out <- term_dict[[piece]]
    if (is.null(out)) {
      list(
        variable = piece,
        variable_label = piece,
        type = "unknown",
        label = piece
      )
    } else {
      out
    }
  })
}

relabel_model_terms <- function(terms, model_df, fixed_effects) {
  if (length(terms) == 0) return(character(0))

  term_dict <- main_effect_term_dictionary(model_df, fixed_effects)
  vapply(terms, function(term) {
    if (is.na(term) || !nzchar(term) || identical(term, "(Intercept)")) return(term)

    pieces <- strsplit(term, ":", fixed = TRUE)[[1]]
    display_pieces <- vapply(pieces, function(piece) {
      out <- term_dict[[piece]]
      if (is.null(out)) piece else out$display_term %||% piece
    }, character(1))

    paste(display_pieces, collapse = ":")
  }, character(1), USE.NAMES = FALSE)
}

format_model_summary_lines <- function(model, model_df, fixed_effects) {
  lines <- capture.output(print(summary(model)))
  if (is_mblogit_model(model)) return(lines)
  if (is.null(model_df) || length(fixed_effects) == 0) return(lines)

  raw_terms <- names(get_model_coefficients(model))
  display_terms <- relabel_model_terms(raw_terms, model_df, fixed_effects)
  replace_idx <- which(!is.na(raw_terms) & !is.na(display_terms) & nzchar(raw_terms) & raw_terms != display_terms)
  if (length(replace_idx) == 0) return(lines)

  replace_idx <- replace_idx[order(nchar(raw_terms[replace_idx]), decreasing = TRUE)]
  for (i in replace_idx) {
    lines <- gsub(raw_terms[[i]], display_terms[[i]], lines, fixed = TRUE)
  }

  lines
}

summarize_significant_terms <- function(
  fixed_tbl,
  model_df,
  fixed_effects,
  response_info = NULL,
  p_cutoff = 0.05
) {
  response_info <- normalize_response_info(response_info)
  if (is.null(fixed_tbl) || nrow(fixed_tbl) == 0) return(character(0))

  sig_tbl <- fixed_tbl[
    fixed_tbl$term != "(Intercept)" & !is.na(fixed_tbl$p_value) & fixed_tbl$p_value < p_cutoff,
    ,
    drop = FALSE
  ]
  if (nrow(sig_tbl) == 0) return(character(0))

  term_dict <- main_effect_term_dictionary(model_df, fixed_effects)

  if (identical(response_info$family, "multinomial")) {
    return(vapply(seq_len(nrow(sig_tbl)), function(i) {
      row <- sig_tbl[i, , drop = FALSE]
      term <- row$term[[1]]
      pieces <- describe_term_components(term, term_dict)
      higher_lower <- if (isTRUE(row$estimate[[1]] >= 0)) "higher" else "lower"
      outcome_label <- row$outcome[[1]] %||% "outcome"
      reference_outcome <- row$reference_outcome[[1]] %||% response_info$reference_level %||% "reference outcome"

      if (length(pieces) == 1) {
        piece <- pieces[[1]]
        if (identical(piece$type, "numeric")) {
          sprintf(
            "A one-unit increase in %s is associated with %s odds of %s relative to %s (OR = %s, 95%% CI [%s, %s], p %s%s).",
            piece$variable_label,
            higher_lower,
            outcome_label,
            reference_outcome,
            format_num(row$odds_ratio[[1]]),
            format_num(row$or_ci_low[[1]]),
            format_num(row$or_ci_high[[1]]),
            format_p_value(row$p_value[[1]]),
            significance_stars(row$p_value[[1]])
          )
        } else if (identical(piece$type, "factor")) {
          sprintf(
            "Compared with %s = %s, %s is associated with %s odds of %s relative to %s (OR = %s, 95%% CI [%s, %s], p %s%s).",
            piece$variable_label,
            piece$reference %||% "reference",
            paste0(piece$variable_label, " = ", piece$level %||% piece$label),
            higher_lower,
            outcome_label,
            reference_outcome,
            format_num(row$odds_ratio[[1]]),
            format_num(row$or_ci_low[[1]]),
            format_num(row$or_ci_high[[1]]),
            format_p_value(row$p_value[[1]]),
            significance_stars(row$p_value[[1]])
          )
        } else {
          sprintf(
            "The coefficient for %s is associated with %s odds of %s relative to %s (OR = %s, 95%% CI [%s, %s], p %s%s).",
            term,
            higher_lower,
            outcome_label,
            reference_outcome,
            format_num(row$odds_ratio[[1]]),
            format_num(row$or_ci_low[[1]]),
            format_num(row$or_ci_high[[1]]),
            format_p_value(row$p_value[[1]]),
            significance_stars(row$p_value[[1]])
          )
        }
      } else {
        component_labels <- vapply(pieces, `[[`, character(1), "label")
        sprintf(
          "The interaction %s is significant for %s relative to %s; the combined term is associated with %s odds (OR = %s, 95%% CI [%s, %s], p %s%s).",
          paste(component_labels, collapse = " x "),
          outcome_label,
          reference_outcome,
          higher_lower,
          format_num(row$odds_ratio[[1]]),
          format_num(row$or_ci_low[[1]]),
          format_num(row$or_ci_high[[1]]),
          format_p_value(row$p_value[[1]]),
          significance_stars(row$p_value[[1]])
        )
      }
    }, character(1)))
  }

  vapply(seq_len(nrow(sig_tbl)), function(i) {
    row <- sig_tbl[i, , drop = FALSE]
    term <- row$term[[1]]
    pieces <- describe_term_components(term, term_dict)
    higher_lower <- if (isTRUE(row$estimate[[1]] >= 0)) "higher" else "lower"
    outcome_label <- response_info$success_label %||% "success"

    if (length(pieces) == 1) {
      piece <- pieces[[1]]
      if (identical(piece$type, "numeric")) {
        sprintf(
          "A one-unit increase in %s is associated with %s odds of %s (OR = %s, 95%% CI [%s, %s], p %s%s).",
          piece$variable_label,
          higher_lower,
          outcome_label,
          format_num(row$odds_ratio[[1]]),
          format_num(row$or_ci_low[[1]]),
          format_num(row$or_ci_high[[1]]),
          format_p_value(row$p_value[[1]]),
          significance_stars(row$p_value[[1]])
        )
      } else if (identical(piece$type, "factor")) {
        sprintf(
          "Compared with %s = %s, %s is associated with %s odds of %s (OR = %s, 95%% CI [%s, %s], p %s%s).",
          piece$variable_label,
          piece$reference %||% "reference",
          paste0(piece$variable_label, " = ", piece$level %||% piece$label),
          higher_lower,
          outcome_label,
          format_num(row$odds_ratio[[1]]),
          format_num(row$or_ci_low[[1]]),
          format_num(row$or_ci_high[[1]]),
          format_p_value(row$p_value[[1]]),
          significance_stars(row$p_value[[1]])
        )
      } else {
        sprintf(
          "The coefficient for %s indicates %s odds of %s (OR = %s, 95%% CI [%s, %s], p %s%s).",
          term,
          higher_lower,
          outcome_label,
          format_num(row$odds_ratio[[1]]),
          format_num(row$or_ci_low[[1]]),
          format_num(row$or_ci_high[[1]]),
          format_p_value(row$p_value[[1]]),
          significance_stars(row$p_value[[1]])
        )
      }
    } else {
      component_labels <- vapply(pieces, `[[`, character(1), "label")
      sprintf(
        "The interaction %s is significant, indicating that the effect differs across these conditions; the combined term is associated with %s odds of %s (OR = %s, 95%% CI [%s, %s], p %s%s).",
        paste(component_labels, collapse = " x "),
        higher_lower,
        outcome_label,
        format_num(row$odds_ratio[[1]]),
        format_num(row$or_ci_low[[1]]),
        format_num(row$or_ci_high[[1]]),
        format_p_value(row$p_value[[1]]),
        significance_stars(row$p_value[[1]])
      )
    }
  }, character(1))
}

make_pred_values <- function(x, level_order = NULL) {
  if (is.factor(x)) return(resolve_prediction_level_order(levels(x), level_order))
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    if (isTRUE(all.equal(rng[1], rng[2]))) return(rng[1])
    return(seq(rng[1], rng[2], length.out = 30))
  }
  unique(x)
}

predict_mblogit_response <- function(model, newdata, se.fit = FALSE) {
  rhs <- delete.response(terms(model))
  m <- model.frame(rhs, data = newdata, na.action = na.exclude)
  na_act <- attr(m, "na.action")
  offset <- model.offset(m)
  offset_in_call <- model$call$offset
  if (!is.null(offset_in_call)) {
    offset_in_call <- eval(offset_in_call, newdata, environment(terms(model)))
    if (length(offset)) {
      offset <- offset + offset_in_call
    } else {
      offset <- offset_in_call
    }
  }

  X <- model.matrix(rhs, m, contrasts.arg = model$contrasts, xlev = model$xlevels)
  rn <- rownames(X)
  D <- model$D
  n_obs <- nrow(X)
  n_categs <- nrow(D)
  XD <- X %x% D
  eta <- c(XD %*% coef(model))

  if (length(offset)) {
    if (!is.matrix(offset)) {
      if (length(offset) != n_obs) stop("'offset' has wrong length")
      offset <- matrix(offset, ncol = n_categs - 1L)
      offset <- cbind(0, offset)
    } else {
      if (nrow(offset) != n_obs) stop("'offset' has wrong number of rows")
      if (ncol(offset) != n_categs) {
        if (ncol(offset) != n_categs - 1L) {
          stop(sprintf("'offset' must either have %d or %d columns", n_categs - 1L, n_categs))
        }
        offset <- cbind(0, offset)
      }
    }
    offset <- as.vector(t(offset))
    eta <- eta + offset
  }

  rspmat <- function(x) {
    y <- t(matrix(x, nrow = nrow(D)))
    colnames(y) <- rownames(D)
    y
  }

  eta <- rspmat(eta)
  rownames(eta) <- rn
  if (se.fit) {
    V <- vcov(model)
    stopifnot(ncol(XD) == ncol(V))
  }

  exp_eta <- exp(eta)
  sum_exp_eta <- rowSums(exp_eta)
  p <- exp_eta / sum_exp_eta

  if (!se.fit) {
    return(if (is.null(na_act)) p else napredict(na_act, p))
  }

  p_long <- as.vector(t(p))
  s <- rep(seq_len(nrow(X)), each = nrow(D))
  wX <- p_long * (XD - rowsum(p_long * XD, s)[s, , drop = FALSE])
  se_p_long <- sqrt(rowSums(wX * (wX %*% V)))
  se_p <- rspmat(se_p_long)
  rownames(se_p) <- rownames(p)

  if (!is.null(na_act)) {
    p <- napredict(na_act, p)
    se_p <- napredict(na_act, se_p)
  }

  list(fit = p, se.fit = se_p)
}

compute_predictions <- function(
  model,
  model_df,
  x_var,
  group_var = NULL,
  level_orders = list(),
  include_multinomial_uncertainty = FALSE
) {
  if (is.null(x_var) || !nzchar(x_var) || !x_var %in% names(model_df)) return(NULL)
  if (!is.null(group_var) && (!nzchar(group_var) || group_var == "__none__")) group_var <- NULL
  if (!is.null(group_var) && !group_var %in% names(model_df)) group_var <- NULL

  fixed_formula <- get_fixed_formula(model)
  fixed_terms <- delete.response(terms(fixed_formula))
  term_labels <- attr(fixed_terms, "term.labels")
  fixed_vars <- unique(all.vars(reformulate(term_labels %||% "1")))
  reference_levels <- attr(model_df, "reference_levels") %||% list()

  if (!x_var %in% fixed_vars) return(NULL)
  if (!is.null(group_var) && !group_var %in% fixed_vars) group_var <- NULL

  x_values <- make_pred_values(model_df[[x_var]], level_order = level_orders[[x_var]])
  if (is.null(group_var) || group_var == x_var) {
    pred_grid <- setNames(data.frame(x_values, stringsAsFactors = FALSE), x_var)
  } else {
    g_values <- make_pred_values(model_df[[group_var]], level_order = level_orders[[group_var]])
    pred_grid <- expand.grid(
      x_tmp = x_values,
      g_tmp = g_values,
      stringsAsFactors = FALSE,
      KEEP.OUT.ATTRS = FALSE
    )
    names(pred_grid) <- c(x_var, group_var)
  }

  for (v in fixed_vars) {
    if (!v %in% names(pred_grid)) {
      if (is.factor(model_df[[v]])) {
        pred_grid[[v]] <- reference_levels[[v]] %||% (levels(model_df[[v]])[1] %||% NA_character_)
      } else if (is.numeric(model_df[[v]])) {
        pred_grid[[v]] <- mean(model_df[[v]], na.rm = TRUE)
      } else {
        pred_grid[[v]] <- model_df[[v]][1]
      }
    }
  }

  for (v in names(pred_grid)) {
    if (is.factor(model_df[[v]])) {
      pred_grid[[v]] <- factor(pred_grid[[v]], levels = levels(model_df[[v]]))
      model_contrasts <- tryCatch(stats::contrasts(model_df[[v]]), error = function(e) NULL)
      if (!is.null(model_contrasts) && nlevels(pred_grid[[v]]) > 1) {
        stats::contrasts(pred_grid[[v]]) <- model_contrasts
      }
    }
  }

  if (is_mblogit_model(model) || response_is_multinomial(attr(model_df, "response_info"))) {
    include_multinomial_uncertainty <- isTRUE(include_multinomial_uncertainty)
    response_levels <- levels(model_df$response_value)

    normalize_prob_matrix <- function(mat, response_levels = NULL) {
      if (is.null(dim(mat))) {
        mat <- matrix(mat, nrow = 1L)
      }
      if (nrow(mat) != nrow(pred_grid) && ncol(mat) == nrow(pred_grid)) {
        mat <- t(mat)
      }
      if (nrow(mat) != nrow(pred_grid)) {
        stop("Unexpected multinomial prediction shape.")
      }
      if (is.null(colnames(mat))) {
        colnames(mat) <- response_levels %||% paste0("outcome_", seq_len(ncol(mat)))
      }
      mat
    }

    prob_mat <- NULL
    se_prob_mat <- NULL

    if (is_mblogit_model(model) && include_multinomial_uncertainty) {
      pred_with_se <- tryCatch(
        predict_mblogit_response(model, newdata = pred_grid, se.fit = TRUE),
        error = function(e) NULL
      )
      if (is.list(pred_with_se) && !is.null(pred_with_se$fit)) {
        prob_mat <- normalize_prob_matrix(pred_with_se$fit, response_levels = response_levels)
        se_prob_mat <- tryCatch(
          normalize_prob_matrix(pred_with_se$se.fit, response_levels = colnames(prob_mat)),
          error = function(e) NULL
        )
      }
    }

    if (is.null(prob_mat)) {
      prob_mat <- if (is_mblogit_model(model)) {
        predict_mblogit_response(model, newdata = pred_grid, se.fit = FALSE)
      } else {
        predict(model, newdata = pred_grid, type = "probs")
      }
      prob_mat <- normalize_prob_matrix(prob_mat, response_levels = response_levels)
    }

    response_levels <- colnames(prob_mat)
    if (
      is.null(se_prob_mat) ||
      !is.matrix(se_prob_mat) ||
      !identical(dim(se_prob_mat), dim(prob_mat))
    ) {
      se_prob_mat <- matrix(
        NA_real_,
        nrow = nrow(prob_mat),
        ncol = ncol(prob_mat),
        dimnames = dimnames(prob_mat)
      )
    }

    crit <- qnorm(0.975)
    prob_low_mat <- prob_mat - crit * se_prob_mat
    prob_high_mat <- prob_mat + crit * se_prob_mat
    prob_low_mat[] <- pmax(0, prob_low_mat)
    prob_high_mat[] <- pmin(1, prob_high_mat)
    prob_low_mat[!is.finite(prob_low_mat)] <- NA_real_
    prob_high_mat[!is.finite(prob_high_mat)] <- NA_real_

    pred_long <- do.call(rbind, lapply(seq_along(response_levels), function(j) {
      out <- pred_grid
      out$.outcome <- response_levels[[j]]
      out$prob <- as.numeric(prob_mat[, j])
      out$prob_low <- as.numeric(prob_low_mat[, j])
      out$prob_high <- as.numeric(prob_high_mat[, j])
      out
    }))

    if (is.factor(model_df[[x_var]])) {
      x_display_levels <- resolve_prediction_level_order(levels(model_df[[x_var]]), level_orders[[x_var]])
      x_labels <- as.character(pred_long[[x_var]])
      pred_long$.x <- factor(x_labels, levels = x_display_levels)
    } else {
      pred_long$.x <- pred_long[[x_var]]
    }
    if (is.null(group_var) || group_var == x_var) {
      pred_long$.group <- factor("All")
    } else {
      if (is.factor(model_df[[group_var]])) {
        group_display_levels <- resolve_prediction_level_order(levels(model_df[[group_var]]), level_orders[[group_var]])
        pred_long$.group <- factor(as.character(pred_long[[group_var]]), levels = group_display_levels)
      } else {
        pred_long$.group <- factor(pred_long[[group_var]])
      }
    }
    pred_long$.outcome <- factor(pred_long$.outcome, levels = response_levels)
    pred_long$.x_var <- x_var
    pred_long$.group_var <- if (is.null(group_var)) "None" else group_var
    return(pred_long)
  }

  mm <- model.matrix(fixed_terms, pred_grid)
  beta <- get_model_coefficients(model)
  missing_cols <- setdiff(names(beta), colnames(mm))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) mm <- cbind(mm, 0)
    colnames(mm)[(ncol(mm) - length(missing_cols) + 1):ncol(mm)] <- missing_cols
  }
  mm <- mm[, names(beta), drop = FALSE]

  eta <- as.numeric(mm %*% beta)
  vcv <- get_model_vcov(model, model_df = model_df)
  se_eta <- sqrt(pmax(0, diag(mm %*% vcv %*% t(mm))))
  crit <- qnorm(0.975)

  pred_grid$prob <- plogis(eta)
  pred_grid$prob_low <- plogis(eta - crit * se_eta)
  pred_grid$prob_high <- plogis(eta + crit * se_eta)
  if (is.factor(model_df[[x_var]])) {
    x_display_levels <- resolve_prediction_level_order(levels(model_df[[x_var]]), level_orders[[x_var]])
    x_labels <- as.character(pred_grid[[x_var]])
    pred_grid$.x <- factor(x_labels, levels = x_display_levels)
  } else {
    pred_grid$.x <- pred_grid[[x_var]]
  }
  if (is.null(group_var) || group_var == x_var) {
    pred_grid$.group <- factor("All")
  } else {
    if (is.factor(model_df[[group_var]])) {
      group_display_levels <- resolve_prediction_level_order(levels(model_df[[group_var]]), level_orders[[group_var]])
      pred_grid$.group <- factor(as.character(pred_grid[[group_var]]), levels = group_display_levels)
    } else {
      pred_grid$.group <- factor(pred_grid[[group_var]])
    }
  }
  pred_grid$.x_var <- x_var
  pred_grid$.group_var <- if (is.null(group_var)) "None" else group_var
  pred_grid
}

prediction_bar_width <- function(x) {
  if (is.factor(x) || is.character(x)) return(0.75)

  x_num <- suppressWarnings(as.numeric(x))
  x_num <- sort(unique(x_num[is.finite(x_num)]))
  if (length(x_num) < 2) return(0.75)

  step <- min(diff(x_num))
  if (!is.finite(step) || step <= 0) return(0.75)
  step * 0.8
}

clamp_probability <- function(x, eps = 1e-6) {
  pmin(pmax(as.numeric(x), eps), 1 - eps)
}

make_prediction_violin_data <- function(pred, n_points = 128L) {
  if (nrow(pred) == 0) {
    out <- data.frame(
      .x = pred$.x[FALSE],
      .group = pred$.group[FALSE],
      .draw_prob = numeric(0)
    )
    if (".outcome" %in% names(pred)) {
      out$.outcome <- pred$.outcome[FALSE]
    }
    return(out)
  }

  crit <- qnorm(0.975)
  probs <- clamp_probability(pred$prob)
  lows <- clamp_probability(ifelse(is.finite(pred$prob_low), pred$prob_low, pred$prob))
  highs <- clamp_probability(ifelse(is.finite(pred$prob_high), pred$prob_high, pred$prob))
  eta <- qlogis(probs)
  se_eta <- (qlogis(highs) - qlogis(lows)) / (2 * crit)
  se_eta[!is.finite(se_eta) | se_eta <= 0] <- 1e-6
  z_grid <- qnorm(ppoints(as.integer(n_points)))

  out <- do.call(rbind, lapply(seq_len(nrow(pred)), function(i) {
    row <- data.frame(
      .x = pred$.x[i],
      .group = pred$.group[i],
      .draw_prob = plogis(eta[i] + z_grid * se_eta[i]),
      stringsAsFactors = FALSE
    )
    if (".outcome" %in% names(pred)) {
      row$.outcome <- as.character(pred$.outcome[i])
    }
    row
  }))

  if (is.factor(pred$.x)) {
    out$.x <- factor(out$.x, levels = levels(pred$.x))
  } else {
    out$.x <- as.numeric(out$.x)
  }
  out$.group <- factor(out$.group, levels = levels(pred$.group))
  if (".outcome" %in% names(pred)) {
    outcome_levels <- if (is.factor(pred$.outcome)) levels(pred$.outcome) else unique(as.character(pred$.outcome))
    out$.outcome <- factor(out$.outcome, levels = outcome_levels)
  }
  out
}

default_prediction_level_palette <- function(n) {
  if (n <= 0) return(character(0))
  grDevices::hcl.colors(n, palette = "Dark 3")
}

prediction_level_specs_from_model_df <- function(model_df, fixed_effects) {
  out <- list()
  if (is.null(model_df) || !is.data.frame(model_df) || length(fixed_effects) == 0) {
    return(out)
  }

  for (v in fixed_effects) {
    if (!v %in% names(model_df) || !is.factor(model_df[[v]])) next
    out[[v]] <- levels(model_df[[v]])
  }

  out
}

default_prediction_level_order <- function(levels) {
  sort(unique(as.character(levels %||% character(0))))
}

normalize_prediction_level_orders <- function(level_specs, orders = list()) {
  if (!is.list(level_specs) || length(level_specs) == 0) return(list())

  out <- list()
  for (v in names(level_specs)) {
    levels_v <- default_prediction_level_order(level_specs[[v]])
    if (length(levels_v) == 0) next

    current <- as.character(orders[[v]] %||% character(0))
    current <- current[current %in% levels_v]
    current <- current[!duplicated(current)]
    out[[v]] <- c(current, setdiff(levels_v, current))
  }

  out
}

is_valid_hex_color <- function(x) {
  grepl("^#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?$", as.character(x %||% ""))
}

normalize_prediction_level_colors <- function(level_specs, colors = list()) {
  if (!is.list(level_specs) || length(level_specs) == 0) return(list())

  out <- list()
  for (v in names(level_specs)) {
    levels_v <- as.character(level_specs[[v]] %||% character(0))
    if (length(levels_v) == 0) next

    defaults <- setNames(default_prediction_level_palette(length(levels_v)), levels_v)
    current <- colors[[v]]
    if (!is.null(current)) {
      current <- unlist(current, use.names = TRUE)
      current_names <- names(current)
      current <- as.character(current)
      if (is.null(current_names) && length(current) > 0) {
        current_names <- levels_v[seq_len(min(length(current), length(levels_v)))]
      }
      if (!is.null(current_names)) {
        names(current) <- current_names
        matches <- intersect(levels_v, names(current))
        for (lvl in matches) {
          if (is_valid_hex_color(current[[lvl]])) {
            defaults[[lvl]] <- current[[lvl]]
          }
        }
      }
    }

    out[[v]] <- defaults
  }

  out
}

normalize_prediction_level_color_store <- function(colors = list()) {
  if (!is.list(colors) || length(colors) == 0) return(list())

  out <- list()
  for (v in names(colors)) {
    colors_v <- unlist(colors[[v]], use.names = TRUE)
    color_names <- names(colors_v)
    colors_v <- as.character(colors_v)
    if (length(colors_v) == 0 || is.null(color_names)) next

    keep <- !is.na(color_names) & nzchar(color_names) & is_valid_hex_color(colors_v)
    colors_v <- colors_v[keep]
    color_names <- color_names[keep]
    if (length(colors_v) == 0) next

    dedupe <- !duplicated(color_names)
    colors_v <- colors_v[dedupe]
    names(colors_v) <- color_names[dedupe]
    out[[v]] <- colors_v
  }

  out
}

merge_prediction_level_color_store <- function(base = list(), update = list()) {
  base <- normalize_prediction_level_color_store(base)
  update <- normalize_prediction_level_color_store(update)
  if (length(update) == 0) return(base)

  out <- base
  for (v in names(update)) {
    existing <- out[[v]]
    merged <- if (is.null(existing)) {
      update[[v]]
    } else {
      c(existing[setdiff(names(existing), names(update[[v]]))], update[[v]])
    }
    out[[v]] <- merged
  }

  out
}

normalize_prediction_level_order_store <- function(orders = list()) {
  if (!is.list(orders) || length(orders) == 0) return(list())

  out <- list()
  for (v in names(orders)) {
    order_v <- as.character(unlist(orders[[v]], use.names = FALSE))
    order_v <- order_v[!is.na(order_v) & nzchar(order_v)]
    order_v <- order_v[!duplicated(order_v)]
    if (length(order_v) == 0) next
    out[[v]] <- order_v
  }

  out
}

merge_prediction_level_order_store <- function(base = list(), update = list()) {
  base <- normalize_prediction_level_order_store(base)
  update <- normalize_prediction_level_order_store(update)
  if (length(update) == 0) return(base)

  out <- base
  for (v in names(update)) {
    existing <- out[[v]]
    merged <- if (is.null(existing)) {
      update[[v]]
    } else {
      c(update[[v]], setdiff(existing, update[[v]]))
    }
    out[[v]] <- merged
  }

  out
}

resolve_prediction_level_order <- function(levels, order = NULL) {
  normalize_prediction_level_orders(list(.order = levels), list(.order = order %||% character(0)))$.order
}

encode_prediction_input_piece <- function(x) {
  bytes <- as.integer(charToRaw(enc2utf8(as.character(x %||% ""))))
  if (length(bytes) == 0) return("empty")
  paste(sprintf("%02x", bytes), collapse = "")
}

prediction_color_input_id <- function(variable, level) {
  paste0("pred_level_color__", encode_prediction_input_piece(variable), "__", encode_prediction_input_piece(level))
}

prediction_level_order_input_id <- function(variable) {
  paste0("pred_level_order__", encode_prediction_input_piece(variable))
}

prediction_level_order_container_id <- function(variable) {
  paste0("pred_level_order_container__", encode_prediction_input_piece(variable))
}

resolve_prediction_palette <- function(levels, colors = NULL) {
  normalize_prediction_level_colors(list(.palette = levels), list(.palette = colors))$.palette
}

prediction_plot_color_spec <- function(pred, level_colors = list()) {
  x_var <- unique(pred$.x_var)[1]
  group_var <- unique(pred$.group_var)[1]
  has_group <- !identical(group_var, "None") && length(unique(as.character(pred$.group))) > 1

  if (has_group) {
    group_levels <- if (is.factor(pred$.group)) levels(pred$.group) else unique(as.character(pred$.group))
    return(list(
      mode = "group",
      label = group_var,
      values = resolve_prediction_palette(group_levels, level_colors[[group_var]]),
      show_legend = TRUE
    ))
  }

  if (is.factor(pred$.x)) {
    x_levels <- levels(pred$.x)
    return(list(
      mode = "x",
      label = x_var,
      values = resolve_prediction_palette(x_levels, level_colors[[x_var]]),
      show_legend = TRUE
    ))
  }

  list(
    mode = "single",
    label = x_var,
    values = c(All = "#4C78A8"),
    show_legend = FALSE
  )
}

format_prediction_percent <- function(x, digits = 1L) {
  paste0(formatC(100 * as.numeric(x), format = "f", digits = digits), "%")
}

save_plot_svg <- function(plot, filename, width = 9, height = 5.5) {
  grDevices::svg(filename = filename, width = width, height = height, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
  invisible(TRUE)
}

prediction_observed_point_shape <- function(use_cross = TRUE) {
  if (isTRUE(use_cross)) 4 else 16
}

prediction_observed_point_color <- function() {
  "#666666"
}

prediction_observed_point_position <- function(x, width, dodge_width = NULL, jitter = TRUE, seed = 1L) {
  if (!is.factor(x)) {
    if (!is.null(dodge_width)) return(ggplot2::position_dodge(width = dodge_width))
    return(ggplot2::position_identity())
  }

  if (isTRUE(jitter)) {
    if (!is.null(dodge_width)) {
      return(ggplot2::position_jitterdodge(
        jitter.width = width * 0.08,
        jitter.height = 0,
        dodge.width = dodge_width,
        seed = seed
      ))
    }
    return(ggplot2::position_jitter(width = width * 0.08, height = 0, seed = seed))
  }

  if (!is.null(dodge_width)) return(ggplot2::position_dodge(width = dodge_width))
  ggplot2::position_identity()
}

prediction_plot_row_colors <- function(data, color_spec) {
  if (nrow(data) == 0) return(character(0))

  if (identical(color_spec$mode, "group")) {
    keys <- as.character(data$.group)
  } else if (identical(color_spec$mode, "x")) {
    keys <- as.character(data$.x)
  } else {
    keys <- rep(names(color_spec$values)[1], nrow(data))
  }

  out <- unname(color_spec$values[keys])
  fallback <- unname(color_spec$values[[1]])
  out[is.na(out)] <- fallback
  out
}

individual_prediction_needs_legend <- function(pred, response_info = NULL) {
  response_info <- normalize_response_info(response_info)
  if (is.null(pred) || !is.data.frame(pred) || nrow(pred) == 0) return(FALSE)

  if (response_is_multinomial(response_info) || ".outcome" %in% names(pred)) {
    return(TRUE)
  }

  if (".group" %in% names(pred)) {
    group_vals <- unique(as.character(pred$.group))
    group_vals <- group_vals[!is.na(group_vals) & nzchar(group_vals)]
    if (length(group_vals) > 1L || (length(group_vals) == 1L && !identical(group_vals, "All"))) {
      return(TRUE)
    }
  }

  is.factor(pred$.x) && length(levels(pred$.x)) > 1L
}

individual_prediction_color_explanation <- function(
  pred,
  response_info = NULL,
  show_observed_points = FALSE
) {
  response_info <- normalize_response_info(response_info)
  if (is.null(pred) || !is.data.frame(pred) || nrow(pred) == 0) return("No color mapping available.")

  x_var <- unique(as.character(pred$.x_var %||% ""))[1] %||% "x"
  parts <- character(0)

  if (response_is_multinomial(response_info) || ".outcome" %in% names(pred)) {
    parts <- c(parts, "Colors denote outcome levels.")
  } else if (".group" %in% names(pred)) {
    group_vals <- unique(as.character(pred$.group))
    group_vals <- group_vals[!is.na(group_vals) & nzchar(group_vals)]
    if (length(group_vals) > 1L || (length(group_vals) == 1L && !identical(group_vals, "All"))) {
      group_var <- unique(as.character(pred$.group_var %||% ""))[1] %||% "group"
      parts <- c(parts, sprintf("Colors denote %s levels.", group_var))
    } else if (is.factor(pred$.x) && length(levels(pred$.x)) > 1L) {
      parts <- c(parts, sprintf("Colors denote %s levels.", x_var))
    } else {
      parts <- c(parts, "A single color is used because this chart shows one prediction series.")
    }
  } else if (is.factor(pred$.x) && length(levels(pred$.x)) > 1L) {
    parts <- c(parts, sprintf("Colors denote %s levels.", x_var))
  } else {
    parts <- c(parts, "A single color is used because this chart shows one prediction series.")
  }

  if (isTRUE(show_observed_points)) {
    parts <- c(parts, "Black points are observed datapoints per lemma.")
  }

  paste(parts, collapse = " ")
}

make_prediction_label_data <- function(pred, color_spec, chart_type = "line") {
  out <- pred
  offset <- if (identical(chart_type, "line")) 0.03 else 0.025
  out$.label <- format_prediction_percent(pred$prob)
  out$.label_y <- pmin(pred$prob_high + offset, 0.985)
  out$.plot_color <- prediction_plot_row_colors(pred, color_spec)
  out
}

make_prediction_observed_data <- function(observed_data, pred) {
  x_var <- unique(pred$.x_var)[1]
  group_var <- unique(pred$.group_var)[1]

  if (is.null(observed_data) || !is.data.frame(observed_data) || nrow(observed_data) == 0) {
    return(NULL)
  }
  if (!x_var %in% names(observed_data)) return(NULL)

  if (".outcome" %in% names(pred)) {
    keep_cols <- unique(c(
      "response_value",
      ".case_weight",
      x_var,
      if (!identical(group_var, "None")) group_var,
      if (".observed_lemma" %in% names(observed_data)) ".observed_lemma",
      if ("lemma" %in% names(observed_data)) "lemma"
    ))
    keep_cols <- keep_cols[keep_cols %in% names(observed_data)]
    out <- observed_data[, keep_cols, drop = FALSE]
    if (!"response_value" %in% names(out)) return(NULL)

    if (!".case_weight" %in% names(out)) {
      out$.case_weight <- 1
    }
    out$response_value <- as.character(out$response_value)
    valid_rows <- !is.na(out$response_value) & nzchar(out$response_value) &
      !is.na(out$.case_weight) & is.finite(out$.case_weight) & out$.case_weight > 0
    out <- out[valid_rows, , drop = FALSE]
    if (nrow(out) == 0) return(NULL)

    has_lemma <- ".observed_lemma" %in% names(out) || "lemma" %in% names(out)
    if (has_lemma) {
      lemma_vals <- if (".observed_lemma" %in% names(out)) {
        as.character(out$.observed_lemma)
      } else {
        as.character(out$lemma)
      }
      lemma_vals[is.na(lemma_vals) | !nzchar(lemma_vals)] <- NA_character_
      out$.observed_lemma <- lemma_vals
    }

    base_cols <- x_var
    if (!identical(group_var, "None") && group_var %in% names(out)) {
      base_cols <- c(base_cols, group_var)
    }
    if (has_lemma && any(!is.na(out$.observed_lemma))) {
      base_cols <- c(".observed_lemma", base_cols)
    }
    base_cols <- unique(base_cols)

    totals <- aggregate(
      x = list(.obs_total = as.numeric(out$.case_weight)),
      by = out[base_cols],
      FUN = sum
    )
    counts <- aggregate(
      x = list(.obs_n = as.numeric(out$.case_weight)),
      by = out[c(base_cols, "response_value")],
      FUN = sum
    )

    outcome_levels <- if (is.factor(pred$.outcome)) levels(pred$.outcome) else unique(as.character(pred$.outcome))
    base_keys <- unique(out[base_cols])
    base_keys$.join_key <- 1L
    outcome_grid <- data.frame(
      .outcome = outcome_levels,
      .join_key = 1L,
      stringsAsFactors = FALSE
    )
    out <- merge(base_keys, outcome_grid, by = ".join_key", all = TRUE, sort = FALSE)
    out$.join_key <- NULL

    counts$.outcome <- counts$response_value
    counts$response_value <- NULL
    out <- merge(out, totals, by = base_cols, all.x = TRUE, sort = FALSE)
    out <- merge(out, counts, by = c(base_cols, ".outcome"), all.x = TRUE, sort = FALSE)
    out$.obs_n[is.na(out$.obs_n)] <- 0
    out <- out[is.finite(out$.obs_total) & out$.obs_total > 0, , drop = FALSE]
    if (nrow(out) == 0) return(NULL)

    out$.obs_prob <- out$.obs_n / out$.obs_total
    out$.outcome <- factor(out$.outcome, levels = outcome_levels)

    if (is.factor(pred$.x)) {
      x_levels <- levels(pred$.x)
      out$.x <- factor(as.character(out[[x_var]]), levels = x_levels)
      out <- out[!is.na(out$.x), , drop = FALSE]
    } else {
      out$.x <- suppressWarnings(as.numeric(out[[x_var]]))
      out <- out[is.finite(out$.x), , drop = FALSE]
    }
    if (nrow(out) == 0) return(NULL)

    has_group <- !identical(group_var, "None") &&
      group_var %in% names(out) &&
      length(unique(as.character(pred$.group))) > 1L
    if (has_group) {
      group_levels <- if (is.factor(pred$.group)) levels(pred$.group) else unique(as.character(pred$.group))
      out$.group <- factor(as.character(out[[group_var]]), levels = group_levels)
      out <- out[!is.na(out$.group), , drop = FALSE]
    } else {
      base_levels <- if (is.factor(pred$.group)) levels(pred$.group) else "All"
      out$.group <- factor("All", levels = base_levels %||% "All")
    }
    if (nrow(out) == 0) return(NULL)

    if (has_lemma && ".observed_lemma" %in% names(out)) {
      out$.point_label <- as.character(out$.observed_lemma)
      out$.point_label[is.na(out$.point_label) | !nzchar(out$.point_label)] <- NA_character_
    } else {
      out$.point_label <- NA_character_
    }

    return(out)
  }

  keep_cols <- unique(c(
    "plural_successes",
    "plural_failures",
    x_var,
    if (!identical(group_var, "None")) group_var,
    if (".observed_lemma" %in% names(observed_data)) ".observed_lemma",
    if ("lemma" %in% names(observed_data)) "lemma"
  ))
  keep_cols <- keep_cols[keep_cols %in% names(observed_data)]
  out <- observed_data[, keep_cols, drop = FALSE]

  valid_counts <- !is.na(out$plural_successes) & !is.na(out$plural_failures) &
    (out$plural_successes + out$plural_failures) > 0
  out <- out[valid_counts, , drop = FALSE]
  if (nrow(out) == 0) return(NULL)

  has_lemma <- ".observed_lemma" %in% names(out) || "lemma" %in% names(out)
  if (has_lemma) {
    lemma_vals <- if (".observed_lemma" %in% names(out)) {
      as.character(out$.observed_lemma)
    } else {
      as.character(out$lemma)
    }
    lemma_vals[is.na(lemma_vals) | !nzchar(lemma_vals)] <- NA_character_
    out$.observed_lemma <- lemma_vals
  }

  grouping_cols <- character(0)
  if (has_lemma && any(!is.na(out$.observed_lemma))) {
    grouping_cols <- c(".observed_lemma", x_var)
    if (!identical(group_var, "None") && group_var %in% names(out)) {
      grouping_cols <- c(grouping_cols, group_var)
    }
  }

  if (length(grouping_cols) > 0 && nrow(out) > 1L) {
    out <- aggregate(
      x = out[c("plural_successes", "plural_failures")],
      by = out[grouping_cols],
      FUN = sum
    )
  }

  out$.obs_total <- out$plural_successes + out$plural_failures
  out$.obs_prob <- out$plural_successes / out$.obs_total

  if (is.factor(pred$.x)) {
    x_levels <- levels(pred$.x)
    out$.x <- factor(as.character(out[[x_var]]), levels = x_levels)
    out <- out[!is.na(out$.x), , drop = FALSE]
  } else {
    out$.x <- suppressWarnings(as.numeric(out[[x_var]]))
    out <- out[is.finite(out$.x), , drop = FALSE]
  }
  if (nrow(out) == 0) return(NULL)

  has_group <- !identical(group_var, "None") &&
    group_var %in% names(out) &&
    length(unique(as.character(pred$.group))) > 1L
  if (has_group) {
    group_levels <- if (is.factor(pred$.group)) levels(pred$.group) else unique(as.character(pred$.group))
    out$.group <- factor(as.character(out[[group_var]]), levels = group_levels)
    out <- out[!is.na(out$.group), , drop = FALSE]
  } else {
    base_levels <- if (is.factor(pred$.group)) levels(pred$.group) else "All"
    out$.group <- factor("All", levels = base_levels %||% "All")
  }
  if (nrow(out) == 0) return(NULL)

  if (has_lemma && ".observed_lemma" %in% names(out)) {
    out$.point_label <- as.character(out$.observed_lemma)
    out$.point_label[is.na(out$.point_label) | !nzchar(out$.point_label)] <- NA_character_
  } else {
    out$.point_label <- NA_character_
  }

  out
}

make_prediction_observed_label_data <- function(observed_data, offset = 0.015) {
  if (is.null(observed_data) || !is.data.frame(observed_data) || nrow(observed_data) == 0) {
    return(data.frame())
  }

  out <- observed_data[!is.na(observed_data$.point_label) & nzchar(observed_data$.point_label), , drop = FALSE]
  if (nrow(out) == 0) return(out)

  out$.label_y <- pmin(out$.obs_prob + offset, 0.985)
  out
}

add_prediction_observed_labels <- function(
  p,
  observed_label_data,
  position = ggplot2::position_identity(),
  group_var = ".group",
  size = 2.5
) {
  if (is.null(observed_label_data) || !is.data.frame(observed_label_data) || nrow(observed_label_data) == 0) {
    return(p)
  }

  label_aes <- if (identical(group_var, ".label_group")) {
    aes(x = .x, y = .label_y, label = .point_label, group = .label_group)
  } else {
    aes(x = .x, y = .label_y, label = .point_label, group = .group)
  }

  if (requireNamespace("ggrepel", quietly = TRUE)) {
    return(p + ggrepel::geom_text_repel(
      data = observed_label_data,
      mapping = label_aes,
      position = position,
      size = size,
      color = "black",
      min.segment.length = 0,
      segment.color = "#999999",
      segment.size = 0.25,
      box.padding = 0.18,
      point.padding = 0.12,
      max.overlaps = Inf,
      show.legend = FALSE,
      inherit.aes = FALSE
    ))
  }

  p + geom_text(
    data = observed_label_data,
    mapping = label_aes,
    position = position,
    size = size,
    vjust = 0,
    alpha = 0.9,
    color = "black",
    check_overlap = TRUE,
    show.legend = FALSE,
    inherit.aes = FALSE
  )
}

make_prediction_observed_hover_text <- function(observed_data, pred = NULL, response_info = NULL) {
  if (is.null(observed_data) || !is.data.frame(observed_data) || nrow(observed_data) == 0) {
    return(character(0))
  }

  x_var <- if (!is.null(pred) && ".x_var" %in% names(pred)) unique(as.character(pred$.x_var))[1] else "x"
  group_var <- if (!is.null(pred) && ".group_var" %in% names(pred)) unique(as.character(pred$.group_var))[1] else "Group"
  response_info <- normalize_response_info(response_info)
  prob_label <- if (response_is_multinomial(response_info) && ".outcome" %in% names(observed_data)) {
    paste("Observed probability of", as.character(observed_data$.outcome))
  } else {
    rep(paste("Observed probability of", response_info$success_label %||% "success"), nrow(observed_data))
  }

  x_val <- if (x_var %in% names(observed_data)) {
    as.character(observed_data[[x_var]])
  } else {
    as.character(observed_data$.x)
  }
  lemma <- observed_data$.point_label %||% rep(NA_character_, nrow(observed_data))
  lemma[is.na(lemma) | !nzchar(lemma)] <- "(unlabeled)"

  parts <- Map(function(i) {
    out <- c(
      paste0("Lemma: ", lemma[[i]]),
      paste0(x_var, ": ", x_val[[i]])
    )
    if (!identical(group_var, "None") && ".group" %in% names(observed_data)) {
      out <- c(out, paste0(group_var, ": ", as.character(observed_data$.group[[i]])))
    }
    if (".outcome" %in% names(observed_data)) {
      out <- c(out, paste0("Outcome: ", as.character(observed_data$.outcome[[i]])))
    }
    if (".obs_n" %in% names(observed_data) && ".obs_total" %in% names(observed_data)) {
      out <- c(out, paste0("Observed count: ", format(observed_data$.obs_n[[i]], big.mark = ","), " / ", format(observed_data$.obs_total[[i]], big.mark = ",")))
    } else if ("plural_successes" %in% names(observed_data) && ".obs_total" %in% names(observed_data)) {
      out <- c(out, paste0("Observed count: ", format(observed_data$plural_successes[[i]], big.mark = ","), " / ", format(observed_data$.obs_total[[i]], big.mark = ",")))
    }
    out <- c(out, paste0(prob_label[[i]], ": ", format_prediction_percent(observed_data$.obs_prob[[i]])))
    paste(out, collapse = "<br>")
  }, seq_len(nrow(observed_data)))

  unlist(parts, use.names = FALSE)
}

restore_observed_prediction_data <- function(model_df, raw_df, res, dataset_filters = list()) {
  if (!is.data.frame(model_df)) return(model_df)

  existing <- attr(model_df, "observed_prediction_data", exact = TRUE)
  if (is.data.frame(existing) && nrow(existing) > 0 && ".observed_lemma" %in% names(existing)) {
    return(model_df)
  }
  if (is.null(raw_df) || !is.data.frame(raw_df)) {
    return(model_df)
  }

  opts <- list(
    response_key = res$response_key %||% "",
    fixed_effects = res$fixed_effects %||% character(0),
    interaction_terms = res$interaction_terms %||% character(0),
    random_effects = res$random_effects %||% character(0),
    reference_levels = res$reference_levels %||% list(),
    response_reference_level = res$response_reference_level %||% (res$response_info %||% list())$reference_level %||% NULL
  )

  rebuilt <- tryCatch(
    prepare_data(apply_dataset_filters(raw_df, dataset_filters), opts),
    error = function(e) NULL
  )
  if (is.null(rebuilt)) return(model_df)

  observed_data <- attr(rebuilt, "observed_prediction_data", exact = TRUE)
  if (is.data.frame(observed_data) && nrow(observed_data) > 0) {
    attr(model_df, "observed_prediction_data") <- observed_data
  }
  model_df
}

build_multinomial_prediction_plot <- function(
  pred,
  response_info = NULL,
  chart_type = "line",
  level_colors = list(),
  show_labels = FALSE,
  observed_data = NULL,
  show_observed_points = FALSE,
  observed_point_jitter = TRUE,
  observed_point_cross = TRUE
) {
  x_var <- unique(pred$.x_var)[1]
  group_var <- unique(pred$.group_var)[1]
  chart_type <- as.character(chart_type %||% "line")
  if (!(chart_type %in% c("line", "bar", "violin"))) chart_type <- "line"
  show_labels <- isTRUE(show_labels)
  show_observed_points <- isTRUE(show_observed_points) && identical(chart_type %in% c("bar", "violin"), TRUE)
  observed_point_jitter <- isTRUE(observed_point_jitter)
  observed_point_shape <- prediction_observed_point_shape(observed_point_cross)

  has_group <- !identical(group_var, "None") && length(unique(as.character(pred$.group))) > 1L
  outcome_levels <- if (is.factor(pred$.outcome)) levels(pred$.outcome) else unique(as.character(pred$.outcome))
  pred_point_data <- pred
  observed_plot_data <- NULL
  point_label_position <- NULL
  pred_label_data <- pred
  pred_label_data$.label <- format_prediction_percent(pred$prob)
  pred_label_anchor <- ifelse(is.finite(pred$prob_high), pred$prob_high, pred$prob)
  pred_label_data$.label_y <- pmin(pred_label_anchor + 0.03, 0.985)

  if (show_observed_points) {
    observed_plot_data <- make_prediction_observed_data(observed_data, pred)
    if (is.null(observed_plot_data) || nrow(observed_plot_data) == 0) {
      show_observed_points <- FALSE
    } else {
      observed_plot_data$.hover_text <- make_prediction_observed_hover_text(observed_plot_data, pred, response_info)
    }
  }

  if (has_group) {
    group_levels <- if (is.factor(pred$.group)) levels(pred$.group) else unique(as.character(pred$.group))
    group_colors <- resolve_prediction_palette(group_levels, level_colors[[group_var]])
    if (identical(chart_type, "bar")) {
      bar_span <- prediction_bar_width(pred$.x)
      dodge <- ggplot2::position_dodge(width = bar_span)
      n_groups <- max(1L, length(group_levels))
      col_width <- if (n_groups > 1L) bar_span / n_groups * 0.9 else bar_span * 0.9
      err_width <- col_width * 0.35
      p <- ggplot(pred, aes(x = .x, y = prob, fill = .group)) +
        geom_col(position = dodge, width = col_width, alpha = 0.85, color = "gray30") +
        geom_errorbar(
          aes(ymin = prob_low, ymax = prob_high, group = .group),
          position = dodge,
          width = err_width,
          color = "gray20"
        ) +
        facet_wrap(~ .outcome) +
        scale_fill_manual(values = group_colors)
      if (show_labels) {
        p <- p + geom_text(
          data = pred_label_data,
          aes(label = .label, y = .label_y, group = .group),
          position = dodge,
          size = 3
        )
      }
      if (show_observed_points) {
        point_position <- prediction_observed_point_position(
          pred$.x,
          bar_span,
          dodge_width = bar_span,
          jitter = observed_point_jitter
        )
        p <- p +
          geom_point(
            data = observed_plot_data,
            aes(x = .x, y = .obs_prob, group = .group, text = .hover_text),
            position = point_position,
            shape = observed_point_shape,
            size = if (identical(observed_point_shape, 4)) 2.2 else 1.85,
            alpha = 0.72,
            color = prediction_observed_point_color(),
            show.legend = FALSE,
            inherit.aes = FALSE
          )
        point_label_position <- point_position
      }
      label_args <- list(x = x_var, y = "Predicted probability", fill = group_var, title = "Multinomial Predictions")
    } else if (identical(chart_type, "violin")) {
      violin_width <- prediction_bar_width(pred$.x)
      dodge <- ggplot2::position_dodge(width = violin_width)
      violin_data <- make_prediction_violin_data(pred)
      p <- ggplot() +
        geom_violin(
          data = violin_data,
          aes(x = .x, y = .draw_prob, fill = .group, group = interaction(.x, .group)),
          position = dodge,
          width = violin_width * 0.9,
          alpha = 0.3,
          color = NA,
          trim = TRUE
        ) +
        geom_linerange(
          data = pred,
          aes(x = .x, ymin = prob_low, ymax = prob_high, color = .group, group = .group),
          position = dodge,
          linewidth = 0.35
        ) +
        facet_wrap(~ .outcome) +
        scale_fill_manual(values = group_colors) +
        scale_color_manual(values = group_colors)
      if (show_labels) {
        p <- p + geom_text(
          data = pred_label_data,
          aes(x = .x, y = .label_y, label = .label, group = .group),
          position = dodge,
          size = 3,
          show.legend = FALSE
        )
      }
      if (show_observed_points) {
        point_position <- prediction_observed_point_position(
          pred$.x,
          violin_width,
          dodge_width = violin_width,
          jitter = observed_point_jitter
        )
        p <- p +
          geom_point(
            data = observed_plot_data,
            aes(x = .x, y = .obs_prob, group = .group, text = .hover_text),
            position = point_position,
            shape = observed_point_shape,
            size = if (identical(observed_point_shape, 4)) 2.2 else 1.85,
            alpha = 0.72,
            color = prediction_observed_point_color(),
            show.legend = FALSE,
            inherit.aes = FALSE
          )
        point_label_position <- point_position
      } else {
        pred_point_position <- if (is.factor(pred$.x)) {
          ggplot2::position_jitterdodge(
            jitter.width = violin_width * 0.08,
            jitter.height = 0,
            dodge.width = violin_width,
            seed = 1
          )
        } else {
          dodge
        }
        p <- p +
          geom_point(
            data = pred_point_data,
            aes(x = .x, y = prob, color = .group, group = .group),
            position = pred_point_position,
            size = 2.1,
            alpha = 0.95,
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      }
      if (!is.factor(pred$.x)) {
        p <- p +
          geom_line(
            data = pred,
            aes(x = .x, y = prob, color = .group, group = .group),
            linewidth = 0.55,
            alpha = 0.75
          )
      }
      label_args <- list(x = x_var, y = "Predicted probability", fill = group_var, title = "Multinomial Predictions")
    } else {
      if (is.factor(pred$.x)) {
        p <- ggplot(pred, aes(x = .x, y = prob, color = .group, group = .group)) +
          geom_line(linewidth = 0.9) +
          geom_point(size = 2) +
          geom_errorbar(aes(ymin = prob_low, ymax = prob_high), width = 0.12) +
          facet_wrap(~ .outcome) +
          scale_color_manual(values = group_colors)
      } else {
        p <- ggplot(pred, aes(x = .x, y = prob, color = .group, group = .group, fill = .group)) +
          geom_ribbon(aes(ymin = prob_low, ymax = prob_high), alpha = 0.15, color = NA, show.legend = FALSE) +
          geom_line(linewidth = 0.9) +
          geom_point(size = 2) +
          facet_wrap(~ .outcome) +
          scale_color_manual(values = group_colors) +
          scale_fill_manual(values = group_colors, guide = "none")
      }
      if (show_labels) {
        p <- p + geom_text(
          data = pred_label_data,
          aes(label = .label, y = .label_y),
          size = 3,
          show.legend = FALSE
        )
      }
      label_args <- list(x = x_var, y = "Predicted probability", color = group_var, title = "Multinomial Predictions")
    }
  } else {
    outcome_colors <- resolve_prediction_palette(outcome_levels, NULL)
    if (identical(chart_type, "bar")) {
      bar_span <- prediction_bar_width(pred$.x)
      dodge <- ggplot2::position_dodge(width = bar_span)
      n_outcomes <- max(1L, length(outcome_levels))
      col_width <- if (n_outcomes > 1L) bar_span / n_outcomes * 0.9 else bar_span * 0.9
      err_width <- col_width * 0.35
      p <- ggplot(pred, aes(x = .x, y = prob, fill = .outcome)) +
        geom_col(position = dodge, width = col_width, alpha = 0.85, color = "gray30") +
        geom_errorbar(
          aes(ymin = prob_low, ymax = prob_high, group = .outcome),
          position = dodge,
          width = err_width,
          color = "gray20"
        ) +
        scale_fill_manual(values = outcome_colors)
      if (show_labels) {
        p <- p + geom_text(
          data = pred_label_data,
          aes(label = .label, y = .label_y, group = .outcome),
          position = dodge,
          size = 3
        )
      }
      if (show_observed_points) {
        point_position <- prediction_observed_point_position(
          pred$.x,
          bar_span,
          dodge_width = bar_span,
          jitter = observed_point_jitter
        )
        p <- p +
          geom_point(
            data = observed_plot_data,
            aes(x = .x, y = .obs_prob, group = .outcome, text = .hover_text),
            position = point_position,
            shape = observed_point_shape,
            size = if (identical(observed_point_shape, 4)) 2.2 else 1.85,
            alpha = 0.72,
            color = prediction_observed_point_color(),
            show.legend = FALSE,
            inherit.aes = FALSE
          )
        point_label_position <- point_position
      }
      label_args <- list(x = x_var, y = "Predicted probability", fill = "Outcome", title = "Multinomial Predictions")
    } else if (identical(chart_type, "violin")) {
      violin_width <- prediction_bar_width(pred$.x)
      dodge <- ggplot2::position_dodge(width = violin_width)
      violin_data <- make_prediction_violin_data(pred)
      p <- ggplot() +
        geom_violin(
          data = violin_data,
          aes(x = .x, y = .draw_prob, fill = .outcome, group = interaction(.x, .outcome)),
          position = dodge,
          width = violin_width * 0.9,
          alpha = 0.3,
          color = NA,
          trim = TRUE
        ) +
        geom_linerange(
          data = pred,
          aes(x = .x, ymin = prob_low, ymax = prob_high, color = .outcome, group = .outcome),
          position = dodge,
          linewidth = 0.35
        ) +
        scale_fill_manual(values = outcome_colors) +
        scale_color_manual(values = outcome_colors)
      if (show_labels) {
        p <- p + geom_text(
          data = pred_label_data,
          aes(x = .x, y = .label_y, label = .label, group = .outcome),
          position = dodge,
          size = 3,
          show.legend = FALSE
        )
      }
      if (show_observed_points) {
        point_position <- prediction_observed_point_position(
          pred$.x,
          violin_width,
          dodge_width = violin_width,
          jitter = observed_point_jitter
        )
        p <- p +
          geom_point(
            data = observed_plot_data,
            aes(x = .x, y = .obs_prob, group = .outcome, text = .hover_text),
            position = point_position,
            shape = observed_point_shape,
            size = if (identical(observed_point_shape, 4)) 2.2 else 1.85,
            alpha = 0.72,
            color = prediction_observed_point_color(),
            show.legend = FALSE,
            inherit.aes = FALSE
          )
        point_label_position <- point_position
      } else {
        pred_point_position <- if (is.factor(pred$.x)) {
          ggplot2::position_jitterdodge(
            jitter.width = violin_width * 0.08,
            jitter.height = 0,
            dodge.width = violin_width,
            seed = 1
          )
        } else {
          dodge
        }
        p <- p +
          geom_point(
            data = pred_point_data,
            aes(x = .x, y = prob, color = .outcome, group = .outcome),
            position = pred_point_position,
            size = 2.1,
            alpha = 0.95,
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      }
      if (!is.factor(pred$.x)) {
        p <- p +
          geom_line(
            data = pred,
            aes(x = .x, y = prob, color = .outcome, group = .outcome),
            linewidth = 0.55,
            alpha = 0.75
          )
      }
      label_args <- list(x = x_var, y = "Predicted probability", fill = "Outcome", title = "Multinomial Predictions")
    } else {
      if (is.factor(pred$.x)) {
        p <- ggplot(pred, aes(x = .x, y = prob, color = .outcome, group = .outcome)) +
          geom_line(linewidth = 0.9) +
          geom_point(size = 2) +
          geom_errorbar(aes(ymin = prob_low, ymax = prob_high), width = 0.12) +
          scale_color_manual(values = outcome_colors)
      } else {
        p <- ggplot(pred, aes(x = .x, y = prob, color = .outcome, group = .outcome, fill = .outcome)) +
          geom_ribbon(aes(ymin = prob_low, ymax = prob_high), alpha = 0.15, color = NA, show.legend = FALSE) +
          geom_line(linewidth = 0.9) +
          geom_point(size = 2) +
          scale_color_manual(values = outcome_colors) +
          scale_fill_manual(values = outcome_colors, guide = "none")
      }
      if (show_labels) {
        p <- p + geom_text(
          data = pred_label_data,
          aes(label = .label, y = .label_y),
          size = 3,
          show.legend = FALSE
        )
      }
      label_args <- list(x = x_var, y = "Predicted probability", color = "Outcome", title = "Multinomial Predictions")
    }
  }

  if (show_observed_points && show_labels) {
    observed_label_data <- make_prediction_observed_label_data(observed_plot_data)
    if (nrow(observed_label_data) > 0) {
      observed_label_data$.label_group <- if (has_group) {
        as.character(observed_label_data$.group)
      } else {
        as.character(observed_label_data$.outcome)
      }
      p <- add_prediction_observed_labels(
        p,
        observed_label_data,
        position = point_label_position %||% ggplot2::position_identity(),
        group_var = ".label_group",
        size = 2.7
      )
    }
  }

  p +
    scale_y_continuous(
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0.02, if (show_labels) 0.08 else 0.03)),
      labels = function(x) paste0(round(x * 100, 1), "%")
    ) +
    do.call(labs, label_args) +
    theme_minimal(base_size = 12)
}

build_prediction_plot <- function(
  pred,
  response_info = NULL,
  chart_type = "line",
  level_colors = list(),
  show_labels = FALSE,
  observed_data = NULL,
  show_observed_points = FALSE,
  observed_point_jitter = TRUE,
  observed_point_cross = TRUE
) {
  x_var <- unique(pred$.x_var)[1]
  group_var <- unique(pred$.group_var)[1]
  response_info <- normalize_response_info(response_info)
  if (response_is_multinomial(response_info) || ".outcome" %in% names(pred)) {
    return(build_multinomial_prediction_plot(
      pred = pred,
      response_info = response_info,
      chart_type = chart_type,
      level_colors = level_colors,
      show_labels = show_labels,
      observed_data = observed_data,
      show_observed_points = show_observed_points,
      observed_point_jitter = observed_point_jitter,
      observed_point_cross = observed_point_cross
    ))
  }
  color_spec <- prediction_plot_color_spec(pred, level_colors = level_colors)
  single_color <- unname(color_spec$values[[1]])
  pred_point_data <- pred
  pred_point_data$.plot_color <- prediction_plot_row_colors(pred, color_spec)
  observed_plot_data <- NULL
  use_color_scale <- FALSE
  use_fill_scale <- FALSE
  color_guide <- "legend"
  fill_guide <- "legend"
  label_position <- NULL

  chart_type <- as.character(chart_type %||% "line")
  if (!(chart_type %in% c("line", "bar", "violin"))) chart_type <- "line"
  show_labels <- isTRUE(show_labels)
  show_observed_points <- isTRUE(show_observed_points) && identical(chart_type %in% c("bar", "violin"), TRUE)
  observed_point_jitter <- isTRUE(observed_point_jitter)
  observed_point_shape <- prediction_observed_point_shape(observed_point_cross)

  if (show_observed_points) {
    observed_plot_data <- make_prediction_observed_data(observed_data, pred)
    if (is.null(observed_plot_data) || nrow(observed_plot_data) == 0) {
      show_observed_points <- FALSE
    } else {
      observed_plot_data$.hover_text <- make_prediction_observed_hover_text(observed_plot_data, pred, response_info)
    }
  }

  group_label <- if (identical(group_var, "None")) "Group" else group_var

  if (identical(chart_type, "bar")) {
    n_groups <- max(1L, length(unique(as.character(pred$.group))))
    bar_span <- prediction_bar_width(pred$.x)
    dodge <- ggplot2::position_dodge(width = bar_span)
    col_width <- if (n_groups > 1L) bar_span / n_groups * 0.9 else bar_span * 0.9
    err_width <- col_width * 0.35

    if (identical(color_spec$mode, "group")) {
      use_fill_scale <- TRUE
      label_position <- dodge
      p <- ggplot(pred, aes(x = .x, y = prob, fill = .group, group = .group)) +
        geom_col(position = dodge, width = col_width, alpha = 0.85, color = "gray30") +
        geom_errorbar(
          aes(ymin = prob_low, ymax = prob_high),
          position = dodge,
          width = err_width,
          color = "gray20"
        )
      if (show_observed_points) {
        point_position <- prediction_observed_point_position(
          pred$.x,
          bar_span,
          dodge_width = bar_span,
          jitter = observed_point_jitter
        )
        p <- p +
          geom_point(
            data = observed_plot_data,
            aes(x = .x, y = .obs_prob, group = .group, text = .hover_text),
            position = point_position,
            shape = observed_point_shape,
            size = if (identical(observed_point_shape, 4)) 2.2 else 1.85,
            alpha = 0.72,
            color = prediction_observed_point_color(),
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      } else {
        p <- p +
          geom_point(
            data = pred_point_data,
            aes(x = .x, y = prob, group = .group, color = I(.plot_color)),
            position = dodge,
            size = 2.2,
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      }
    } else if (identical(color_spec$mode, "x")) {
      use_color_scale <- TRUE
      use_fill_scale <- TRUE
      color_guide <- "none"
      p <- ggplot(pred, aes(x = .x, y = prob, fill = .x, color = .x)) +
        geom_col(width = col_width, alpha = 0.85, color = "gray30") +
        geom_errorbar(
          aes(ymin = prob_low, ymax = prob_high),
          width = err_width
      )
      if (show_observed_points) {
        point_position <- prediction_observed_point_position(
          pred$.x,
          bar_span,
          jitter = observed_point_jitter
        )
        p <- p +
          geom_point(
            data = observed_plot_data,
            aes(x = .x, y = .obs_prob, text = .hover_text),
            position = point_position,
            shape = observed_point_shape,
            size = if (identical(observed_point_shape, 4)) 2.2 else 1.85,
            alpha = 0.72,
            color = prediction_observed_point_color(),
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      } else {
        p <- p +
          geom_point(
            data = pred_point_data,
            aes(x = .x, y = prob, color = I(.plot_color)),
            size = 2.2,
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      }
    } else {
      p <- ggplot(pred, aes(x = .x, y = prob)) +
        geom_col(width = col_width, alpha = 0.85, fill = single_color, color = "gray30") +
        geom_errorbar(
          aes(ymin = prob_low, ymax = prob_high),
          width = err_width,
          color = single_color
      )
      if (show_observed_points) {
        point_position <- prediction_observed_point_position(
          pred$.x,
          bar_span,
          jitter = observed_point_jitter
        )
        p <- p +
          geom_point(
            data = observed_plot_data,
            aes(x = .x, y = .obs_prob, text = .hover_text),
            position = point_position,
            shape = observed_point_shape,
            size = if (identical(observed_point_shape, 4)) 2.2 else 1.85,
            alpha = 0.72,
            color = prediction_observed_point_color(),
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      } else {
        p <- p +
          geom_point(
            data = pred,
            aes(x = .x, y = prob),
            size = 2.2,
            color = single_color,
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      }
    }
  } else if (identical(chart_type, "violin")) {
    violin_width <- prediction_bar_width(pred$.x)
    dodge <- ggplot2::position_dodge(width = violin_width)
    violin_data <- make_prediction_violin_data(pred)

    if (identical(color_spec$mode, "group")) {
      use_color_scale <- TRUE
      use_fill_scale <- TRUE
      fill_guide <- "none"
      label_position <- dodge
      p <- ggplot() +
        geom_violin(
          data = violin_data,
          aes(x = .x, y = .draw_prob, fill = .group, group = interaction(.x, .group)),
          position = dodge,
          width = violin_width * 0.9,
          alpha = 0.3,
          color = NA,
          trim = TRUE
        ) +
        geom_linerange(
          data = pred,
          aes(x = .x, ymin = prob_low, ymax = prob_high, color = .group, group = .group),
          position = dodge,
          linewidth = 0.35
        )
      if (show_observed_points) {
        point_position <- prediction_observed_point_position(
          pred$.x,
          violin_width,
          dodge_width = violin_width,
          jitter = observed_point_jitter
        )
        p <- p +
          geom_point(
            data = observed_plot_data,
            aes(x = .x, y = .obs_prob, group = .group, text = .hover_text),
            position = point_position,
            shape = observed_point_shape,
            size = if (identical(observed_point_shape, 4)) 2.2 else 1.85,
            alpha = 0.72,
            color = prediction_observed_point_color(),
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      } else {
        p <- p +
          geom_point(
            data = pred_point_data,
            aes(x = .x, y = prob, group = .group, color = I(.plot_color)),
            position = if (is.factor(pred$.x)) {
              ggplot2::position_jitterdodge(
                jitter.width = violin_width * 0.08,
                jitter.height = 0,
                dodge.width = violin_width,
                seed = 1
              )
            } else {
              dodge
            },
            size = 2.1,
            alpha = 0.95,
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      }

      if (!is.factor(pred$.x)) {
        p <- p +
          geom_line(
            data = pred,
            aes(x = .x, y = prob, color = .group, group = .group),
            linewidth = 0.55,
            alpha = 0.75
          )
      }
    } else if (identical(color_spec$mode, "x")) {
      use_color_scale <- TRUE
      use_fill_scale <- TRUE
      fill_guide <- "none"
      p <- ggplot() +
        geom_violin(
          data = violin_data,
          aes(x = .x, y = .draw_prob, fill = .x, group = .x),
          width = violin_width * 0.9,
          alpha = 0.3,
          color = NA,
          trim = TRUE
        ) +
        geom_linerange(
          data = pred,
          aes(x = .x, ymin = prob_low, ymax = prob_high, color = .x, group = .x),
          linewidth = 0.35
      )
      if (show_observed_points) {
        point_position <- prediction_observed_point_position(
          pred$.x,
          violin_width,
          jitter = observed_point_jitter
        )
        p <- p +
          geom_point(
            data = observed_plot_data,
            aes(x = .x, y = .obs_prob, text = .hover_text),
            position = point_position,
            shape = observed_point_shape,
            size = if (identical(observed_point_shape, 4)) 2.2 else 1.85,
            alpha = 0.72,
            color = prediction_observed_point_color(),
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      } else {
        p <- p +
          geom_point(
            data = pred_point_data,
            aes(x = .x, y = prob, color = I(.plot_color)),
            position = ggplot2::position_jitter(
              width = violin_width * 0.08,
              height = 0,
              seed = 1
            ),
            size = 2.1,
            alpha = 0.95,
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      }
    } else {
      p <- ggplot() +
        geom_violin(
          data = violin_data,
          aes(x = .x, y = .draw_prob, group = interaction(.x, .group)),
          position = dodge,
          width = violin_width * 0.9,
          alpha = 0.3,
          color = NA,
          fill = single_color,
          trim = TRUE
        ) +
        geom_linerange(
          data = pred,
          aes(x = .x, ymin = prob_low, ymax = prob_high, group = .group),
          position = dodge,
          linewidth = 0.35,
          color = single_color
        )
      if (show_observed_points) {
        point_position <- prediction_observed_point_position(
          pred$.x,
          violin_width,
          jitter = observed_point_jitter
        )
        p <- p +
          geom_point(
            data = observed_plot_data,
            aes(x = .x, y = .obs_prob, group = .group, text = .hover_text),
            position = point_position,
            shape = observed_point_shape,
            size = if (identical(observed_point_shape, 4)) 2.2 else 1.85,
            alpha = 0.72,
            color = prediction_observed_point_color(),
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      } else {
        p <- p +
          geom_point(
            data = pred,
            aes(x = .x, y = prob, group = .group),
            position = if (is.factor(pred$.x)) {
              ggplot2::position_jitter(
                width = violin_width * 0.08,
                height = 0,
                seed = 1
              )
            } else {
              ggplot2::position_identity()
            },
            size = 2.1,
            alpha = 0.95,
            color = single_color,
            show.legend = FALSE,
            inherit.aes = FALSE
          )
      }

      if (!is.factor(pred$.x)) {
        p <- p +
          geom_line(
            data = pred,
            aes(x = .x, y = prob, group = .group),
            linewidth = 0.55,
            alpha = 0.75,
            color = single_color
          )
      }
    }
  } else {
    if (identical(color_spec$mode, "group")) {
      use_color_scale <- TRUE
      p <- ggplot(pred, aes(x = .x, y = prob, color = .group, group = .group))
    } else {
      p <- ggplot(pred, aes(x = .x, y = prob, group = 1))
    }

    if (is.factor(pred$.x) && identical(color_spec$mode, "x")) {
      use_color_scale <- TRUE
      p <- p +
        geom_line(linewidth = 0.9, color = "gray50") +
        geom_point(aes(color = .x), size = 2.2) +
        geom_errorbar(aes(ymin = prob_low, ymax = prob_high, color = .x), width = 0.12)
    } else if (is.factor(pred$.x)) {
      if (identical(color_spec$mode, "single")) {
        p <- p +
          geom_line(linewidth = 1, color = single_color) +
          geom_point(size = 2, color = single_color) +
          geom_errorbar(aes(ymin = prob_low, ymax = prob_high), width = 0.12, color = single_color)
      } else {
        p <- p +
          geom_line(linewidth = 1) +
          geom_point(size = 2) +
          geom_errorbar(aes(ymin = prob_low, ymax = prob_high), width = 0.12)
      }
    } else if (identical(color_spec$mode, "single")) {
      p <- p +
        geom_line(linewidth = 1, color = single_color) +
        geom_ribbon(aes(ymin = prob_low, ymax = prob_high), alpha = 0.15, color = NA, fill = single_color) +
        geom_point(size = 1.5, color = single_color)
    } else {
      use_fill_scale <- TRUE
      fill_guide <- "none"
      p <- p +
        geom_line(linewidth = 1) +
        geom_ribbon(aes(ymin = prob_low, ymax = prob_high, fill = .group), alpha = 0.15, color = NA) +
        geom_point(size = 1.5)
    }
  }

  if (show_observed_points && show_labels) {
    observed_label_data <- make_prediction_observed_label_data(observed_plot_data)
    if (nrow(observed_label_data) > 0) {
      if (identical(chart_type, "bar") && identical(color_spec$mode, "group")) {
        point_label_position <- prediction_observed_point_position(
          pred$.x,
          bar_span,
          dodge_width = bar_span,
          jitter = observed_point_jitter
        )
      } else if (identical(chart_type, "violin") && identical(color_spec$mode, "group")) {
        point_label_position <- prediction_observed_point_position(
          pred$.x,
          violin_width,
          dodge_width = violin_width,
          jitter = observed_point_jitter
        )
      } else if (identical(chart_type, "bar") && is.factor(pred$.x)) {
        point_label_position <- prediction_observed_point_position(
          pred$.x,
          bar_span,
          jitter = observed_point_jitter
        )
      } else if (identical(chart_type, "violin") && is.factor(pred$.x)) {
        point_label_position <- prediction_observed_point_position(
          pred$.x,
          violin_width,
          jitter = observed_point_jitter
        )
      } else {
        point_label_position <- ggplot2::position_identity()
      }

      p <- add_prediction_observed_labels(
        p,
        observed_label_data,
        position = point_label_position,
        group_var = ".group",
        size = 2.5
      )
    }
  }

  if (show_labels) {
    if (is.null(label_position)) {
      p <- p +
        geom_text(
          data = make_prediction_label_data(pred, color_spec, chart_type = chart_type),
          aes(x = .x, y = .label_y, label = .label, group = .group, color = I(.plot_color)),
          size = 3.3,
          fontface = "bold",
          vjust = 0,
          check_overlap = TRUE,
          show.legend = FALSE,
          inherit.aes = FALSE
        )
    } else {
      p <- p +
        geom_text(
          data = make_prediction_label_data(pred, color_spec, chart_type = chart_type),
          aes(x = .x, y = .label_y, label = .label, group = .group, color = I(.plot_color)),
          position = label_position,
          size = 3.3,
          fontface = "bold",
          vjust = 0,
          check_overlap = TRUE,
          show.legend = FALSE,
          inherit.aes = FALSE
        )
    }
  }

  if (use_color_scale) {
    p <- p + scale_color_manual(values = color_spec$values, drop = FALSE, guide = color_guide)
  }
  if (use_fill_scale) {
    p <- p + scale_fill_manual(values = color_spec$values, drop = FALSE, guide = fill_guide)
  }

  label_args <- list(
    x = x_var,
    y = paste("Predicted probability of", response_info$success_label %||% "success"),
    title = "Model Predictions with 95% CI"
  )
  if (use_color_scale && !identical(color_guide, "none")) {
    label_args$color <- color_spec$label %||% group_label
  }
  if (use_fill_scale && !identical(fill_guide, "none")) {
    label_args$fill <- color_spec$label %||% group_label
  }

  p <- p +
    scale_y_continuous(
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0.02, if (show_labels) 0.08 else 0.03)),
      labels = function(x) paste0(round(x * 100, 1), "%")
    ) +
    do.call(labs, label_args) +
    theme_minimal(base_size = 12)

  if ((!use_color_scale && !use_fill_scale) || !isTRUE(color_spec$show_legend)) {
    p <- p + theme(legend.position = "none")
  }

  p
}

r_quote <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  paste0("\"", x, "\"")
}

r_char_vec <- function(x) {
  if (length(x) == 0) return("character(0)")
  paste0("c(", paste(vapply(x, r_quote, character(1)), collapse = ", "), ")")
}

r_named_char_vec <- function(x) {
  if (length(x) == 0) return("character(0)")
  parts <- sprintf(
    "%s = %s",
    vapply(names(x), r_quote, character(1)),
    vapply(unname(x), r_quote, character(1))
  )
  paste0("c(", paste(parts, collapse = ", "), ")")
}

sanitize_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", x %||% "")
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) "saved_model" else x
}

saved_model_dir <- function() {
  path <- file.path("results", "saved_models")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

prediction_level_color_store_path <- function() {
  file.path(saved_model_dir(), "prediction_level_colors.rds")
}

prediction_level_order_store_path <- function() {
  file.path(saved_model_dir(), "prediction_level_orders.rds")
}

prediction_axis_store_path <- function() {
  file.path(saved_model_dir(), "prediction_axes.rds")
}

app_settings_store_path <- function() {
  file.path(saved_model_dir(), "app_settings.rds")
}

load_prediction_level_color_store <- function() {
  path <- prediction_level_color_store_path()
  if (!file.exists(path)) return(list())
  tryCatch(
    normalize_prediction_level_color_store(readRDS(path)),
    error = function(e) list()
  )
}

save_prediction_level_color_store <- function(colors) {
  saveRDS(
    normalize_prediction_level_color_store(colors),
    prediction_level_color_store_path()
  )
  invisible(TRUE)
}

load_prediction_level_order_store <- function() {
  path <- prediction_level_order_store_path()
  if (!file.exists(path)) return(list())
  tryCatch(
    normalize_prediction_level_order_store(readRDS(path)),
    error = function(e) list()
  )
}

save_prediction_level_order_store <- function(orders) {
  saveRDS(
    normalize_prediction_level_order_store(orders),
    prediction_level_order_store_path()
  )
  invisible(TRUE)
}

normalize_prediction_axis_store <- function(axes = list()) {
  if (!is.list(axes)) axes <- list()
  pred_x <- as.character(axes$pred_x %||% "")
  pred_group <- as.character(axes$pred_group %||% "__none__")
  list(
    pred_x = if (length(pred_x) > 0 && nzchar(pred_x[[1]])) pred_x[[1]] else "",
    pred_group = if (length(pred_group) > 0 && nzchar(pred_group[[1]])) pred_group[[1]] else "__none__"
  )
}

load_prediction_axis_store <- function() {
  path <- prediction_axis_store_path()
  if (!file.exists(path)) return(normalize_prediction_axis_store(list()))
  tryCatch(
    normalize_prediction_axis_store(readRDS(path)),
    error = function(e) normalize_prediction_axis_store(list())
  )
}

save_prediction_axis_store <- function(axes) {
  saveRDS(normalize_prediction_axis_store(axes), prediction_axis_store_path())
  invisible(TRUE)
}

normalize_app_settings_store <- function(settings = list()) {
  if (!is.list(settings)) settings <- list()

  numeric_setting <- function(x, default) {
    out <- suppressWarnings(as.numeric(x %||% default))
    if (length(out) == 0 || is.na(out[[1]])) default else out[[1]]
  }

  dataset_source <- as.character(settings$dataset_source %||% "project")
  if (!dataset_source %in% c("project", "upload")) dataset_source <- "project"

  fit_backend <- as.character(settings$fit_backend %||% "glmer")
  if (!fit_backend %in% c("glmer", "fastglm", "multinom")) fit_backend <- "glmer"

  pred_chart_type <- as.character(settings$pred_chart_type %||% "line")
  if (!pred_chart_type %in% c("line", "bar", "violin")) pred_chart_type <- "line"

  fastglm_method <- as.character(settings$fastglm_method %||% "3")
  if (!fastglm_method %in% c("0", "1", "2", "3")) fastglm_method <- "3"

  multinom_method <- toupper(as.character(settings$multinom_method %||% "PQL"))
  if (!multinom_method %in% c("PQL", "MQL")) multinom_method <- "PQL"

  multinom_catcov <- as.character(settings$multinom_catcov %||% "single")
  if (!multinom_catcov %in% c("single", "diagonal", "free")) multinom_catcov <- "single"

  list(
    app_settings_version = 1L,
    dataset_source = dataset_source,
    project_file = as.character(settings$project_file %||% ""),
    response_var = as.character(settings$response_var %||% ""),
    response_ref = as.character(settings$response_ref %||% ""),
    fixed_effects = unique(as.character(settings$fixed_effects %||% character(0))),
    interaction_terms = unique(as.character(settings$interaction_terms %||% character(0))),
    random_effects = unique(as.character(settings$random_effects %||% character(0))),
    fit_backend = fit_backend,
    optimizer = as.character(settings$optimizer %||% "bobyqa"),
    maxfun = numeric_setting(settings$maxfun, 200000),
    fastglm_method = fastglm_method,
    fastglm_maxit = numeric_setting(settings$fastglm_maxit, 100),
    multinom_method = multinom_method,
    multinom_catcov = multinom_catcov,
    multinom_maxit = numeric_setting(settings$multinom_maxit, 25),
    reference_levels = as.list(settings$reference_levels %||% list()),
    compute_lrt = isTRUE(settings$compute_lrt),
    dataset_filters = normalize_dataset_filters(settings$dataset_filters %||% list()),
    pred_x = as.character(settings$pred_x %||% ""),
    pred_group = as.character(settings$pred_group %||% "__none__"),
    pred_chart_type = pred_chart_type,
    pred_show_labels = isTRUE(settings$pred_show_labels),
    pred_show_observed_points = isTRUE(settings$pred_show_observed_points),
    pred_observed_jitter = isTRUE(settings$pred_observed_jitter %||% TRUE),
    pred_observed_cross = isTRUE(settings$pred_observed_cross %||% TRUE),
    pred_interactive_hover = isTRUE(settings$pred_interactive_hover),
    pred_level_orders = normalize_prediction_level_order_store(settings$pred_level_orders %||% list()),
    pred_level_colors = normalize_prediction_level_color_store(settings$pred_level_colors %||% list())
  )
}

load_app_settings_store <- function() {
  path <- app_settings_store_path()
  if (!file.exists(path)) return(normalize_app_settings_store(list()))
  tryCatch(
    normalize_app_settings_store(readRDS(path)),
    error = function(e) normalize_app_settings_store(list())
  )
}

save_app_settings_store <- function(settings) {
  saveRDS(
    normalize_app_settings_store(settings),
    app_settings_store_path()
  )
  invisible(TRUE)
}

last_saved_model_path <- function() {
  file.path(saved_model_dir(), "last_fitted_model.rds")
}

snapshot_fit_meta <- function(fit_meta) {
  isolate(list(
    last_runtime_sec = fit_meta$last_runtime_sec %||% NA_real_,
    model_runtime_sec = fit_meta$model_runtime_sec %||% NA_real_,
    lrt_runtime_sec = fit_meta$lrt_runtime_sec %||% NA_real_,
    last_finished = fit_meta$last_finished %||% NULL
  ))
}

build_saved_fit_state <- function(
  res,
  fit_meta,
  pred_x,
  pred_group,
  pred_chart_type,
  pred_show_labels = FALSE,
  pred_show_observed_points = FALSE,
  pred_observed_jitter = TRUE,
  pred_observed_cross = TRUE,
  pred_interactive_hover = FALSE,
  pred_level_orders = list(),
  pred_level_colors = list(),
  dataset_filters = list(),
  include_raw_data = FALSE
) {
  saved_res <- res
  if (!isTRUE(include_raw_data) && !identical(saved_res$dataset_source, "upload")) {
    saved_res$raw_data <- NULL
  }

  fit_meta_state <- snapshot_fit_meta(fit_meta)

  list(
    app_state_version = 10L,
    saved_at = Sys.time(),
    fit_result = saved_res,
    fit_meta = list(
      last_runtime_sec = fit_meta_state$last_runtime_sec,
      model_runtime_sec = fit_meta_state$model_runtime_sec,
      lrt_runtime_sec = fit_meta_state$lrt_runtime_sec,
      last_finished = fit_meta_state$last_finished
    ),
    pred_x = pred_x %||% "",
    pred_group = pred_group %||% "__none__",
    pred_chart_type = pred_chart_type %||% "line",
    pred_show_labels = isTRUE(pred_show_labels),
    pred_show_observed_points = isTRUE(pred_show_observed_points),
    pred_observed_jitter = isTRUE(pred_observed_jitter %||% TRUE),
    pred_observed_cross = isTRUE(pred_observed_cross %||% TRUE),
    pred_interactive_hover = isTRUE(pred_interactive_hover),
    pred_level_orders = pred_level_orders %||% list(),
    pred_level_colors = pred_level_colors %||% list(),
    dataset_filters = normalize_dataset_filters(dataset_filters)
  )
}

validate_saved_fit_state <- function(state) {
  if (!is.list(state) || is.null(state$fit_result) || !is.list(state$fit_result)) {
    stop("The selected file is not a saved model produced by this app.")
  }
  required <- c("model", "data", "fixed_effects", "random_effects")
  missing_fields <- setdiff(required, names(state$fit_result))
  if (length(missing_fields) > 0) {
    stop(sprintf(
      "The selected file is missing required model fields: %s",
      paste(missing_fields, collapse = ", ")
    ))
  }
  invisible(TRUE)
}

saved_model_file_choices <- function() {
  files <- list.files(
    saved_model_dir(),
    pattern = "\\.rds$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) == 0) return(character(0))

  valid <- vapply(files, function(path) {
    tryCatch(
      {
        validate_saved_fit_state(readRDS(path))
        TRUE
      },
      error = function(e) FALSE
    )
  }, logical(1))
  files <- files[valid]
  if (length(files) == 0) return(character(0))

  info <- file.info(files)
  files <- files[order(info$mtime, decreasing = TRUE, na.last = TRUE)]
  labels <- basename(files)
  labels[labels == "last_fitted_model.rds"] <- "last_fitted_model.rds (automatic cache)"
  stats::setNames(files, labels)
}

load_saved_dataset <- function(res) {
  if (!is.null(res$raw_data) && is.data.frame(res$raw_data)) {
    return(res$raw_data)
  }
  if (!is.null(res$project_file) && nzchar(res$project_file) && file.exists(res$project_file)) {
    return(read_dataset(res$project_file))
  }
  NULL
}

function_definition_lines <- function(name) {
  lines <- deparse(get(name, mode = "function"), control = c("keepInteger", "niceNames"))
  lhs <- if (grepl("^[A-Za-z][A-Za-z0-9._]*$", name)) name else paste0("`", name, "`")
  lines[1] <- paste0(lhs, " <- ", lines[1])
  c(lines, "")
}

build_repro_code <- function(
  dataset_path,
  dataset_source,
  response_key,
  response_reference_level,
  dataset_filters,
  fixed_effects,
  interaction_terms,
  random_effects,
  fit_backend,
  reference_levels,
  optimizer,
  maxfun,
  compute_lrt,
  fastglm_method,
  fastglm_maxit,
  multinom_method,
  multinom_catcov,
  multinom_maxit,
  pred_x,
  pred_group,
  pred_chart_type,
  pred_show_labels = FALSE,
  pred_show_observed_points = FALSE,
  pred_level_orders = list(),
  pred_level_colors = list()
) {
  ref_lines <- "reference_levels <- list()"
  if (length(reference_levels) > 0) {
    ref_lines <- c(
      ref_lines,
      vapply(
        names(reference_levels),
        function(v) paste0("reference_levels[[", r_quote(v), "]] <- ", r_quote(reference_levels[[v]])),
        character(1)
      )
    )
  }

  data_note <- if (identical(dataset_source, "upload")) {
    "# Uploaded files use temporary Shiny paths; set a stable local path before running."
  } else {
    "# Dataset path from the project file selector."
  }

  if (is.null(pred_x) || !nzchar(pred_x)) {
    pred_x <- fixed_effects[1] %||% ""
  }
  if (is.null(pred_chart_type) || !(pred_chart_type %in% c("line", "bar", "violin"))) {
    pred_chart_type <- "line"
  }
  pred_group_line <- if (is.null(pred_group) || !nzchar(pred_group) || pred_group == "__none__") {
    "group_var <- NULL"
  } else {
    paste0("group_var <- ", r_quote(pred_group))
  }
  dataset_filters <- normalize_dataset_filters(dataset_filters)
  pred_level_orders <- pred_level_orders %||% list()
  pred_level_colors <- pred_level_colors %||% list()
  filter_lines <- "dataset_filters <- list()"
  if (length(dataset_filters) > 0) {
    filter_lines <- c(
      filter_lines,
      unlist(lapply(names(dataset_filters), function(v) {
        spec <- dataset_filters[[v]]
        c(
          paste0("dataset_filters[[", r_quote(v), "]] <- list("),
          paste0("  mode = ", r_quote(spec$mode), ","),
          paste0("  values = ", r_char_vec(spec$values), ","),
          paste0("  collapse_to_other = ", if (isTRUE(spec$collapse_to_other)) "TRUE" else "FALSE", ","),
          paste0("  search_pattern = ", r_quote(spec$search_pattern %||% ""), ","),
          paste0("  replacement = ", r_quote(spec$replacement %||% ""), ","),
          paste0("  ignore_case = ", if (isTRUE(spec$ignore_case)) "TRUE" else "FALSE"),
          ")"
        )
      }), use.names = FALSE)
    )
  }
  pred_level_color_lines <- "pred_level_colors <- list()"
  if (length(pred_level_colors) > 0) {
    pred_level_color_lines <- c(
      pred_level_color_lines,
      unlist(lapply(names(pred_level_colors), function(v) {
        colors_v <- pred_level_colors[[v]]
        if (length(colors_v) == 0) return(character(0))
        paste0("pred_level_colors[[", r_quote(v), "]] <- ", r_named_char_vec(colors_v))
      }), use.names = FALSE)
    )
  }
  pred_level_order_lines <- "pred_level_orders <- list()"
  if (length(pred_level_orders) > 0) {
    pred_level_order_lines <- c(
      pred_level_order_lines,
      unlist(lapply(names(pred_level_orders), function(v) {
        order_v <- pred_level_orders[[v]]
        if (length(order_v) == 0) return(character(0))
        paste0("pred_level_orders[[", r_quote(v), "]] <- ", r_char_vec(order_v))
      }), use.names = FALSE)
    )
  }

  helper_code <- unlist(
    lapply(
      c(
        "%||%",
        "detect_sep",
        "read_dataset",
        "encode_filter_tokens",
        "filter_can_collapse_to_other",
        "filter_replace_spec",
        "filter_regex_is_valid",
        "apply_filter_replacement",
        "normalize_dataset_filters",
        "apply_dataset_filters",
        "is_integer_like",
        "list_response_candidates",
        "get_response_spec",
        "response_level_choices",
        "resolve_response_reference",
        "resolve_binomial_outcomes",
        "normalize_response_info",
        "response_is_multinomial",
        "response_count_table",
        "response_count_lines",
        "fit_backend_choices",
        "coerce_fit_backend",
        "model_backend_label",
        "format_factor_term_suffix",
        "make_treatment_contrasts_with_labels",
        "is_mixed_model",
        "is_mblogit_model",
        "get_model_formula",
        "get_fixed_formula",
        "get_model_coefficients",
        "compute_fastglm_vcov",
        "get_model_vcov",
        "build_formula",
        "prepare_data",
        "fit_fastglm_model",
        "make_mblogit_random_spec",
        "fit_multinom_model",
        "compute_fixed_effects",
        "default_prediction_level_order",
        "normalize_prediction_level_orders",
        "resolve_prediction_level_order",
        "make_pred_values",
        "predict_mblogit_response",
        "compute_predictions",
        "prediction_bar_width",
        "clamp_probability",
        "make_prediction_violin_data",
        "default_prediction_level_palette",
        "is_valid_hex_color",
        "normalize_prediction_level_colors",
        "resolve_prediction_palette",
        "prediction_plot_color_spec",
        "format_prediction_percent",
        "prediction_observed_point_shape",
        "prediction_observed_point_color",
        "prediction_observed_point_position",
        "prediction_plot_row_colors",
        "make_prediction_label_data",
        "make_prediction_observed_data",
        "make_prediction_observed_label_data",
        "add_prediction_observed_labels",
        "make_prediction_observed_hover_text",
        "build_multinomial_prediction_plot",
        "build_prediction_plot"
      ),
      function_definition_lines
    ),
    use.names = FALSE
  )

  package_lines <- c(
    "#!/usr/bin/env Rscript",
    "",
    "suppressPackageStartupMessages({",
    "  library(lme4)",
    if (identical(fit_backend, "fastglm")) "  library(fastglm)",
    if (identical(fit_backend, "multinom")) "  library(mclogit)",
    "  library(ggplot2)",
    "  library(readxl)",
    "})",
    ""
  )

  fit_lines <- if (identical(fit_backend, "fastglm")) {
    c(
      paste0("fit_backend <- ", r_quote(fit_backend)),
      paste0("fastglm_method <- ", as.integer(fastglm_method %||% 3L)),
      paste0("fastglm_maxit <- ", as.integer(fastglm_maxit %||% 100L)),
      "multinom_method <- NULL",
      "multinom_catcov <- NULL",
      "multinom_maxit <- NULL",
      "if (length(random_effects) > 0) stop(\"fastglm fits in this app require zero random intercepts.\")",
      "model <- fit_fastglm_model(frm, model_df, method = fastglm_method, maxit = fastglm_maxit)"
    )
  } else if (identical(fit_backend, "multinom")) {
    c(
      paste0("fit_backend <- ", r_quote(fit_backend)),
      "fastglm_method <- NULL",
      "fastglm_maxit <- NULL",
      paste0("multinom_method <- ", r_quote(multinom_method %||% "PQL")),
      paste0("multinom_catcov <- ", r_quote(multinom_catcov %||% "single")),
      paste0("multinom_maxit <- ", as.integer(multinom_maxit %||% 25L)),
      "model <- fit_multinom_model(frm, model_df, random_effects = random_effects, method = multinom_method, catCov = multinom_catcov, maxit = multinom_maxit)"
    )
  } else {
    c(
      paste0("fit_backend <- ", r_quote(fit_backend)),
      "fastglm_method <- NULL",
      "fastglm_maxit <- NULL",
      "multinom_method <- NULL",
      "multinom_catcov <- NULL",
      "multinom_maxit <- NULL",
      paste0("optimizer <- ", r_quote(optimizer)),
      paste0("maxfun <- ", as.integer(maxfun)),
      "if (length(random_effects) == 0) stop(\"Select at least one random intercept to use lme4::glmer.\")",
      "model <- glmer(",
      "  frm,",
      "  data = model_df,",
      "  family = binomial(link = \"logit\"),",
      "  control = glmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))",
      ")"
    )
  }

  code_lines <- c(
    package_lines,
    helper_code,
    paste0("data_path <- ", r_quote(dataset_path)),
    data_note,
    "df_raw <- read_dataset(data_path)",
    filter_lines,
    "df_filtered <- apply_dataset_filters(df_raw, dataset_filters)",
    "",
    paste0("response_key <- ", r_quote(response_key %||% "")),
    paste0("response_reference_level <- ", if (is.null(response_reference_level) || !nzchar(response_reference_level)) "NULL" else r_quote(response_reference_level)),
    paste0("fixed_effects <- ", r_char_vec(fixed_effects)),
    paste0("interaction_terms <- ", r_char_vec(interaction_terms)),
    paste0("random_effects <- ", r_char_vec(random_effects)),
    ref_lines,
    paste0("compute_lrt <- ", if (isTRUE(compute_lrt)) "TRUE" else "FALSE"),
    pred_level_order_lines,
    pred_level_color_lines,
    "",
    "opts <- list(",
    "  response_key = response_key,",
    "  response_reference_level = response_reference_level,",
    "  fixed_effects = fixed_effects,",
    "  interaction_terms = interaction_terms,",
    "  random_effects = random_effects,",
    "  reference_levels = reference_levels",
    ")",
    "model_df <- prepare_data(df_filtered, opts)",
    "response_info <- attr(model_df, \"response_info\")",
    "frm <- build_formula(fixed_effects, interaction_terms, random_effects, response_info = response_info)",
    "",
    fit_lines,
    "",
    "# Tests",
    "cat(\"Backend:\", model_backend_label(fit_backend, fastglm_method %||% NULL, multinom_method %||% NULL, multinom_catcov %||% NULL), \"\\n\")",
    "print(summary(model))",
    "if (isTRUE(compute_lrt) && identical(fit_backend, \"glmer\")) {",
    "  lrt_raw <- drop1(model, test = \"Chisq\")",
    "  lrt_tbl <- data.frame(term = rownames(lrt_raw), lrt_raw, row.names = NULL, check.names = FALSE)",
    "  print(lrt_tbl)",
    "} else if (isTRUE(compute_lrt) && identical(fit_backend, \"multinom\")) {",
    "  lrt_tbl <- data.frame(message = \"Likelihood-ratio tests are unavailable for mclogit::mblogit fits in this app.\")",
    "  print(lrt_tbl)",
    "} else if (isTRUE(compute_lrt)) {",
    "  lrt_tbl <- data.frame(message = \"Likelihood-ratio tests are unavailable for fastglm fits in this app.\")",
    "  print(lrt_tbl)",
    "} else {",
    "  lrt_tbl <- data.frame(message = \"Likelihood-ratio tests skipped. Set compute_lrt <- TRUE and use glmer to run drop1().\")",
    "  print(lrt_tbl)",
    "}",
    "",
    "# Table of fixed effects",
    "fixed_tbl <- compute_fixed_effects(model)",
    "print(fixed_tbl)",
    "",
    "# Chart 1: odds ratios for fixed effects",
    "coef_plot_data <- subset(fixed_tbl, term != \"(Intercept)\")",
    "coef_plot <- ggplot(coef_plot_data, aes(y = reorder(term, odds_ratio), x = odds_ratio)) +",
    "  geom_vline(xintercept = 1, linetype = \"dashed\", color = \"gray40\") +",
    "  geom_errorbarh(aes(xmin = or_ci_low, xmax = or_ci_high), height = 0.2, color = \"#4C78A8\") +",
    "  geom_point(size = 2.3, color = \"#F58518\") +",
    "  scale_x_log10() +",
    "  labs(x = \"Odds ratio (log scale) with 95% Wald CI\", y = \"Term\", title = if (\"outcome\" %in% names(coef_plot_data)) \"Fixed Effects Odds Ratios by Outcome\" else \"Fixed Effects Odds Ratios\") +",
    "  theme_minimal(base_size = 12)",
    "if (\"outcome\" %in% names(coef_plot_data)) coef_plot <- coef_plot + facet_wrap(~ outcome, scales = \"free_y\")",
    "print(coef_plot)",
    "",
    "# Chart 2: predicted probabilities",
    paste0("x_var <- ", r_quote(pred_x)),
    pred_group_line,
    paste0("pred_chart_type <- ", r_quote(pred_chart_type)),
    paste0("pred_show_labels <- ", if (isTRUE(pred_show_labels)) "TRUE" else "FALSE"),
    paste0("pred_show_observed_points <- ", if (isTRUE(pred_show_observed_points)) "TRUE" else "FALSE"),
    "pred_tbl <- compute_predictions(model, model_df, x_var = x_var, group_var = group_var, level_orders = pred_level_orders, include_multinomial_uncertainty = response_is_multinomial(response_info))",
    "if (is.null(pred_tbl)) stop(\"Predictions unavailable for the selected x/group variables.\")",
    "",
    "pred_plot <- build_prediction_plot(pred_tbl, response_info = response_info, chart_type = pred_chart_type, level_colors = pred_level_colors, show_labels = pred_show_labels, observed_data = attr(model_df, \"observed_prediction_data\"), show_observed_points = pred_show_observed_points)",
    "print(pred_plot)",
    "",
    "# Optional exports",
    "write.csv(fixed_tbl, \"glmm_fixed_effects.csv\", row.names = FALSE)",
    "write.csv(lrt_tbl, \"glmm_lrt.csv\", row.names = FALSE)",
    "write.csv(pred_tbl, \"glmm_predictions.csv\", row.names = FALSE)"
  )
  paste(code_lines, collapse = "\n")
}

project_files <- sort(list.files(".", pattern = "\\.(csv|xlsx|xls)$", ignore.case = TRUE))
project_file_choices <- c("Select a dataset" = "", project_files)
project_saved_model_choices <- c("Select a saved model" = "", saved_model_file_choices())
default_file <- ""
fastglm_method_choices <- c("LLT Cholesky" = "2", "LDLT Cholesky" = "3")
multinom_method_choices <- c(
  "PQL (recommended)" = "PQL",
  "MQL (faster, rougher)" = "MQL"
)
multinom_catcov_choices <- c(
  "single (fastest)" = "single",
  "diagonal" = "diagonal",
  "free (slowest)" = "free"
)

combined_visualisations_app <- new.env(parent = globalenv())
sys.source(
  file.path("combined_visualisations", "app.R"),
  envir = combined_visualisations_app
)
combined_visualisations_ui <- combined_visualisations_app$ui
combined_visualisations_server <- combined_visualisations_app$server

explorer_ui <- fluidPage(
  tags$head(
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('copyReproCode', function(_) {
         var el = document.getElementById('repro_code_text');
         if (!el) return;
         var text = el.value || '';
         if (navigator.clipboard && navigator.clipboard.writeText) {
           navigator.clipboard.writeText(text).catch(function() {
             el.focus();
             el.select();
             try { document.execCommand('copy'); } catch (err) {}
           });
         } else {
           el.focus();
           el.select();
           try { document.execCommand('copy'); } catch (err) {}
         }
       });

       document.addEventListener('click', function(event) {
         var button = event.target.closest('.prediction-color-commit');
         if (!button) return;
         var colorInput = button.parentElement.querySelector('input[type=\"color\"]');
         if (!colorInput || !window.Shiny || !Shiny.setInputValue) return;
         Shiny.setInputValue('prediction_color_commit', {
           variable: button.getAttribute('data-variable'),
           level: button.getAttribute('data-level'),
           color: colorInput.value,
           nonce: Date.now()
         }, {priority: 'event'});
       });

       var predictionLevelDragRow = null;
       document.addEventListener('dragstart', function(event) {
         var handle = event.target.closest('.pred-level-handle');
         if (!handle) return;
         predictionLevelDragRow = handle.closest('.pred-level-row');
         if (!predictionLevelDragRow) return;
         predictionLevelDragRow.classList.add('pred-level-dragging');
         event.dataTransfer.effectAllowed = 'move';
         event.dataTransfer.setData('text/plain', predictionLevelDragRow.getAttribute('data-level') || '');
       });

       document.addEventListener('dragover', function(event) {
         if (!predictionLevelDragRow) return;
         var row = event.target.closest('.pred-level-row');
         if (!row || row === predictionLevelDragRow || row.parentElement !== predictionLevelDragRow.parentElement) return;
         event.preventDefault();
         var rect = row.getBoundingClientRect();
         row.parentElement.insertBefore(predictionLevelDragRow, event.clientY < rect.top + rect.height / 2 ? row : row.nextSibling);
       });

       document.addEventListener('drop', function(event) {
         if (!predictionLevelDragRow) return;
         event.preventDefault();
         var container = predictionLevelDragRow.parentElement;
         var order = Array.from(container.querySelectorAll(':scope > .pred-level-row')).map(function(row) {
           return row.getAttribute('data-level');
         });
         if (window.Shiny && Shiny.setInputValue) {
           Shiny.setInputValue(container.getAttribute('data-order-input'), order, {priority: 'event'});
         }
       });

       document.addEventListener('dragend', function() {
         if (predictionLevelDragRow) predictionLevelDragRow.classList.remove('pred-level-dragging');
         predictionLevelDragRow = null;
       });"
    ))
  ),
  titlePanel("GLMM / GLM Explorer: Custom Effects and Interactions"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "dataset_source",
        "Dataset source",
        choices = c("Project file" = "project", "Upload file" = "upload"),
        selected = "project"
      ),
      conditionalPanel(
        "input.dataset_source == 'project'",
        selectInput("project_file", "Project dataset", choices = project_file_choices, selected = default_file)
      ),
      conditionalPanel(
        "input.dataset_source == 'upload'",
        fileInput("upload_file", "Upload CSV/XLSX", accept = c(".csv", ".xlsx", ".xls"))
      ),
      tags$hr(),
      selectInput(
        "response_var",
        "Dependent variable",
        choices = c("Select a dependent variable" = ""),
        selected = ""
      ),
      selectizeInput("fixed_effects", "Fixed effects", choices = NULL, multiple = TRUE),
      selectizeInput("interaction_terms", "Interactions (pairwise among selected fixed effects)", choices = NULL, multiple = TRUE),
      selectInput(
        "fit_backend",
        "Fitting backend",
        choices = c(
          "lme4::glmer (mixed model)" = "glmer",
          "fastglm::fastglm (fixed-effects GLM)" = "fastglm",
          "mclogit::mblogit (multinomial; random intercepts supported)" = "multinom"
        ),
        selected = "glmer"
      ),
      conditionalPanel(
        "input.fit_backend != 'fastglm'",
        selectizeInput("random_effects", "Random intercepts (grouping variables)", choices = NULL, multiple = TRUE)
      ),
      conditionalPanel(
        "input.fit_backend == 'glmer'",
        selectInput("optimizer", "Optimizer", choices = c("bobyqa", "nloptwrap", "Nelder_Mead"), selected = "bobyqa"),
        numericInput("maxfun", "Optimizer maxfun", value = 200000, min = 10000, step = 10000)
      ),
      conditionalPanel(
        "input.fit_backend == 'fastglm'",
        tags$small("`fastglm` fits a binomial GLM only, so random intercepts are disabled."),
        selectInput("fastglm_method", "fastglm decomposition", choices = fastglm_method_choices, selected = "3"),
        numericInput("fastglm_maxit", "fastglm maxit", value = 100, min = 10, step = 10)
      ),
      conditionalPanel(
        "input.fit_backend == 'multinom'",
        tags$small("`mclogit::mblogit` supports multinomial random intercepts in this app. For large models, start with `catCov = single`; `drop1()` likelihood-ratio tests remain disabled."),
        selectInput("multinom_method", "mclogit approximation", choices = multinom_method_choices, selected = "PQL"),
        selectInput("multinom_catcov", "Random-effect covariance across logits", choices = multinom_catcov_choices, selected = "single"),
        numericInput("multinom_maxit", "mclogit maxit", value = 25, min = 5, step = 5)
      ),
      uiOutput("reference_levels_ui"),
      checkboxInput("compute_lrt", "Compute likelihood-ratio tests (`drop1`; slower)", value = FALSE),
      actionButton("fit_model", "Fit / Refit model"),
      tags$br(),
      tags$br(),
      verbatimTextOutput("fit_indicator"),
      tags$hr(),
      downloadButton("save_model", "Save fitted model"),
      tags$br(),
      tags$br(),
      radioButtons(
        "model_load_source",
        "Load saved model from",
        choices = c("Server saved model" = "server", "Upload model" = "upload"),
        selected = "server"
      ),
      conditionalPanel(
        "input.model_load_source == 'server'",
        selectInput(
          "project_saved_model",
          "Server saved model",
          choices = project_saved_model_choices,
          selected = ""
        ),
        actionButton("refresh_saved_models", "Refresh saved models")
      ),
      conditionalPanel(
        "input.model_load_source == 'upload'",
        fileInput("load_model", "Upload saved model", accept = c(".rds"))
      ),
      tags$small("Successful fits and app settings are cached automatically in results/saved_models.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Data",
          verbatimTextOutput("data_info"),
          uiOutput("dataset_filter_ui"),
          plotOutput("class_balance_plot", height = 280),
          DTOutput("preview_table")
        ),
        tabPanel(
          "Model",
          verbatimTextOutput("model_formula"),
          verbatimTextOutput("model_status"),
          verbatimTextOutput("model_summary")
        ),
        tabPanel(
          "Fixed Effects",
          downloadButton("download_coef_plot_svg", "Download fixed-effects plot (.svg)"),
          tags$br(),
          tags$br(),
          DTOutput("fixed_effects_table"),
          plotOutput("coef_plot", height = 360)
        ),
        tabPanel(
          "Interpretation",
          tags$p("Draft wording based on the fitted fixed effects. Edit for house style and methodological framing before using it in a paper."),
          uiOutput("interpretation_ui")
        ),
        tabPanel(
          "Predictions",
          uiOutput("pred_controls_ui"),
          downloadButton("download_pred_plot", "Download prediction plot (.png)"),
          downloadButton("download_pred_plot_svg", "Download prediction plot (.svg)"),
          tags$br(),
          tags$br(),
          uiOutput("pred_plot_ui"),
          tags$hr(),
          uiOutput("individual_pred_plots_ui"),
          tags$hr(),
          DTOutput("pred_table")
        ),
        tabPanel(
          "Reproducible Code",
          tags$p("Copy or download the R script below to rerun the same tests and charts outside the app."),
          actionButton("copy_repro_code", "Copy code"),
          downloadButton("download_repro_code", "Download .R"),
          tags$br(),
          tags$br(),
          uiOutput("repro_code_ui")
        )
      )
    )
  )
)

explorer_server <- function(input, output, session) {
  server_saved_model_choices <- reactiveVal(saved_model_file_choices())
  app_settings_initial <- load_app_settings_store()
  prediction_axes_initial <- normalize_prediction_axis_store(
    if (file.exists(prediction_axis_store_path())) {
      load_prediction_axis_store()
    } else {
      list(
        pred_x = app_settings_initial$pred_x %||% "",
        pred_group = app_settings_initial$pred_group %||% "__none__"
      )
    }
  )
  prediction_level_orders_initial <- merge_prediction_level_order_store(
    app_settings_initial$pred_level_orders %||% list(),
    load_prediction_level_order_store()
  )
  prediction_level_colors_initial <- merge_prediction_level_color_store(
    app_settings_initial$pred_level_colors %||% list(),
    load_prediction_level_color_store()
  )
  fit_meta <- reactiveValues(
    is_fitting = FALSE,
    last_runtime_sec = NA_real_,
    model_runtime_sec = NA_real_,
    lrt_runtime_sec = NA_real_,
    last_finished = NULL
  )
  fit_result <- reactiveVal(NULL)
  dataset_override <- reactiveVal(NULL)
  dataset_filters_state <- reactiveVal(app_settings_initial$dataset_filters %||% list())
  filter_target_column <- reactiveVal(NULL)
  prediction_level_order_store_state <- reactiveVal(prediction_level_orders_initial)
  prediction_level_orders_state <- reactiveVal(prediction_level_orders_initial)
  prediction_level_color_store_state <- reactiveVal(prediction_level_colors_initial)
  prediction_level_colors_state <- reactiveVal(prediction_level_colors_initial)
  prediction_axis_selection_state <- reactiveVal(prediction_axes_initial)
  app_settings_state <- reactiveVal(app_settings_initial)
  state_meta <- reactiveValues(
    is_restoring = FALSE,
    is_applying_settings = FALSE,
    suppress_settings_save = TRUE,
    loaded_display_path = NULL,
    suppress_dataset_source = FALSE,
    suppress_project_file = FALSE,
    suppress_upload_file = FALSE,
    suppress_active_data = FALSE,
    suppress_response_var = FALSE,
    suppress_fixed_effects = FALSE,
    suppress_fit_backend = FALSE,
    clear_model_variables_on_active_data = TRUE
  )
  auto_fit_pending <- reactiveVal(FALSE)
  queued_fit_request <- reactiveVal(NULL)

  current_reference_levels <- function(fixed_effects = input$fixed_effects %||% character(0)) {
    refs <- list()
    for (v in fixed_effects) {
      ref_val <- input[[paste0("ref_", v)]]
      if (!is.null(ref_val) && nzchar(ref_val)) refs[[v]] <- ref_val
    }
    refs
  }

  nonempty_or <- function(x, y) {
    if (!is.null(x) && length(x) > 0 && any(nzchar(as.character(x)))) x else y
  }

  collect_app_settings <- function() {
    saved_reference_levels <- as.list((app_settings_state() %||% list())$reference_levels %||% list())
    reference_levels <- utils::modifyList(saved_reference_levels, current_reference_levels(), keep.null = TRUE)
    normalize_app_settings_store(list(
      dataset_source = input$dataset_source %||% "project",
      project_file = input$project_file %||% "",
      response_var = input$response_var %||% "",
      response_ref = input$response_ref %||% "",
      fixed_effects = input$fixed_effects %||% character(0),
      interaction_terms = input$interaction_terms %||% character(0),
      random_effects = input$random_effects %||% character(0),
      fit_backend = input$fit_backend %||% "glmer",
      optimizer = input$optimizer %||% "bobyqa",
      maxfun = input$maxfun %||% 200000,
      fastglm_method = input$fastglm_method %||% "3",
      fastglm_maxit = input$fastglm_maxit %||% 100,
      multinom_method = input$multinom_method %||% "PQL",
      multinom_catcov = input$multinom_catcov %||% "single",
      multinom_maxit = input$multinom_maxit %||% 25,
      reference_levels = reference_levels,
      compute_lrt = isTRUE(input$compute_lrt),
      dataset_filters = dataset_filters_state(),
      pred_x = prediction_axis_selection_state()$pred_x,
      pred_group = prediction_axis_selection_state()$pred_group,
      pred_chart_type = input$pred_chart_type %||% "line",
      pred_show_labels = isTRUE(input$pred_show_labels),
      pred_show_observed_points = isTRUE(input$pred_show_observed_points),
      pred_observed_jitter = isTRUE(input$pred_observed_jitter %||% TRUE),
      pred_observed_cross = isTRUE(input$pred_observed_cross %||% TRUE),
      pred_interactive_hover = isTRUE(input$pred_interactive_hover),
      pred_level_orders = prediction_level_orders_state(),
      pred_level_colors = prediction_level_colors_state()
    ))
  }

  persist_app_settings <- function(settings = collect_app_settings()) {
    if (isTRUE(state_meta$suppress_settings_save)) return(invisible(FALSE))
    settings <- normalize_app_settings_store(settings)
    if (identical(settings, app_settings_state())) return(invisible(TRUE))
    app_settings_state(settings)
    tryCatch(
      save_app_settings_store(settings),
      error = function(e) {
        showNotification(
          paste("Saving app settings failed:", e$message),
          type = "warning",
          duration = 8
        )
        invisible(FALSE)
      }
    )
  }

  merge_app_settings <- function(update = list()) {
    persist_app_settings(utils::modifyList(app_settings_state(), update, keep.null = TRUE))
  }

  reset_fit_state <- function() {
    fit_meta$is_fitting <- FALSE
    fit_meta$last_runtime_sec <- NA_real_
    fit_meta$model_runtime_sec <- NA_real_
    fit_meta$lrt_runtime_sec <- NA_real_
    fit_meta$last_finished <- NULL
    fit_result(NULL)
  }

  persist_fit_state <- function(
    res = fit_result(),
    pred_x = input$pred_x %||% "",
    pred_group = input$pred_group %||% "__none__",
    pred_chart_type = input$pred_chart_type %||% "line",
    pred_show_labels = input$pred_show_labels %||% FALSE,
    pred_show_observed_points = input$pred_show_observed_points %||% FALSE,
    pred_observed_jitter = input$pred_observed_jitter %||% TRUE,
    pred_observed_cross = input$pred_observed_cross %||% TRUE,
    pred_interactive_hover = input$pred_interactive_hover %||% FALSE,
    pred_level_orders = prediction_level_orders_state(),
    pred_level_colors = prediction_level_colors_state(),
    dataset_filters = dataset_filters_state(),
    include_raw_data = FALSE
  ) {
    if (is.null(res) || !is.null(res$error)) return(invisible(FALSE))

    tryCatch(
      saveRDS(
        build_saved_fit_state(
          res,
          fit_meta,
          pred_x = pred_x,
          pred_group = pred_group,
          pred_chart_type = pred_chart_type,
          pred_show_labels = pred_show_labels,
          pred_show_observed_points = pred_show_observed_points,
          pred_observed_jitter = pred_observed_jitter,
          pred_observed_cross = pred_observed_cross,
          pred_interactive_hover = pred_interactive_hover,
          pred_level_orders = pred_level_orders,
          pred_level_colors = pred_level_colors,
          dataset_filters = dataset_filters,
          include_raw_data = include_raw_data
        ),
        last_saved_model_path()
      ),
      error = function(e) {
        showNotification(
          paste("Automatic model caching failed:", e$message),
          type = "warning",
          duration = 8
        )
        invisible(FALSE)
      }
    )
  }

  persist_prediction_level_order_store <- function(orders = prediction_level_orders_state()) {
    merged_orders <- merge_prediction_level_order_store(
      prediction_level_order_store_state(),
      orders %||% list()
    )
    if (identical(merged_orders, prediction_level_order_store_state())) {
      return(invisible(TRUE))
    }

    tryCatch(
      {
        save_prediction_level_order_store(merged_orders)
        prediction_level_order_store_state(merged_orders)
        invisible(TRUE)
      },
      error = function(e) {
        showNotification(
          paste("Saving prediction level order failed:", e$message),
          type = "warning",
          duration = 8
        )
        invisible(FALSE)
      }
    )
  }

  persist_prediction_level_color_store <- function(colors = prediction_level_colors_state()) {
    merged_colors <- merge_prediction_level_color_store(
      prediction_level_color_store_state(),
      colors %||% list()
    )
    if (identical(merged_colors, prediction_level_color_store_state())) {
      return(invisible(TRUE))
    }

    tryCatch(
      {
        save_prediction_level_color_store(merged_colors)
        prediction_level_color_store_state(merged_colors)
        invisible(TRUE)
      },
      error = function(e) {
        showNotification(
          paste("Saving prediction colors failed:", e$message),
          type = "warning",
          duration = 8
        )
        invisible(FALSE)
      }
    )
  }

  normalized_prediction_level_orders <- function(res = fit_result(), orders = prediction_level_orders_state()) {
    if (is.null(res) || !is.list(res) || is.null(res$data) || is.null(res$fixed_effects)) {
      return(list())
    }
    normalize_prediction_level_orders(
      prediction_level_specs_from_model_df(res$data, res$fixed_effects),
      merge_prediction_level_order_store(
        prediction_level_order_store_state(),
        orders %||% list()
      )
    )
  }

  normalized_prediction_level_colors <- function(res = fit_result(), colors = prediction_level_colors_state()) {
    if (is.null(res) || !is.list(res) || is.null(res$data) || is.null(res$fixed_effects)) {
      return(list())
    }
    normalize_prediction_level_colors(
      prediction_level_specs_from_model_df(res$data, res$fixed_effects),
      merge_prediction_level_color_store(
        prediction_level_color_store_state(),
        colors %||% list()
      )
    )
  }

  sync_variable_inputs <- function(
    df,
    response_selected = NULL,
    fixed_selected = NULL,
    random_selected = NULL,
    interaction_selected = NULL,
    fit_backend_selected = NULL
  ) {
    response_specs <- list_response_candidates(df)
    response_choices <- c(
      "Select a dependent variable" = "",
      stats::setNames(
        vapply(response_specs, `[[`, character(1), "key"),
        vapply(response_specs, `[[`, character(1), "label")
      )
    )
    selected_response <- if (!is.null(response_selected) && nzchar(response_selected)) {
      response_selected
    } else {
      ""
    }
    response_spec <- if (nzchar(selected_response)) {
      tryCatch(get_response_spec(df, selected_response), error = function(e) NULL)
    } else {
      NULL
    }
    if (!is.null(response_spec)) selected_response <- response_spec$key
    excluded_response_cols <- if (!is.null(response_spec)) response_spec$outcome_cols else character(0)
    selected_backend <- coerce_fit_backend(fit_backend_selected %||% (input$fit_backend %||% "glmer"), response_spec)
    backend_choices <- fit_backend_choices(response_spec)

    candidate_vars <- setdiff(
      names(df),
      c(excluded_response_cols, "plural_successes", "plural_failures", "response_value", ".case_weight")
    )

    default_random <- if (identical(selected_backend, "fastglm")) {
      character(0)
    } else {
      intersect("lemma", candidate_vars)
    }

    fixed_selected <- fixed_selected %||% character(0)
    fixed_selected <- intersect(fixed_selected, candidate_vars)

    random_selected <- random_selected %||% default_random
    random_selected <- intersect(random_selected, candidate_vars)
    if (identical(selected_backend, "fastglm")) random_selected <- character(0)

    interaction_choices <- make_interaction_choices(fixed_selected)
    interaction_selected <- intersect(interaction_selected %||% character(0), interaction_choices)

    current_response <- input$response_var %||% ""
    if (!identical(current_response, selected_response)) {
      state_meta$suppress_response_var <- TRUE
    }
    updateSelectInput(
      session,
      "response_var",
      choices = response_choices,
      selected = selected_response
    )
    current_fixed <- input$fixed_effects %||% character(0)
    if (!identical(current_fixed, fixed_selected)) {
      state_meta$suppress_fixed_effects <- TRUE
    }
    current_backend <- input$fit_backend %||% "glmer"
    if (!identical(current_backend, selected_backend)) {
      state_meta$suppress_fit_backend <- TRUE
    }
    updateSelectInput(
      session,
      "fit_backend",
      choices = backend_choices,
      selected = selected_backend
    )

    updateSelectizeInput(
      session,
      "fixed_effects",
      choices = candidate_vars,
      selected = fixed_selected,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "interaction_terms",
      choices = interaction_choices,
      selected = interaction_selected,
      server = TRUE
    )
    updateSelectizeInput(
      session,
      "random_effects",
      choices = candidate_vars,
      selected = random_selected,
      server = TRUE
    )
  }

  apply_app_settings <- function(settings = NULL) {
    if (is.null(settings)) settings <- app_settings_initial
    settings <- normalize_app_settings_store(settings)
    state_meta$is_applying_settings <- TRUE
    state_meta$suppress_settings_save <- TRUE
    on.exit({
      state_meta$is_applying_settings <- FALSE
      state_meta$suppress_settings_save <- FALSE
    }, add = TRUE)

    if (identical(settings$dataset_source, "project")) {
      state_meta$suppress_dataset_source <- TRUE
      updateRadioButtons(session, "dataset_source", selected = "project")
    }

    updateSelectInput(session, "optimizer", selected = settings$optimizer %||% "bobyqa")
    updateNumericInput(session, "maxfun", value = settings$maxfun %||% 200000)
    updateSelectInput(session, "fastglm_method", selected = as.character(settings$fastglm_method %||% "3"))
    updateNumericInput(session, "fastglm_maxit", value = settings$fastglm_maxit %||% 100)
    updateSelectInput(session, "multinom_method", selected = as.character(settings$multinom_method %||% "PQL"))
    updateSelectInput(session, "multinom_catcov", selected = as.character(settings$multinom_catcov %||% "single"))
    updateNumericInput(session, "multinom_maxit", value = settings$multinom_maxit %||% 25)
    updateCheckboxInput(session, "compute_lrt", value = isTRUE(settings$compute_lrt))
    updateCheckboxInput(session, "pred_observed_jitter", value = isTRUE(settings$pred_observed_jitter %||% TRUE))
    updateCheckboxInput(session, "pred_observed_cross", value = isTRUE(settings$pred_observed_cross %||% TRUE))
    updateCheckboxInput(session, "pred_interactive_hover", value = isTRUE(settings$pred_interactive_hover))
  }

  session$onFlushed(function() {
    apply_app_settings(app_settings_initial)
  }, once = TRUE)

  restore_saved_fit <- function(state, source_label = "saved model", auto_restored = FALSE) {
    validate_saved_fit_state(state)
    res <- state$fit_result
    dataset_df <- load_saved_dataset(res)
    filtered_dataset_df <- if (!is.null(dataset_df)) {
      apply_dataset_filters(dataset_df, state$dataset_filters %||% list())
    } else {
      NULL
    }
    res$data <- restore_observed_prediction_data(
      res$data,
      raw_df = filtered_dataset_df %||% dataset_df,
      res = res,
      dataset_filters = state$dataset_filters %||% list()
    )
    use_project_file <- (
      identical(res$dataset_source, "project") &&
      !is.null(res$project_file) &&
      nzchar(res$project_file) &&
      res$project_file %in% project_files &&
      is.null(res$raw_data)
    )

    state_meta$is_restoring <- TRUE
    state_meta$clear_model_variables_on_active_data <- FALSE
    on.exit({
      state_meta$is_restoring <- FALSE
    }, add = TRUE)
    auto_fit_pending(FALSE)
    queued_fit_request(NULL)

    if (use_project_file) {
      dataset_override(NULL)
      state_meta$loaded_display_path <- NULL
    } else if (!is.null(dataset_df)) {
      dataset_override(dataset_df)
      state_meta$loaded_display_path <- res$dataset_display_path %||% source_label
    } else {
      dataset_override(NULL)
      state_meta$loaded_display_path <- NULL
    }

    if (use_project_file || !is.null(dataset_df)) {
      state_meta$suppress_active_data <- TRUE
    }

    if (use_project_file) {
      state_meta$suppress_dataset_source <- TRUE
      state_meta$suppress_project_file <- TRUE
      updateRadioButtons(session, "dataset_source", selected = "project")
      updateSelectInput(session, "project_file", selected = res$project_file)
    } else {
      state_meta$suppress_dataset_source <- TRUE
      updateRadioButtons(session, "dataset_source", selected = "upload")
    }

    fit_meta$is_fitting <- FALSE
    fit_meta$last_runtime_sec <- state$fit_meta$last_runtime_sec %||% NA_real_
    fit_meta$model_runtime_sec <- state$fit_meta$model_runtime_sec %||% res$model_runtime_sec %||% NA_real_
    fit_meta$lrt_runtime_sec <- state$fit_meta$lrt_runtime_sec %||% res$lrt_runtime_sec %||% NA_real_
    fit_meta$last_finished <- state$fit_meta$last_finished %||% res$fitted_at %||% Sys.time()
    dataset_filters_state(normalize_dataset_filters(state$dataset_filters %||% list()))
    fit_result(res)
    prediction_level_orders_state(normalized_prediction_level_orders(res, state$pred_level_orders %||% list()))
    prediction_level_colors_state(normalized_prediction_level_colors(res, state$pred_level_colors %||% list()))
    restored_axes <- normalize_prediction_axis_store(list(
      pred_x = state$pred_x %||% (res$fixed_effects[1] %||% ""),
      pred_group = state$pred_group %||% "__none__"
    ))
    prediction_axis_selection_state(restored_axes)
    tryCatch(save_prediction_axis_store(restored_axes), error = function(e) invisible(FALSE))

    tryCatch(
      persist_fit_state(
        res = res,
        pred_x = state$pred_x %||% "",
        pred_group = state$pred_group %||% "__none__",
        pred_chart_type = state$pred_chart_type %||% "line",
        pred_show_labels = state$pred_show_labels %||% FALSE,
        pred_show_observed_points = state$pred_show_observed_points %||% FALSE,
        pred_level_orders = normalized_prediction_level_orders(res, state$pred_level_orders %||% list()),
        pred_level_colors = normalized_prediction_level_colors(res, state$pred_level_colors %||% list()),
        dataset_filters = state$dataset_filters %||% list(),
        include_raw_data = !is.null(res$raw_data)
      ),
      error = function(e) {
        showNotification(
          paste("Automatic model caching failed:", e$message),
          type = "warning",
          duration = 8
        )
      }
    )

    if (!is.null(dataset_df)) {
      state_meta$suppress_response_var <- TRUE
      state_meta$suppress_fixed_effects <- TRUE
      sync_variable_inputs(
        dataset_df,
        response_selected = res$response_key %||% "",
        fixed_selected = res$fixed_effects,
        random_selected = res$random_effects,
        interaction_selected = res$interaction_terms,
        fit_backend_selected = res$fit_backend %||% "glmer"
      )
    }

    updateSelectInput(session, "optimizer", selected = res$optimizer %||% "bobyqa")
    updateNumericInput(session, "maxfun", value = res$maxfun %||% 200000)
    updateSelectInput(session, "fastglm_method", selected = as.character(res$fastglm_method %||% "3"))
    updateNumericInput(session, "fastglm_maxit", value = res$fastglm_maxit %||% 100)
    updateSelectInput(session, "multinom_method", selected = as.character(res$multinom_method %||% "PQL"))
    updateSelectInput(session, "multinom_catcov", selected = as.character(res$multinom_catcov %||% "single"))
    updateNumericInput(session, "multinom_maxit", value = res$multinom_maxit %||% 25)
    updateCheckboxInput(session, "compute_lrt", value = isTRUE(res$compute_lrt))

    session$onFlushed(function() {
      updateSelectInput(
        session,
        "response_ref",
        selected = res$response_reference_level %||% (res$response_info %||% list())$reference_level %||% ""
      )
      for (v in names(res$reference_levels %||% list())) {
        updateSelectInput(session, paste0("ref_", v), selected = res$reference_levels[[v]])
      }
      updateSelectInput(
        session,
        "pred_x",
        selected = state$pred_x %||% (res$fixed_effects[1] %||% "")
      )
      updateSelectInput(session, "pred_group", selected = state$pred_group %||% "__none__")
      updateRadioButtons(session, "pred_chart_type", selected = state$pred_chart_type %||% "line")
      updateCheckboxInput(session, "pred_show_labels", value = isTRUE(state$pred_show_labels))
      updateCheckboxInput(session, "pred_show_observed_points", value = isTRUE(state$pred_show_observed_points))
      updateCheckboxInput(session, "pred_observed_jitter", value = isTRUE(state$pred_observed_jitter %||% TRUE))
      updateCheckboxInput(session, "pred_observed_cross", value = isTRUE(state$pred_observed_cross %||% TRUE))
      updateCheckboxInput(session, "pred_interactive_hover", value = isTRUE(state$pred_interactive_hover))
    }, once = TRUE)

    loaded_settings <- normalize_app_settings_store(list(
      dataset_source = res$dataset_source %||% "project",
      project_file = res$project_file %||% "",
      response_var = res$response_key %||% "",
      response_ref = res$response_reference_level %||% (res$response_info %||% list())$reference_level %||% "",
      fixed_effects = res$fixed_effects %||% character(0),
      interaction_terms = res$interaction_terms %||% character(0),
      random_effects = res$random_effects %||% character(0),
      fit_backend = res$fit_backend %||% "glmer",
      optimizer = res$optimizer %||% "bobyqa",
      maxfun = res$maxfun %||% 200000,
      fastglm_method = res$fastglm_method %||% "3",
      fastglm_maxit = res$fastglm_maxit %||% 100,
      multinom_method = res$multinom_method %||% "PQL",
      multinom_catcov = res$multinom_catcov %||% "single",
      multinom_maxit = res$multinom_maxit %||% 25,
      reference_levels = res$reference_levels %||% list(),
      compute_lrt = isTRUE(res$compute_lrt),
      dataset_filters = state$dataset_filters %||% list(),
      pred_x = state$pred_x %||% "",
      pred_group = state$pred_group %||% "__none__",
      pred_chart_type = state$pred_chart_type %||% "line",
      pred_show_labels = isTRUE(state$pred_show_labels),
      pred_show_observed_points = isTRUE(state$pred_show_observed_points),
      pred_observed_jitter = isTRUE(state$pred_observed_jitter %||% TRUE),
      pred_observed_cross = isTRUE(state$pred_observed_cross %||% TRUE),
      pred_interactive_hover = isTRUE(state$pred_interactive_hover),
      pred_level_orders = prediction_level_orders_state(),
      pred_level_colors = prediction_level_colors_state()
    ))
    app_settings_state(loaded_settings)
    tryCatch(save_app_settings_store(loaded_settings), error = function(e) invisible(FALSE))

    showNotification(
      if (isTRUE(auto_restored)) {
        "Last fitted model restored automatically."
      } else {
        sprintf("Loaded %s.", source_label)
      },
      type = "message"
    )
  }

  chosen_path <- reactive({
    if (input$dataset_source == "project") {
      req(nzchar(input$project_file %||% ""))
      input$project_file
    } else {
      req(input$upload_file$datapath)
      input$upload_file$datapath
    }
  })

  dataset_ready <- reactive({
    if (!is.null(dataset_override())) return(TRUE)
    if (identical(input$dataset_source, "project")) {
      return(nzchar(input$project_file %||% ""))
    }
    !is.null(input$upload_file$datapath)
  })

  current_dataset_display_path <- reactive({
    if (!is.null(state_meta$loaded_display_path)) {
      return(state_meta$loaded_display_path)
    }
    if (input$dataset_source == "project") {
      if (nzchar(input$project_file %||% "")) input$project_file else "No dataset selected"
    } else {
      input$upload_file$name %||% "No dataset selected"
    }
  })

  raw_data <- reactive({
    read_dataset(chosen_path())
  })

  source_data <- reactive({
    dataset_override() %||% raw_data()
  })

  active_data <- reactive({
    apply_dataset_filters(source_data(), dataset_filters_state())
  })

  run_model_fit <- function() {
    fit_backend <- input$fit_backend %||% "glmer"
    response_key <- input$response_var %||% ""
    if (!nzchar(response_key)) {
      showNotification("Select a dependent variable before fitting.", type = "warning", duration = 8)
      return(invisible(NULL))
    }
    selected_fixed <- input$fixed_effects %||% character(0)
    selected_random <- input$random_effects %||% character(0)
    valid_interactions <- make_interaction_choices(selected_fixed)
    selected_interactions <- intersect(input$interaction_terms %||% character(0), valid_interactions)
    multinom_method <- toupper(as.character(input$multinom_method %||% "PQL"))
    if (!multinom_method %in% c("PQL", "MQL")) multinom_method <- "PQL"
    multinom_catcov <- as.character(input$multinom_catcov %||% "single")
    if (!multinom_catcov %in% c("free", "diagonal", "single")) multinom_catcov <- "single"
    multinom_maxit <- suppressWarnings(as.integer(input$multinom_maxit %||% 25L))
    if (is.na(multinom_maxit) || multinom_maxit < 1L) multinom_maxit <- 25L

    reference_levels <- current_reference_levels(selected_fixed)
    response_reference_level <- input$response_ref %||% ""
    if (!nzchar(response_reference_level)) response_reference_level <- NULL

    opts <- list(
      response_key = response_key,
      response_reference_level = response_reference_level,
      fixed_effects = selected_fixed,
      interaction_terms = selected_interactions,
      random_effects = selected_random,
      reference_levels = reference_levels
    )

    res <- tryCatch({
      fit_meta$is_fitting <- TRUE
      fit_meta$model_runtime_sec <- NA_real_
      fit_meta$lrt_runtime_sec <- NA_real_
      start_time <- Sys.time()
      on.exit({
        fit_meta$is_fitting <- FALSE
        fit_meta$last_finished <- Sys.time()
        fit_meta$last_runtime_sec <- as.numeric(difftime(fit_meta$last_finished, start_time, units = "secs"))
      }, add = TRUE)

      withProgress(message = if (identical(fit_backend, "fastglm")) {
        "Fitting GLM..."
      } else if (identical(fit_backend, "multinom")) {
        "Fitting multinomial model..."
      } else {
        "Fitting GLMM..."
      }, value = 0, {
        incProgress(0.15, detail = "Preparing data")
        current_df <- active_data()
        response_spec <- get_response_spec(current_df, opts$response_key %||% "")

        if (identical(response_spec$family, "multinomial")) {
          if (!identical(fit_backend, "multinom")) {
            stop("Multinomial responses in this app currently require the `mclogit::mblogit` backend.")
          }
        } else {
          if (identical(fit_backend, "glmer") && length(selected_random) == 0) {
            stop("`lme4::glmer` requires at least one random intercept. Switch to `fastglm` for a fixed-effects GLM.")
          }
          if (identical(fit_backend, "fastglm") && length(selected_random) > 0) {
            stop("`fastglm` does not support random intercepts. Clear the random-intercepts field or switch back to `lme4::glmer`.")
          }
          if (identical(fit_backend, "multinom")) {
            stop("`mclogit::mblogit` is only available here for multinomial responses.")
          }
        }

        model_df <- prepare_data(current_df, opts)
        response_info <- attr(model_df, "response_info")
        if (identical(response_info$family, "multinomial") && length(opts$random_effects) > 0) {
          sparse_outcomes <- multinomial_random_effect_sparsity_report(
            model_df,
            random_effects = opts$random_effects,
            min_group_levels = 2L
          )
          if (nrow(sparse_outcomes) > 0) {
            stop(format_multinomial_random_effect_sparsity(
              sparse_outcomes,
              random_effects = opts$random_effects,
              max_levels = 8L
            ))
          }
        }

        incProgress(0.25, detail = "Building formula")
        frm <- build_formula(
          fixed_effects = opts$fixed_effects,
          interaction_terms = opts$interaction_terms,
          random_effects = opts$random_effects,
          response_info = response_info
        )

        model_warnings <- character(0)
        incProgress(
          0.35,
          detail = if (identical(fit_backend, "fastglm")) {
            "Estimating fixed-effects GLM with fastglm"
          } else if (identical(fit_backend, "multinom")) {
            sprintf(
              "Estimating multinomial model with mclogit (%s, catCov = %s)",
              multinom_method,
              multinom_catcov
            )
          } else {
            "Estimating mixed model (this can take a while)"
          }
        )
        model_started <- Sys.time()
        model <- withCallingHandlers(
          {
            if (identical(fit_backend, "fastglm")) {
              fit_fastglm_model(
                frm,
                model_df,
                method = input$fastglm_method %||% "3",
                maxit = input$fastglm_maxit %||% 100
              )
            } else if (identical(fit_backend, "multinom")) {
              fit_multinom_model(
                frm,
                model_df,
                random_effects = opts$random_effects,
                method = multinom_method,
                catCov = multinom_catcov,
                maxit = multinom_maxit
              )
            } else {
              glmer(
                frm,
                data = model_df,
                family = binomial(link = "logit"),
                control = glmerControl(
                  optimizer = input$optimizer,
                  optCtrl = list(maxfun = input$maxfun)
                )
              )
            }
          },
          warning = function(w) {
            model_warnings <<- c(model_warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
        fit_meta$model_runtime_sec <- as.numeric(difftime(Sys.time(), model_started, units = "secs"))

        incProgress(0.7, detail = "Computing fixed-effect table")
        fixed <- compute_fixed_effects(model)
        fixed_warning <- attr(fixed, "warning", exact = TRUE)
        if (!is.null(fixed_warning) && nzchar(fixed_warning)) {
          model_warnings <- c(model_warnings, fixed_warning)
        }
        fixed$term_display <- relabel_model_terms(fixed$term, model_df, opts$fixed_effects)
        lrt <- if (isTRUE(input$compute_lrt) && identical(fit_backend, "glmer")) {
          incProgress(0.9, detail = "Computing likelihood-ratio tests (refits reduced models)")
          lrt_started <- Sys.time()
          out <- tryCatch({
            drop1(model, test = "Chisq")
          }, error = function(e) data.frame(message = paste("LRT not available:", e$message)))
          fit_meta$lrt_runtime_sec <- as.numeric(difftime(Sys.time(), lrt_started, units = "secs"))
          if (is.data.frame(out) && !"message" %in% names(out)) {
            data.frame(term = rownames(out), out, row.names = NULL, check.names = FALSE)
          } else {
            out
          }
        } else if (isTRUE(input$compute_lrt) && identical(fit_backend, "multinom")) {
          data.frame(message = "Likelihood-ratio tests are unavailable for mclogit::mblogit fits in this app.")
        } else if (isTRUE(input$compute_lrt)) {
          data.frame(message = "Likelihood-ratio tests are unavailable for fastglm fits in this app.")
        } else {
          data.frame(message = "Likelihood-ratio tests skipped. Enable 'Compute likelihood-ratio tests' before fitting to run drop1().")
        }

        incProgress(1, detail = "Done")
        list(
          error = NULL,
          model = model,
          data = model_df,
          response_info = attr(model_df, "response_info"),
          response_key = opts$response_key,
          response_reference_level = response_info$reference_level %||% opts$response_reference_level %||% NULL,
          raw_data = current_df,
          dataset_source = input$dataset_source %||% "project",
          dataset_display_path = current_dataset_display_path(),
          project_file = if (identical(input$dataset_source, "project")) input$project_file %||% default_file else "",
          fitted_at = Sys.time(),
          formula = frm,
          fixed = fixed,
          lrt = lrt,
          fixed_effects = opts$fixed_effects,
          interaction_terms = opts$interaction_terms,
          random_effects = opts$random_effects,
          reference_levels = opts$reference_levels,
          fit_backend = fit_backend,
          backend_label = model_backend_label(
            fit_backend,
            input$fastglm_method %||% "3",
            multinom_method,
            multinom_catcov
          ),
          optimizer = if (identical(fit_backend, "glmer")) input$optimizer else NULL,
          maxfun = if (identical(fit_backend, "glmer")) input$maxfun else NULL,
          fastglm_method = if (identical(fit_backend, "fastglm")) as.integer(input$fastglm_method %||% "3") else NA_integer_,
          fastglm_maxit = if (identical(fit_backend, "fastglm")) as.integer(input$fastglm_maxit %||% 100) else NA_integer_,
          multinom_method = if (identical(fit_backend, "multinom")) multinom_method else NULL,
          multinom_catcov = if (identical(fit_backend, "multinom")) multinom_catcov else NULL,
          multinom_maxit = if (identical(fit_backend, "multinom")) as.integer(multinom_maxit) else NA_integer_,
          compute_lrt = isTRUE(input$compute_lrt),
          model_runtime_sec = fit_meta$model_runtime_sec,
          lrt_runtime_sec = fit_meta$lrt_runtime_sec,
          warnings = unique(model_warnings)
        )
      })
    }, error = function(e) {
      list(error = e$message)
    })
    fit_result(res)
    if (is.null(res$error)) {
      pred_level_orders <- normalized_prediction_level_orders(res)
      pred_level_colors <- normalized_prediction_level_colors(res)
      prediction_level_orders_state(pred_level_orders)
      prediction_level_colors_state(pred_level_colors)
      persist_fit_state(
        res = res,
        pred_level_orders = pred_level_orders,
        pred_level_colors = pred_level_colors
      )
    }
  }

  observeEvent(input$dataset_source, {
    if (isTRUE(state_meta$is_restoring) || isTRUE(state_meta$suppress_dataset_source)) {
      state_meta$suppress_dataset_source <- FALSE
      return(invisible(NULL))
    }
    auto_fit_pending(FALSE)
    queued_fit_request(NULL)
    dataset_override(NULL)
    dataset_filters_state(normalize_dataset_filters((app_settings_state() %||% list())$dataset_filters %||% list()))
    state_meta$loaded_display_path <- NULL
    state_meta$clear_model_variables_on_active_data <- TRUE
  }, ignoreInit = TRUE)

  observeEvent(input$project_file, {
    if (isTRUE(state_meta$is_restoring) || isTRUE(state_meta$suppress_project_file)) {
      state_meta$suppress_project_file <- FALSE
      return(invisible(NULL))
    }
    auto_fit_pending(FALSE)
    queued_fit_request(NULL)
    dataset_override(NULL)
    dataset_filters_state(normalize_dataset_filters((app_settings_state() %||% list())$dataset_filters %||% list()))
    state_meta$loaded_display_path <- NULL
    state_meta$clear_model_variables_on_active_data <- TRUE
  }, ignoreInit = TRUE)

  observeEvent(input$upload_file, {
    if (isTRUE(state_meta$is_restoring) || isTRUE(state_meta$suppress_upload_file)) {
      state_meta$suppress_upload_file <- FALSE
      return(invisible(NULL))
    }
    auto_fit_pending(FALSE)
    queued_fit_request(NULL)
    dataset_override(NULL)
    dataset_filters_state(normalize_dataset_filters((app_settings_state() %||% list())$dataset_filters %||% list()))
    state_meta$loaded_display_path <- NULL
    state_meta$clear_model_variables_on_active_data <- TRUE
  }, ignoreInit = TRUE)

  observeEvent(active_data(), {
    if (isTRUE(state_meta$is_restoring) || isTRUE(state_meta$suppress_active_data)) {
      state_meta$suppress_active_data <- FALSE
      return(invisible(NULL))
    }
    settings <- app_settings_state()
    clear_model_variables <- isTRUE(state_meta$clear_model_variables_on_active_data)
    state_meta$clear_model_variables_on_active_data <- FALSE
    sync_variable_inputs(
      active_data(),
      response_selected = if (clear_model_variables) "" else input$response_var %||% "",
      fixed_selected = if (clear_model_variables) character(0) else input$fixed_effects %||% character(0),
      random_selected = nonempty_or(input$random_effects, settings$random_effects %||% character(0)),
      interaction_selected = nonempty_or(input$interaction_terms, settings$interaction_terms %||% character(0)),
      fit_backend_selected = nonempty_or(input$fit_backend, settings$fit_backend %||% "glmer")
    )
    reset_fit_state()
    if (isTRUE(auto_fit_pending())) {
      auto_fit_pending(FALSE)
      session$onFlushed(function() {
        queued_fit_request(Sys.time())
      }, once = TRUE)
    }
  }, ignoreInit = FALSE)

  observeEvent(queued_fit_request(), {
    req(!is.null(queued_fit_request()))
    if (isTRUE(state_meta$is_restoring)) return(invisible(NULL))
    current_res <- fit_result()
    if (!is.null(current_res) && is.null(current_res$error)) return(invisible(NULL))
    run_model_fit()
  }, ignoreInit = TRUE)

  observeEvent(input$response_var, {
    if (isTRUE(state_meta$is_restoring) || isTRUE(state_meta$suppress_response_var)) {
      state_meta$suppress_response_var <- FALSE
      return(invisible(NULL))
    }
    sync_variable_inputs(
      active_data(),
      response_selected = input$response_var %||% "",
      fixed_selected = input$fixed_effects %||% character(0),
      random_selected = input$random_effects %||% character(0),
      interaction_selected = input$interaction_terms %||% character(0),
      fit_backend_selected = input$fit_backend %||% "glmer"
    )
    reset_fit_state()
  }, ignoreInit = TRUE)

  observeEvent(input$fit_backend, {
    if (isTRUE(state_meta$is_restoring) || isTRUE(state_meta$suppress_fit_backend)) {
      state_meta$suppress_fit_backend <- FALSE
      return(invisible(NULL))
    }
    if (identical(input$fit_backend, "fastglm") && isTRUE(dataset_ready())) {
      updateSelectizeInput(session, "random_effects", selected = character(0), server = TRUE)
    }
    reset_fit_state()
  }, ignoreInit = TRUE)

  observeEvent(input$fixed_effects, {
    if (isTRUE(state_meta$is_restoring) || isTRUE(state_meta$suppress_fixed_effects)) {
      state_meta$suppress_fixed_effects <- FALSE
      return(invisible(NULL))
    }
    selected_fixed <- input$fixed_effects %||% character(0)
    valid_interactions <- make_interaction_choices(selected_fixed)
    selected_interactions <- intersect(input$interaction_terms %||% character(0), valid_interactions)
    updateSelectizeInput(
      session,
      "interaction_terms",
      choices = valid_interactions,
      selected = selected_interactions,
      server = TRUE
    )
  }, ignoreInit = TRUE)

  observe({
    list(
      input$dataset_source,
      input$project_file,
      input$response_var,
      input$response_ref,
      input$fixed_effects,
      input$interaction_terms,
      input$random_effects,
      input$fit_backend,
      input$optimizer,
      input$maxfun,
      input$fastglm_method,
      input$fastglm_maxit,
      input$multinom_method,
      input$multinom_catcov,
      input$multinom_maxit,
      input$compute_lrt,
      current_reference_levels(),
      dataset_filters_state(),
      input$pred_x,
      input$pred_group,
      input$pred_chart_type,
      input$pred_show_labels,
      input$pred_show_observed_points,
      input$pred_observed_jitter,
      input$pred_observed_cross,
      input$pred_interactive_hover,
      prediction_level_orders_state(),
      prediction_level_colors_state()
    )
    if (isTRUE(state_meta$is_restoring) || isTRUE(state_meta$is_applying_settings) || isTRUE(state_meta$suppress_settings_save)) {
      return(invisible(NULL))
    }
    persist_app_settings()
  })

  output$reference_levels_ui <- renderUI({
    if (!dataset_ready()) {
      return(helpText("Select a dataset first."))
    }
    df <- active_data()
    fx <- input$fixed_effects %||% character(0)
    response_key <- input$response_var %||% ""
    response_spec <- if (nzchar(response_key)) {
      tryCatch(get_response_spec(df, response_key), error = function(e) NULL)
    } else {
      NULL
    }
    settings <- app_settings_state()

    controls <- list()
    if (!is.null(response_spec) && identical(response_spec$family, "multinomial")) {
      outcome_vals <- trimws(as.character(df[[response_spec$response_col]]))
      outcome_vals[is.na(outcome_vals)] <- ""
      response_levels <- response_spec$outcome_levels %||% sort(unique(outcome_vals[nzchar(outcome_vals)]))
      if (length(response_levels) > 1) {
        selected_response_ref <- nonempty_or(input$response_ref, settings$response_ref %||% response_spec$reference_level %||% response_levels[[1]])
        if (!selected_response_ref %in% response_levels) {
          selected_response_ref <- response_levels[[1]]
        }
        controls[[length(controls) + 1L]] <- selectInput(
          inputId = "response_ref",
          label = "Reference outcome level",
          choices = response_levels,
          selected = selected_response_ref
        )
      }
    } else if (!is.null(response_spec) && identical(response_spec$family %||% "binomial", "binomial")) {
      response_levels <- response_level_choices(response_spec)
      if (length(response_levels) > 1) {
        selected_response_ref <- nonempty_or(input$response_ref, settings$response_ref %||% resolve_response_reference(response_spec))
        if (!selected_response_ref %in% response_levels) {
          selected_response_ref <- response_levels[[1]]
        }
        controls[[length(controls) + 1L]] <- selectInput(
          inputId = "response_ref",
          label = "Reference outcome level",
          choices = response_levels,
          selected = selected_response_ref
        )
      }
    }

    fixed_controls <- lapply(fx, function(v) {
      if (!v %in% names(df)) return(NULL)
      if (is.numeric(df[[v]])) return(NULL)

      vals <- as.character(df[[v]])
      vals <- vals[!is.na(vals) & vals != ""]
      lvls <- sort(unique(vals))
      if (length(lvls) <= 1 || length(lvls) > 50) return(NULL)
      selected_ref <- nonempty_or(input[[paste0("ref_", v)]], (settings$reference_levels %||% list())[[v]] %||% lvls[1])
      if (!selected_ref %in% lvls) selected_ref <- lvls[1]

      selectInput(
        inputId = paste0("ref_", v),
        label = paste("Reference level:", v),
        choices = lvls,
        selected = selected_ref
      )
    })

    controls <- c(controls, Filter(Negate(is.null), fixed_controls))
    if (length(controls) == 0) {
      return(helpText("Select fixed effects first."))
    }

    tagList(controls)
  })

  open_preview_filter_modal <- function(col_idx) {
    df <- source_data()
    if (is.null(df) || ncol(df) == 0) return(invisible(NULL))

    col_idx <- suppressWarnings(as.integer(col_idx))
    if (is.na(col_idx) || col_idx < 1 || col_idx > ncol(df)) return(invisible(NULL))

    col_name <- names(df)[col_idx]
    current_filter <- normalize_dataset_filters(dataset_filters_state())[[col_name]] %||% list()
    can_collapse_to_other <- filter_can_collapse_to_other(df[[col_name]])
    filter_target_column(col_name)

    showModal(modalDialog(
      title = paste("Filter column:", col_name),
      tags$p("Select values to exclude or keep, or use regex search/replace to recode values in this column."),
      tags$strong("Regex search/replace"),
      textInput(
        "preview_filter_search_pattern",
        "Search pattern",
        value = current_filter$search_pattern %||% "",
        placeholder = "Regular expression, e.g. ^(foo|bar)$"
      ),
      textInput(
        "preview_filter_replacement",
        "Replace with",
        value = current_filter$replacement %||% "",
        placeholder = "New value"
      ),
      checkboxInput(
        "preview_filter_ignore_case",
        "Ignore case",
        value = isTRUE(current_filter$ignore_case)
      ),
      tags$hr(),
      radioButtons(
        "preview_filter_mode",
        "Filtering rule",
        choices = c("Exclude selected values" = "exclude", "Keep only selected values" = "include"),
        selected = current_filter$mode %||% "exclude"
      ),
      uiOutput("preview_filter_values_ui"),
      if (isTRUE(can_collapse_to_other)) {
        checkboxInput(
          "preview_filter_collapse_other",
          "Collapse rows that would be removed to `other` instead",
          value = isTRUE(current_filter$collapse_to_other)
        )
      } else {
        tags$small("This column can only be filtered; it cannot be collapsed to `other`.")
      },
      uiOutput("preview_filter_summary"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("clear_preview_filter", "Clear this filter"),
        actionButton("apply_preview_filter", "Apply preprocessing", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  }

  output$preview_filter_values_ui <- renderUI({
    col_name <- filter_target_column() %||% ""
    if (!nzchar(col_name) || !dataset_ready()) return(NULL)

    df <- source_data()
    if (is.null(df) || !col_name %in% names(df)) return(NULL)

    current_filter <- normalize_dataset_filters(dataset_filters_state())[[col_name]] %||% list()
    search_pattern <- as.character(input$preview_filter_search_pattern %||% current_filter$search_pattern %||% "")
    replacement <- as.character(input$preview_filter_replacement %||% current_filter$replacement %||% "")
    ignore_case <- isTRUE(input$preview_filter_ignore_case %||% current_filter$ignore_case)
    replacement_spec <- list(
      search_pattern = search_pattern,
      replacement = replacement,
      ignore_case = ignore_case
    )

    values_col <- if (filter_regex_is_valid(search_pattern)) {
      apply_filter_replacement(df[[col_name]], replacement_spec)
    } else {
      df[[col_name]]
    }
    choices <- filter_choice_vector_with_counts(values_col)
    value_choices_available <- length(choices) > 1 && length(choices) <= 5000
    selected_values <- unique(as.character(input$preview_filter_values %||% current_filter$values %||% character(0)))
    if (isTRUE(value_choices_available)) {
      selected_values <- intersect(selected_values, unname(choices))
    } else {
      selected_values <- character(0)
    }

    tagList(
      if (length(choices) <= 1) {
        tags$small("The current column values after regex replacement do not have enough distinct values for selected-value filtering.")
      } else if (length(choices) > 5000) {
        tags$small("The current column values after regex replacement have too many distinct values for selected-value filtering.")
      },
      selectizeInput(
        "preview_filter_values",
        "Column values after regex replacement",
        choices = if (isTRUE(value_choices_available)) choices else setNames(character(0), character(0)),
        selected = selected_values,
        multiple = TRUE,
        options = list(
          plugins = list("remove_button"),
          placeholder = "Select one or more values"
        )
      )
    )
  })

  output$preview_filter_summary <- renderUI({
    col_name <- filter_target_column() %||% ""
    if (!nzchar(col_name) || !dataset_ready()) return(NULL)

    df <- source_data()
    if (is.null(df) || !col_name %in% names(df)) return(NULL)

    current_filter <- normalize_dataset_filters(dataset_filters_state())[[col_name]] %||% list()
    mode <- as.character(input$preview_filter_mode %||% current_filter$mode %||% "exclude")
    if (!mode %in% c("include", "exclude")) mode <- "exclude"
    selected_values <- unique(as.character(input$preview_filter_values %||% current_filter$values %||% character(0)))
    collapse_to_other <- isTRUE(input$preview_filter_collapse_other %||% current_filter$collapse_to_other) &&
      filter_can_collapse_to_other(df[[col_name]])
    search_pattern <- as.character(input$preview_filter_search_pattern %||% current_filter$search_pattern %||% "")
    replacement <- as.character(input$preview_filter_replacement %||% current_filter$replacement %||% "")
    ignore_case <- isTRUE(input$preview_filter_ignore_case %||% current_filter$ignore_case)

    replacement_spec <- list(
      search_pattern = search_pattern,
      replacement = replacement,
      ignore_case = ignore_case
    )
    replacement_valid <- filter_regex_is_valid(search_pattern)
    filter_base <- if (isTRUE(replacement_valid)) {
      apply_filter_replacement(df[[col_name]], replacement_spec)
    } else {
      df[[col_name]]
    }
    counts <- filter_effect_counts(
      x = filter_base,
      mode = mode,
      values = selected_values,
      collapse_to_other = collapse_to_other
    )
    replacement_counts <- filter_replacement_counts(df[[col_name]], replacement_spec)

    help_lines <- if (length(selected_values) == 0) {
      c(
        sprintf(
          "No values selected. Selected-value filtering will keep all %s rows.",
          format(counts$total_n, big.mark = ",")
        ),
        sprintf("Totals: %s.", format_filter_effect_counts(counts))
      )
    } else {
      c(
        sprintf(
          "Selected values in this column account for %s rows.",
          format(counts$selected_n, big.mark = ",")
        ),
        sprintf("Totals after applying this rule: %s.", format_filter_effect_counts(counts))
      )
    }
    if (nzchar(search_pattern)) {
      help_lines <- c(
        help_lines,
        if (isTRUE(replacement_valid)) {
          sprintf(
            "Regex replacement runs first, changing %s rows and distinct values from %s to %s. Selected-value filtering then uses the replaced values.",
            format(replacement_counts$matched_n, big.mark = ","),
            format(replacement_counts$distinct_before, big.mark = ","),
            format(replacement_counts$distinct_after, big.mark = ",")
          )
        } else {
          "Regex replacement pattern is invalid."
        }
      )
    }

    tags$div(
      style = "margin-top: 8px;",
      tags$small(paste(help_lines, collapse = " "))
    )
  })

  output$dataset_filter_ui <- renderUI({
    validate(need(dataset_ready(), ""))

    filters <- normalize_dataset_filters(dataset_filters_state())
    source_n <- nrow(source_data())
    active_n <- nrow(active_data())

    if (length(filters) == 0) {
      return(tagList(
        tags$small("Click any preview-table cell or column header to filter that column or collapse selected categorical values to `other`."),
        tags$div(style = "margin: 6px 0 10px;", sprintf("Showing all %s rows.", format(source_n, big.mark = ",")))
      ))
    }

    filter_lines <- describe_dataset_filters(source_data(), filters)
    tagList(
      tags$small("Click a preview-table cell or column header to edit a filter. The preview shows the first 100 rows after any filtering or recoding."),
      tags$div(
        style = "margin: 6px 0;",
        sprintf(
          "Showing %s of %s rows after dataset preprocessing.",
          format(active_n, big.mark = ","),
          format(source_n, big.mark = ",")
        )
      ),
      tags$ul(lapply(filter_lines, tags$li)),
      actionButton("clear_dataset_filters", "Clear all dataset filters")
    )
  })

  output$data_info <- renderPrint({
    validate(need(dataset_ready(), "Select a dataset to begin."))
    source_df <- source_data()
    df <- active_data()
    filters <- normalize_dataset_filters(dataset_filters_state())
    response_key <- input$response_var %||% ""
    response_info <- if (nzchar(response_key)) {
      tryCatch(
        summarize_response_data(
          df,
          response_key,
          input$response_ref %||% NULL
        ),
        error = function(e) NULL
      )
    } else {
      NULL
    }
    cat("Source:", current_dataset_display_path(), "\n")
    if (length(filters) > 0) {
      cat("Rows after dataset preprocessing:", nrow(df), "\n")
      cat("Rows before dataset preprocessing:", nrow(source_df), "\n")
    } else {
      cat("Rows:", nrow(df), "\n")
    }
    if (length(filters) > 0) {
      for (line in describe_dataset_preprocessing(source_df, df, filters, max_values = 6L)) cat(line, "\n")
    }
    cat("Columns:", ncol(df), "\n")
    if (!is.null(response_info)) {
      cat("Dependent variable:", response_info$label, "\n")
      cat("Response family:", response_info$family %||% "binomial", "\n")
      cat("Outcome layout:", response_info$mode, "\n")
      cat("Outcome columns:", paste(response_info$outcome_cols, collapse = ", "), "\n")
      cat("Usable outcome rows:", response_info$usable_rows, "\n")
      for (line in response_count_lines(response_info)) cat(line, "\n")
    }
    cat("Has `sg`:", "sg" %in% names(df), "\n")
    cat("Has `pl`:", "pl" %in% names(df), "\n")
    cat("Has `plural`:", "plural" %in% names(df), "\n")
    cat("Has `q_lemma`:", "q_lemma" %in% names(df), "\n")
    cat("Has `period_simple`:", "period_simple" %in% names(df), "\n")
    cat("Has `semantics`:", "semantics" %in% names(df), "\n")
    cat("Has `lemma`:", "lemma" %in% names(df), "\n")
  })

  output$preview_table <- renderDT({
    validate(need(dataset_ready(), "Select a dataset to preview."))
    df <- active_data()
    datatable(
      head(df, 100),
      rownames = FALSE,
      selection = "none",
      callback = JS(
        "function attachPreviewTopScroll() {",
        "  var container = $(table.table().container());",
        "  var body = container.find('div.dataTables_scrollBody');",
        "  if (!body.length) return;",
        "  var existing = container.find('div.preview-table-top-scroll');",
        "  var top = existing.length ? existing : $('<div class=\"preview-table-top-scroll\"><div></div></div>');",
        "  if (!existing.length) {",
        "    top.css({ overflowX: 'auto', overflowY: 'hidden', height: '16px', marginBottom: '2px' });",
        "    top.children().css({ height: '1px' });",
        "    container.find('div.dataTables_scrollHead').before(top);",
        "  }",
        "  var syncWidth = function() { top.children().width(body.get(0).scrollWidth); };",
        "  syncWidth();",
        "  top.off('scroll.previewTop').on('scroll.previewTop', function() { body.scrollLeft(this.scrollLeft); });",
        "  body.off('scroll.previewTop').on('scroll.previewTop', function() { top.scrollLeft(this.scrollLeft); });",
        "  table.off('draw.previewTop column-sizing.previewTop').on('draw.previewTop column-sizing.previewTop', syncWidth);",
        "  $(window).off('resize.previewTop').on('resize.previewTop', syncWidth);",
        "}",
        "setTimeout(attachPreviewTopScroll, 0);",
        "table.on('click.dt', 'thead th', function() {",
        "  var idx = table.column(this).index();",
        "  if (idx === undefined) return;",
        "  Shiny.setInputValue('preview_table_column_click', { col: idx + 1, nonce: Date.now() }, { priority: 'event' });",
        "});",
        "table.on('click.dt', 'tbody td', function() {",
        "  var idx = table.cell(this).index();",
        "  if (!idx) return;",
        "  Shiny.setInputValue('preview_table_column_click', { col: idx.column + 1, nonce: Date.now() }, { priority: 'event' });",
        "});"
      ),
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  observeEvent(input$preview_table_column_click, {
    open_preview_filter_modal((input$preview_table_column_click %||% list())$col %||% NA_integer_)
  }, ignoreInit = TRUE)

  observeEvent(input$apply_preview_filter, {
    col_name <- filter_target_column() %||% ""
    if (!nzchar(col_name)) return(invisible(NULL))

    filters <- dataset_filters_state()
    selected_values <- unique(as.character(input$preview_filter_values %||% character(0)))
    collapse_to_other <- isTRUE(input$preview_filter_collapse_other) &&
      filter_can_collapse_to_other(source_data()[[col_name]])
    search_pattern <- as.character(input$preview_filter_search_pattern %||% "")
    replacement <- as.character(input$preview_filter_replacement %||% "")
    ignore_case <- isTRUE(input$preview_filter_ignore_case)
    if (nzchar(search_pattern) && !filter_regex_is_valid(search_pattern)) {
      showNotification("Regex search pattern is invalid.", type = "error", duration = 8)
      return(invisible(NULL))
    }

    if (length(selected_values) == 0 && !nzchar(search_pattern)) {
      filters[[col_name]] <- NULL
    } else {
      filters[[col_name]] <- list(
        mode = input$preview_filter_mode %||% "exclude",
        values = selected_values,
        collapse_to_other = collapse_to_other,
        search_pattern = search_pattern,
        replacement = replacement,
        ignore_case = ignore_case
      )
    }

    filtered_df <- apply_dataset_filters(source_data(), filters)
    dataset_filters_state(normalize_dataset_filters(filters))
    filter_target_column(NULL)
    removeModal()

    if (nrow(filtered_df) == 0) {
      showNotification("Current dataset preprocessing leaves no rows.", type = "warning", duration = 8)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$clear_preview_filter, {
    col_name <- filter_target_column() %||% ""
    if (!nzchar(col_name)) return(invisible(NULL))

    filters <- dataset_filters_state()
    filters[[col_name]] <- NULL
    dataset_filters_state(normalize_dataset_filters(filters))
    filter_target_column(NULL)
    removeModal()
  }, ignoreInit = TRUE)

  observeEvent(input$clear_dataset_filters, {
    if (length(normalize_dataset_filters(dataset_filters_state())) == 0) return(invisible(NULL))
    dataset_filters_state(list())
  }, ignoreInit = TRUE)

  output$class_balance_plot <- renderPlot({
    validate(need(dataset_ready(), "Select a dataset to inspect outcome balance."))
    df <- active_data()
    response_key <- input$response_var %||% ""
    validate(need(nzchar(response_key), "Select a dependent variable to inspect outcome balance."))
    response_info <- tryCatch(
      summarize_response_data(
        df,
        response_key,
        input$response_ref %||% NULL
      ),
      error = function(e) NULL
    )
    validate(need(
      !is.null(response_info),
      "Dataset needs a supported binomial or multinomial response column."
    ))
    balance <- response_count_table(response_info)
    if (response_is_multinomial(response_info) && nrow(balance) > 20) {
      other_n <- sum(balance$n[-seq_len(20)])
      balance <- rbind(
        balance[seq_len(20), , drop = FALSE],
        data.frame(outcome = "Other", n = other_n, stringsAsFactors = FALSE)
      )
    }
    balance$outcome <- factor(balance$outcome, levels = rev(balance$outcome))
    fill_vals <- setNames(default_prediction_level_palette(nrow(balance)), as.character(balance$outcome))
    ggplot(balance, aes(x = outcome, y = n, fill = outcome)) +
      geom_col(width = 0.7, alpha = 0.8) +
      coord_flip() +
      scale_fill_manual(values = fill_vals, guide = "none") +
      labs(x = "Outcome", y = "Token count", title = "Dependent Variable Balance") +
      theme_minimal(base_size = 12)
  })

  observeEvent(input$fit_model, {
    run_model_fit()
  }, ignoreInit = TRUE)

  output$fit_indicator <- renderPrint({
    if (fit_meta$is_fitting) {
      cat("Status: fitting in progress...\n")
      cat("A progress bar should be visible while estimation runs.\n")
      return(invisible(NULL))
    }
    if (is.null(fit_result())) {
      cat("Status: model not fitted yet.\n")
      cat("Click 'Fit / Refit model' to run the analysis.\n")
      return(invisible(NULL))
    }
    cat("Status: last fit completed.\n")
    res <- fit_result()
    if (!is.null(res$error)) {
      cat("Last attempt ended with an error.\n")
      return(invisible(NULL))
    }
    cat(
      "Backend:",
      res$backend_label %||% model_backend_label(
        res$fit_backend %||% "glmer",
        res$fastglm_method %||% NULL,
        res$multinom_method %||% NULL,
        res$multinom_catcov %||% NULL
      ),
      "\n"
    )
    if (!is.na(fit_meta$last_runtime_sec)) {
      cat(sprintf("Total runtime: %.1f seconds\n", fit_meta$last_runtime_sec))
    }
    if (!is.na(fit_meta$model_runtime_sec)) {
      cat(sprintf("Model fit time: %.1f seconds\n", fit_meta$model_runtime_sec))
    }
    if (isTRUE(res$compute_lrt) && !is.na(fit_meta$lrt_runtime_sec)) {
      cat(sprintf("Likelihood-ratio test time: %.1f seconds\n", fit_meta$lrt_runtime_sec))
    } else if (isTRUE(res$compute_lrt)) {
      cat("Likelihood-ratio test time: not available\n")
    } else {
      cat("Likelihood-ratio test time: skipped\n")
    }
    if (!is.null(fit_meta$last_finished)) {
      cat("Finished:", format(fit_meta$last_finished, "%Y-%m-%d %H:%M:%S"), "\n")
    }
  })

  output$model_formula <- renderPrint({
    if (is.null(fit_result())) {
      cat("No model yet. Click 'Fit / Refit model'.\n")
      return(invisible(NULL))
    }
    res <- fit_result()
    if (!is.null(res$error)) return(cat("Formula unavailable due to error.\n"))
    cat("Model formula:\n")
    print(res$formula)
  })

  output$model_status <- renderPrint({
    if (is.null(fit_result())) {
      cat("No model yet. Click 'Fit / Refit model'.\n")
      return(invisible(NULL))
    }
    res <- fit_result()
    if (!is.null(res$error)) {
      cat("Model fit failed:\n", res$error, "\n", sep = "")
      return(invisible(NULL))
    }
    response_info <- normalize_response_info(res$response_info %||% attr(res$data, "response_info"))
    input_rows <- response_info$input_rows %||% nrow(res$data)
    modeled_rows <- response_info$modeled_rows %||% nrow(res$data)
    if (!isTRUE(all.equal(input_rows, modeled_rows))) {
      cat("Rows used:", modeled_rows, "(collapsed from", input_rows, "equivalent observations)\n")
    } else {
      cat("Rows used:", modeled_rows, "\n")
    }
    if (!is.null(response_info)) {
      cat("Dependent variable:", response_info$label, "\n")
      cat("Response family:", response_info$family %||% "binomial", "\n")
      for (line in response_count_lines(response_info)) cat(line, "\n")
    }
    cat(
      "Backend:",
      res$backend_label %||% model_backend_label(
        res$fit_backend %||% "glmer",
        res$fastglm_method %||% NULL,
        res$multinom_method %||% NULL,
        res$multinom_catcov %||% NULL
      ),
      "\n"
    )
    cat("Fixed effects:", paste(res$fixed_effects, collapse = ", "), "\n")
    cat("Interactions:", if (length(res$interaction_terms) == 0) "None" else paste(res$interaction_terms, collapse = ", "), "\n")
    if (identical(res$fit_backend %||% "", "multinom")) {
      cat("mclogit approximation:", res$multinom_method %||% "PQL", "\n")
      cat("mclogit catCov:", res$multinom_catcov %||% "single", "\n")
      cat("mclogit maxit:", as.integer(res$multinom_maxit %||% 25L), "\n")
    }
    if (length(res$random_effects) == 0) {
      cat("Random intercepts: None\n")
    } else {
      cat("Random intercepts:", paste(res$random_effects, collapse = ", "), "\n")
      for (rv in res$random_effects) {
        if (rv %in% names(res$data)) {
          cat(" - levels(", rv, "): ", nlevels(res$data[[rv]]), "\n", sep = "")
        }
      }
    }
    if (is_mixed_model(res$model)) {
      cat("Singular fit:", isSingular(res$model, tol = 1e-4), "\n")
    } else if (identical(res$fit_backend %||% "", "multinom")) {
      cat("Singular fit: not applicable for mclogit::mblogit fits\n")
    } else {
      cat("Singular fit: not applicable for fixed-effects fastglm fits\n")
    }
    if (!is.na(res$model_runtime_sec %||% NA_real_)) {
      cat(sprintf("Model fit time: %.1f seconds\n", res$model_runtime_sec), "\n")
    }
    if (isTRUE(res$compute_lrt) && !is.na(res$lrt_runtime_sec %||% NA_real_)) {
      cat(sprintf("Likelihood-ratio test time: %.1f seconds\n", res$lrt_runtime_sec), "\n")
    }
    if (length(res$warnings) > 0) {
      cat("\nModel warnings:\n")
      for (w in res$warnings) cat("- ", w, "\n", sep = "")
    }
    cat("\nLikelihood-ratio tests (drop1):\n")
    print(res$lrt)
  })

  output$model_summary <- renderPrint({
    if (is.null(fit_result())) {
      cat("No model yet. Click 'Fit / Refit model'.\n")
      return(invisible(NULL))
    }
    res <- fit_result()
    if (!is.null(res$error)) return(cat("No summary due to fit error.\n"))
    cat(format_model_summary_lines(res$model, res$data, res$fixed_effects), sep = "\n")
  })

  output$fixed_effects_table <- renderDT({
    if (is.null(fit_result())) {
      return(datatable(data.frame(info = "No model yet. Click 'Fit / Refit model'."), options = list(dom = "t")))
    }
    res <- fit_result()
    if (is.null(res)) {
      return(datatable(data.frame(info = "Model is currently fitting..."), options = list(dom = "t")))
    }
    validate(need(is.null(res$error), res$error))
    fixed_display <- res$fixed
    if ("term_display" %in% names(fixed_display)) {
      fixed_display$term <- fixed_display$term_display
      fixed_display$term_display <- NULL
    }
    datatable(fixed_display, options = list(pageLength = 15, scrollX = TRUE))
  })

  interpretation_text <- reactive({
    res <- fit_result()
    if (is.null(res)) {
      return(paste(
        "Fit a model first, then this tab will generate a draft interpretation.",
        "The text is based on the fitted fixed-effect coefficients and Wald intervals.",
        sep = "\n"
      ))
    }
    if (!is.null(res$error)) {
      return(paste(
        "Model fit failed, so interpretive text is not available.",
        paste0("Fit error: ", res$error),
        sep = "\n"
      ))
    }

    response_info <- normalize_response_info(res$response_info %||% attr(res$data, "response_info"))
    fixed_tbl <- res$fixed %||% data.frame()
    sig_lines <- summarize_significant_terms(
      fixed_tbl = fixed_tbl,
      model_df = res$data,
      fixed_effects = res$fixed_effects,
      response_info = response_info
    )

    factor_refs <- vapply(res$fixed_effects, function(v) {
      if (!v %in% names(res$data) || is.numeric(res$data[[v]]) || !is.factor(res$data[[v]])) return("")
      ref <- (res$reference_levels %||% list())[[v]] %||% (levels(res$data[[v]])[1] %||% "")
      if (!nzchar(ref)) return("")
      sprintf("%s = %s", v, ref)
    }, character(1))
    factor_refs <- factor_refs[nzchar(factor_refs)]

    lrt_line <- NULL
    if (is.data.frame(res$lrt) && nrow(res$lrt) > 0) {
      p_col <- names(res$lrt)[grepl("^Pr", names(res$lrt))]
      if (length(p_col) > 0 && "term" %in% names(res$lrt)) {
        p_vals <- suppressWarnings(as.numeric(res$lrt[[p_col[1]]]))
        sig_terms <- res$lrt$term[!is.na(p_vals) & p_vals < 0.05]
        sig_terms <- setdiff(sig_terms, "<none>")
        if (length(sig_terms) > 0) {
          lrt_line <- paste(
            "Likelihood-ratio tests also indicate that the following terms improve model fit:",
            paste(sig_terms, collapse = ", "),
            "."
          )
        }
      }
    }

    has_sig_interaction <- any(
      fixed_tbl$term != "(Intercept)" &
        grepl(":", fixed_tbl$term, fixed = TRUE) &
        !is.na(fixed_tbl$p_value) &
        fixed_tbl$p_value < 0.05
    )
    preprocessing_lines <- describe_dataset_preprocessing(
      source_df = source_data(),
      filtered_df = active_data(),
      filters = dataset_filters_state(),
      max_values = 8L
    )

    lines <- c(
      "Draft interpretation for citation.",
      "This summary is generated from the fixed-effect coefficient table with Wald confidence intervals; edit the wording to match the reporting conventions of your paper.",
      if (response_is_multinomial(response_info)) {
        sprintf(
          "The fitted model estimates the odds of each outcome category relative to %s.",
          response_info$reference_level %||% "the reference outcome"
        )
      } else {
        sprintf("The fitted model estimates the probability of %s.", response_info$success_label %||% "success")
      }
    )

    if (length(preprocessing_lines) > 0) {
      lines <- c(lines, "", preprocessing_lines)
    }

    if (length(factor_refs) > 0) {
      lines <- c(lines, paste("Reference levels for categorical predictors:", paste(factor_refs, collapse = "; "), "."))
    }

    if (length(sig_lines) == 0) {
      lines <- c(
        lines,
        "No non-intercept fixed-effect coefficients reached p < 0.05 in the Wald coefficient table."
      )
    } else {
      lines <- c(lines, "Statistically significant findings:")
      lines <- c(lines, paste0(seq_along(sig_lines), ". ", sig_lines))
    }

    if (!is.null(lrt_line)) {
      lines <- c(lines, "", lrt_line)
    }

    if (isTRUE(has_sig_interaction)) {
      lines <- c(
        lines,
        "",
        "At least one interaction term is significant, so any main-effect statement should be read as conditional on the reference or held-constant values of the interacting predictors."
      )
    }

    paste(lines, collapse = "\n")
  })

  output$interpretation_ui <- renderUI({
    tags$textarea(
      id = "interpretation_text",
      style = "width: 100%; height: 420px; font-family: monospace; white-space: pre-wrap;",
      readonly = "readonly",
      interpretation_text()
    )
  })

  coefficient_plot <- function() {
    if (is.null(fit_result())) {
      validate(need(FALSE, "No model yet. Click 'Fit / Refit model'."))
    }
    res <- fit_result()
    validate(need(!is.null(res), "Model is currently fitting..."))
    validate(need(is.null(res$error), res$error))

    fe <- res$fixed
    fe$.plot_term <- fe$term_display %||% fe$term
    fe <- fe[fe$term != "(Intercept)", , drop = FALSE]
    validate(need(nrow(fe) > 0, "No non-intercept fixed effects to plot."))
    p <- ggplot(fe, aes(y = reorder(.plot_term, odds_ratio), x = odds_ratio)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
      geom_errorbarh(aes(xmin = or_ci_low, xmax = or_ci_high), height = 0.2, color = "#4C78A8") +
      geom_point(size = 2.3, color = "#F58518") +
      scale_x_log10() +
      labs(
        x = "Odds ratio (log scale) with 95% Wald CI",
        y = "Term",
        title = if ("outcome" %in% names(fe)) "Fixed Effects Odds Ratios by Outcome" else "Fixed Effects Odds Ratios"
      ) +
      theme_minimal(base_size = 12)
    if ("outcome" %in% names(fe)) {
      p <- p + facet_wrap(~ outcome, scales = "free_y")
    }
    p
  }

  output$coef_plot <- renderPlot({
    coefficient_plot()
  })

  output$pred_controls_ui <- renderUI({
    if (is.null(fit_result())) {
      return(helpText("Fit a model to enable prediction controls."))
    }
    res <- fit_result()
    if (!is.null(res$error)) {
      return(helpText("Prediction controls unavailable because model fit failed."))
    }

    fx <- res$fixed_effects
    if (length(fx) == 0) return(helpText("No fixed effects available for predictions."))
    response_info <- normalize_response_info(res$response_info %||% attr(res$data, "response_info"))
    chart_choices <- c("Line" = "line", "Bar" = "bar", "Violin" = "violin")
    axes <- prediction_axis_selection_state()

    default_x <- if ("period_simple" %in% fx) "period_simple" else fx[1]
    default_group <- if ("semantics" %in% fx && "semantics" != default_x) {
      "semantics"
    } else if (length(fx) > 1) {
      fx[2]
    } else {
      "__none__"
    }
    saved_x <- as.character(axes$pred_x %||% "")
    selected_x <- if (length(saved_x) > 0 && saved_x[[1]] %in% fx) saved_x[[1]] else default_x
    saved_group <- as.character(axes$pred_group %||% "__none__")
    selected_group <- if (
      length(saved_group) > 0 &&
      saved_group[[1]] %in% c("__none__", fx) &&
      !identical(saved_group[[1]], selected_x)
    ) {
      saved_group[[1]]
    } else if (!identical(default_group, selected_x)) {
      default_group
    } else {
      "__none__"
    }

    tagList(
      selectInput("pred_x", "Prediction x variable", choices = fx, selected = selected_x),
      selectInput(
        "pred_group",
        "Prediction group/color variable (optional)",
        choices = c("None" = "__none__", fx),
        selected = selected_group
      ),
      radioButtons(
        "pred_chart_type",
        "Prediction chart type",
        choices = chart_choices,
        selected = "line",
        inline = TRUE
      ),
      checkboxInput("pred_show_labels", "Show value labels", value = FALSE),
      checkboxInput("pred_show_observed_points", "Show observed datapoints per lemma", value = FALSE),
      checkboxInput("pred_observed_jitter", "Jitter observed datapoints horizontally", value = TRUE),
      checkboxInput("pred_observed_cross", "Show observed datapoints as crosses", value = TRUE),
      checkboxInput("pred_interactive_hover", "Use interactive hover labels", value = FALSE),
      tags$hr(),
      uiOutput("pred_color_controls_ui")
    )
  })

  prediction_level_specs <- reactive({
    res <- fit_result()
    if (is.null(res) || !is.null(res$error)) return(list())
    prediction_level_specs_from_model_df(res$data, res$fixed_effects)
  })

  output$pred_color_controls_ui <- renderUI({
    specs <- prediction_level_specs()
    if (length(specs) == 0) {
      return(tags$small("No categorical fixed effects are available for custom prediction colors."))
    }

    order_state <- normalize_prediction_level_orders(specs, prediction_level_orders_state())
    color_state <- normalize_prediction_level_colors(
      specs,
      merge_prediction_level_color_store(
        prediction_level_color_store_state(),
        prediction_level_colors_state()
      )
    )
    tagList(
      tags$p(
        style = "margin-bottom: 0.5rem;",
        "Drag rows to reorder prediction levels. Colors and order are saved by variable and level name for future app sessions and reused in the main Predictions chart and the individual fixed-effect charts."
      ),
      lapply(names(specs), function(v) {
        levels_v <- order_state[[v]]
        container_id <- prediction_level_order_container_id(v)
        order_input_id <- prediction_level_order_input_id(v)
        tags$details(
          id = paste0(container_id, "_details"),
          style = "margin-bottom: 0.85rem;",
          tags$summary(
            style = "cursor: pointer; font-weight: 600; margin-bottom: 0.35rem;",
            sprintf("%s (%d levels)", v, length(levels_v))
          ),
          tags$div(
            id = container_id,
            `data-order-input` = order_input_id,
            style = "display: flex; flex-direction: column; gap: 0.35rem; margin-top: 0.45rem;",
            lapply(levels_v, function(level) {
              input_id <- prediction_color_input_id(v, level)
              level_color <- unname(color_state[[v]][level])
              tags$div(
                class = "pred-level-row",
                `data-level` = level,
                style = paste(
                  "display: flex; align-items: center; justify-content: space-between;",
                  "gap: 0.75rem; padding: 0.35rem 0.5rem; border: 1px solid #d9d9d9;",
                  "border-radius: 4px; background: #fafafa;"
                ),
                tags$div(
                  style = "display: inline-flex; align-items: center; gap: 0.55rem;",
                  tags$span(
                    class = "pred-level-handle",
                    draggable = "true",
                    style = "cursor: move; color: #666; font-weight: 600; letter-spacing: 0.08em; user-select: none;",
                    "|||"
                  ),
                  tags$span(level)
                ),
                tags$div(
                  style = "display: inline-flex; align-items: center; gap: 0.35rem;",
                  tags$input(
                    id = input_id,
                    type = "color",
                    value = level_color,
                    style = "width: 2.8rem; height: 2rem; padding: 0; border: none; background: transparent;"
                  ),
                  tags$button(
                    type = "button",
                    class = "btn btn-default btn-xs prediction-color-commit",
                    `data-variable` = v,
                    `data-level` = level,
                    "OK"
                  )
                )
              )
            })
          )
        )
      })
    )
  })

  observe({
    specs <- prediction_level_specs()
    if (length(specs) == 0) return(invisible(NULL))
    updated <- normalize_prediction_level_orders(specs, prediction_level_orders_state())
    for (v in names(specs)) {
      order_val <- input[[prediction_level_order_input_id(v)]]
      if (!is.null(order_val) && length(order_val) > 0) {
        updated[[v]] <- resolve_prediction_level_order(specs[[v]], order_val)
      }
    }

    if (!identical(updated, prediction_level_orders_state())) {
      prediction_level_orders_state(updated)
      persist_prediction_level_order_store(updated)
      merge_app_settings(list(pred_level_orders = updated))
    }
  })

  observeEvent(input$prediction_color_commit, {
    specs <- prediction_level_specs()
    color_commit <- input$prediction_color_commit %||% list()
    v <- as.character(color_commit$variable %||% "")
    level <- as.character(color_commit$level %||% "")
    color_val <- as.character(color_commit$color %||% "")
    if (!v %in% names(specs) || !level %in% as.character(specs[[v]]) || !is_valid_hex_color(color_val)) {
      return(invisible(NULL))
    }

    updated <- prediction_level_colors_state()
    if (is.null(updated[[v]])) updated[[v]] <- character(0)
    updated[[v]][level] <- color_val
    updated <- normalize_prediction_level_colors(
      specs,
      merge_prediction_level_color_store(
        prediction_level_color_store_state(),
        updated
      )
    )

    if (!identical(updated, prediction_level_colors_state())) {
      prediction_level_colors_state(updated)
      persist_prediction_level_color_store(updated)
      merge_app_settings(list(pred_level_colors = updated))
    }
  })

  pred_data <- reactive({
    if (is.null(fit_result())) return(NULL)
    res <- fit_result()
    if (!is.null(res$error)) return(NULL)
    x_var <- input$pred_x %||% ""
    group_var <- input$pred_group %||% "__none__"
    response_info <- normalize_response_info(res$response_info %||% attr(res$data, "response_info"))
    compute_predictions(
      res$model,
      res$data,
      x_var = x_var,
      group_var = group_var,
      level_orders = prediction_level_orders_state(),
      include_multinomial_uncertainty = response_is_multinomial(response_info)
    )
  })

  individual_pred_data <- reactive({
    if (is.null(fit_result())) return(NULL)
    res <- fit_result()
    if (!is.null(res$error)) return(NULL)
    response_info <- normalize_response_info(res$response_info %||% attr(res$data, "response_info"))

    pred_list <- lapply(res$fixed_effects, function(v) {
      compute_predictions(
        res$model,
        res$data,
        x_var = v,
        group_var = NULL,
        level_orders = prediction_level_orders_state(),
        include_multinomial_uncertainty = response_is_multinomial(response_info)
      )
    })
    names(pred_list) <- res$fixed_effects
    pred_list[!vapply(pred_list, is.null, logical(1))]
  })

  observeEvent(list(input$pred_x, input$pred_group, input$pred_chart_type, input$pred_show_labels, input$pred_show_observed_points, input$pred_observed_jitter, input$pred_observed_cross, input$pred_interactive_hover), {
    res <- fit_result()
    if (is.null(res) || !is.null(res$error)) return(invisible(NULL))
    if (!isTRUE(state_meta$is_restoring) && !isTRUE(state_meta$is_applying_settings)) {
      axes <- normalize_prediction_axis_store(list(
        pred_x = input$pred_x %||% "",
        pred_group = input$pred_group %||% "__none__"
      ))
      prediction_axis_selection_state(axes)
      tryCatch(
        save_prediction_axis_store(axes),
        error = function(e) {
          showNotification(
            paste("Saving prediction axes failed:", e$message),
            type = "warning",
            duration = 8
          )
        }
      )
      merge_app_settings(list(
        pred_x = axes$pred_x,
        pred_group = axes$pred_group
      ))
    }
    persist_fit_state(res = res)
  }, ignoreInit = TRUE)

  observeEvent(prediction_level_colors_state(), {
    persist_prediction_level_color_store(prediction_level_colors_state())
    res <- fit_result()
    if (is.null(res) || !is.null(res$error) || isTRUE(state_meta$is_restoring)) {
      return(invisible(NULL))
    }
    persist_fit_state(res = res, pred_level_colors = prediction_level_colors_state())
  }, ignoreInit = TRUE)

  observeEvent(prediction_level_orders_state(), {
    persist_prediction_level_order_store(prediction_level_orders_state())
    res <- fit_result()
    if (is.null(res) || !is.null(res$error) || isTRUE(state_meta$is_restoring)) {
      return(invisible(NULL))
    }
    persist_fit_state(res = res, pred_level_orders = prediction_level_orders_state())
  }, ignoreInit = TRUE)

  output$individual_pred_plots_ui <- renderUI({
    if (is.null(fit_result())) {
      return(helpText("Fit a model to generate one prediction chart per fixed effect."))
    }
    res <- fit_result()
    if (!is.null(res$error)) {
      return(helpText("Individual prediction charts are unavailable because model fit failed."))
    }

    pred_list <- individual_pred_data()
    if (is.null(pred_list) || length(pred_list) == 0) {
      return(helpText("No individual fixed-effect prediction charts are available for this model."))
    }

    plot_ids <- paste0("individual_pred_plot_", seq_along(pred_list))
    tagList(
      tags$p("Individual fixed-effect views: each chart varies one fixed effect while holding the other predictors at their reference levels or means. Interaction combinations are not expanded here."),
      lapply(seq_along(pred_list), function(i) {
        pred_i <- pred_list[[i]]
        note <- individual_prediction_color_explanation(
          pred = pred_i,
          response_info = res$response_info %||% attr(res$data, "response_info"),
          show_observed_points = isTRUE(input$pred_show_observed_points)
        )
        tags$div(
          style = "margin-bottom: 18px;",
          tags$strong(names(pred_list)[i]),
          tags$div(
            style = "margin: 4px 0 8px 0; color: #555; font-size: 0.92em;",
            note
          ),
          plotOutput(plot_ids[i], height = 280)
        )
      })
    )
  })

  observe({
    pred_list <- individual_pred_data()
    res <- fit_result()
    if (is.null(pred_list) || is.null(res) || !is.null(res$error)) return(invisible(NULL))

    response_info <- res$response_info %||% attr(res$data, "response_info")
    observed_prediction_data <- attr(res$data, "observed_prediction_data")
    for (i in seq_along(pred_list)) {
      local({
        idx <- i
        x_var <- names(pred_list)[idx]
        plot_id <- paste0("individual_pred_plot_", idx)
        output[[plot_id]] <- renderPlot({
          pred <- individual_pred_data()[[x_var]]
          validate(need(!is.null(pred), paste("Predictions unavailable for", x_var)))
          plot_obj <- build_prediction_plot(
            pred,
            response_info = response_info,
            chart_type = input$pred_chart_type %||% "line",
            level_colors = prediction_level_colors_state(),
            show_labels = input$pred_show_labels %||% FALSE,
            observed_data = observed_prediction_data,
            show_observed_points = input$pred_show_observed_points %||% FALSE,
            observed_point_jitter = input$pred_observed_jitter %||% TRUE,
            observed_point_cross = input$pred_observed_cross %||% TRUE
          ) +
            labs(title = paste("Predictions for", x_var))

          if (isTRUE(individual_prediction_needs_legend(pred, response_info = response_info))) {
            plot_obj <- plot_obj +
              guides(
                color = ggplot2::guide_legend(nrow = 2, byrow = TRUE),
                fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
              ) +
              theme(
                legend.position = "bottom",
                legend.box = "vertical",
                legend.title = element_text(size = 9),
                legend.text = element_text(size = 8)
              )
          } else {
            plot_obj <- plot_obj + theme(legend.position = "none")
          }

          plot_obj
        })
      })
    }
  })

  repro_code <- reactive({
    res <- fit_result()
    if (is.null(res)) {
      return(paste(
        "# Fit a model first, then this tab will show a copy-ready R script.",
        "# The generated script includes data prep, GLMM tests, and chart code.",
        sep = "\n"
      ))
    }
    if (!is.null(res$error)) {
      return(paste(
        "# Model fit failed, so reproducible code is not generated yet.",
        paste0("# Fit error: ", res$error),
        sep = "\n"
      ))
    }

    build_repro_code(
      dataset_path = res$dataset_display_path %||% current_dataset_display_path(),
      dataset_source = res$dataset_source %||% (input$dataset_source %||% "project"),
      response_key = res$response_key %||% (res$response_info %||% list())$response_key %||% "",
      response_reference_level = res$response_reference_level %||% (res$response_info %||% list())$reference_level %||% NULL,
      dataset_filters = dataset_filters_state(),
      fixed_effects = res$fixed_effects,
      interaction_terms = res$interaction_terms,
      random_effects = res$random_effects,
      fit_backend = res$fit_backend %||% "glmer",
      reference_levels = res$reference_levels %||% list(),
      optimizer = res$optimizer %||% "bobyqa",
      maxfun = res$maxfun %||% 200000,
      compute_lrt = isTRUE(res$compute_lrt),
      fastglm_method = res$fastglm_method %||% 3L,
      fastglm_maxit = res$fastglm_maxit %||% 100L,
      multinom_method = res$multinom_method %||% "PQL",
      multinom_catcov = res$multinom_catcov %||% "single",
      multinom_maxit = res$multinom_maxit %||% 25L,
      pred_x = input$pred_x %||% "",
      pred_group = input$pred_group %||% "__none__",
      pred_chart_type = input$pred_chart_type %||% "line",
      pred_show_labels = input$pred_show_labels %||% FALSE,
      pred_show_observed_points = input$pred_show_observed_points %||% FALSE,
      pred_level_orders = prediction_level_orders_state(),
      pred_level_colors = prediction_level_colors_state()
    )
  })

  output$repro_code_ui <- renderUI({
    tags$textarea(
      id = "repro_code_text",
      style = "width: 100%; height: 560px; font-family: monospace; white-space: pre;",
      readonly = "readonly",
      repro_code()
    )
  })

  observeEvent(input$refresh_saved_models, {
    choices <- saved_model_file_choices()
    server_saved_model_choices(choices)
    selected <- input$project_saved_model %||% ""
    if (!selected %in% unname(choices)) selected <- ""
    updateSelectInput(
      session,
      "project_saved_model",
      choices = c("Select a saved model" = "", choices),
      selected = selected
    )
    showNotification(
      sprintf("Found %d loadable saved model%s.", length(choices), if (length(choices) == 1L) "" else "s"),
      type = "message"
    )
  }, ignoreInit = TRUE)

  observeEvent(input$project_saved_model, {
    path <- input$project_saved_model %||% ""
    req(nzchar(path))
    choices <- server_saved_model_choices()
    if (!path %in% unname(choices) || !file.exists(path)) {
      showNotification("The selected server model is no longer available. Refresh the saved-model list.", type = "error", duration = 10)
      return(invisible(NULL))
    }

    state <- tryCatch(
      readRDS(path),
      error = function(e) {
        showNotification(paste("Could not read saved model:", e$message), type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(state)) return(invisible(NULL))

    tryCatch(
      restore_saved_fit(state, source_label = basename(path)),
      error = function(e) {
        showNotification(paste("Could not load saved model:", e$message), type = "error", duration = 10)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$load_model, {
    req(input$load_model$datapath)
    state <- tryCatch(
      readRDS(input$load_model$datapath),
      error = function(e) {
        showNotification(paste("Could not read saved model:", e$message), type = "error", duration = 10)
        NULL
      }
    )
    if (is.null(state)) return(invisible(NULL))

    tryCatch(
      restore_saved_fit(state, source_label = input$load_model$name %||% "saved model"),
      error = function(e) {
        showNotification(paste("Could not load saved model:", e$message), type = "error", duration = 10)
      }
    )
  }, ignoreInit = TRUE)

  observeEvent(input$copy_repro_code, {
    session$sendCustomMessage("copyReproCode", list())
    showNotification("Code copied to clipboard.", type = "message")
  }, ignoreInit = TRUE)

  output$save_model <- downloadHandler(
    filename = function() {
      res <- fit_result()
      base <- sanitize_filename((res %||% list())$dataset_display_path %||% "glmm_model")
      paste0(base, "_fit_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
    },
    content = function(file) {
      res <- fit_result()
      if (is.null(res) || !is.null(res$error)) {
        stop("Fit a model successfully before saving it.")
      }
      state <- build_saved_fit_state(
        res,
        fit_meta,
        pred_x = input$pred_x %||% "",
        pred_group = input$pred_group %||% "__none__",
        pred_chart_type = input$pred_chart_type %||% "line",
        pred_show_labels = input$pred_show_labels %||% FALSE,
        pred_show_observed_points = input$pred_show_observed_points %||% FALSE,
        pred_observed_jitter = input$pred_observed_jitter %||% TRUE,
        pred_observed_cross = input$pred_observed_cross %||% TRUE,
        pred_interactive_hover = input$pred_interactive_hover %||% FALSE,
        pred_level_orders = prediction_level_orders_state(),
        pred_level_colors = prediction_level_colors_state(),
        dataset_filters = dataset_filters_state(),
        include_raw_data = TRUE
      )
      saveRDS(state, file)
    }
  )

  output$download_repro_code <- downloadHandler(
    filename = function() paste0("glmm_reproducible_code_", format(Sys.Date(), "%Y%m%d"), ".R"),
    content = function(file) {
      writeLines(repro_code(), con = file, useBytes = TRUE)
    }
  )

  output$download_coef_plot_svg <- downloadHandler(
    filename = function() {
      paste0("glmm_fixed_effects_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".svg")
    },
    content = function(file) {
      res <- fit_result()
      if (is.null(res) || !is.null(res$error)) {
        stop("Fit a model successfully before downloading the fixed-effects plot.")
      }
      save_plot_svg(coefficient_plot(), file, width = 9, height = 5.5)
    }
  )

  output$download_pred_plot <- downloadHandler(
    filename = function() {
      paste0("glmm_prediction_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      res <- fit_result()
      if (is.null(res) || !is.null(res$error)) {
        stop("Fit a model successfully before downloading the prediction plot.")
      }

      ggplot2::ggsave(
        filename = file,
        plot = main_prediction_plot(),
        device = "png",
        width = 9,
        height = 5.5,
        units = "in",
        dpi = 300
      )
    }
  )

  output$download_pred_plot_svg <- downloadHandler(
    filename = function() {
      paste0("glmm_prediction_plot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".svg")
    },
    content = function(file) {
      res <- fit_result()
      if (is.null(res) || !is.null(res$error)) {
        stop("Fit a model successfully before downloading the prediction plot.")
      }

      save_plot_svg(main_prediction_plot(), file, width = 9, height = 5.5)
    }
  )

  main_prediction_plot <- function() {
    if (is.null(fit_result())) {
      validate(need(FALSE, "No model yet. Click 'Fit / Refit model'."))
    }
    res <- fit_result()
    validate(need(!is.null(res), "Model is currently fitting..."))
    validate(need(is.null(res$error), res$error))
    pred <- pred_data()
    validate(need(!is.null(pred), "Predictions unavailable for selected variables."))
    build_prediction_plot(
      pred,
      response_info = res$response_info %||% attr(res$data, "response_info"),
      chart_type = input$pred_chart_type %||% "line",
      level_colors = prediction_level_colors_state(),
      show_labels = input$pred_show_labels %||% FALSE,
      observed_data = attr(res$data, "observed_prediction_data"),
      show_observed_points = input$pred_show_observed_points %||% FALSE,
      observed_point_jitter = input$pred_observed_jitter %||% TRUE,
      observed_point_cross = input$pred_observed_cross %||% TRUE
    )
  }

  output$pred_plot_ui <- renderUI({
    if (isTRUE(input$pred_interactive_hover)) {
      if (!requireNamespace("plotly", quietly = TRUE)) {
        return(helpText("Install the `plotly` package to use interactive hover labels."))
      }
      plotly::plotlyOutput("pred_plot_interactive", height = 420)
    } else {
      plotOutput("pred_plot_static", height = 420)
    }
  })

  output$pred_plot_static <- renderPlot({
    main_prediction_plot()
  })

  if (requireNamespace("plotly", quietly = TRUE)) {
    output$pred_plot_interactive <- plotly::renderPlotly({
      interactive_plot <- plotly::ggplotly(main_prediction_plot(), tooltip = "text")
      interactive_plot <- plotly::layout(interactive_plot, hovermode = "closest")
      plotly::config(interactive_plot, displaylogo = FALSE)
    })
  }

  output$pred_table <- renderDT({
    if (is.null(fit_result())) {
      return(datatable(data.frame(info = "No model yet. Click 'Fit / Refit model'."), options = list(dom = "t")))
    }
    res <- fit_result()
    if (is.null(res)) {
      return(datatable(data.frame(info = "Model is currently fitting..."), options = list(dom = "t")))
    }
    validate(need(is.null(res$error), res$error))
    pred <- pred_data()
    validate(need(!is.null(pred), "No prediction table available for selected variables."))
    datatable(pred, options = list(pageLength = 12, scrollX = TRUE))
  })
}

ui <- fluidPage(
  tabsetPanel(
    id = "application_tab",
    selected = "Model Explorer",
    tabPanel("Model Explorer", explorer_ui),
    tabPanel("Combined Visualisations", combined_visualisations_ui)
  )
)

server <- function(input, output, session) {
  explorer_server(input, output, session)
  combined_visualisations_server(input, output, session)
}

shinyApp(ui, server)
