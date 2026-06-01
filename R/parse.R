extract_numbers <- function(text) {
  tokens <- regmatches(
    text,
    gregexpr("[+-]?(?:[0-9]+\\.[0-9]+|[0-9]+|\\.[0-9]+)", text, perl = TRUE)
  )[[1]]

  if (length(tokens) == 1 && identical(tokens, "")) {
    return(numeric(0))
  }

  as.numeric(tokens)
}

split_pipe_fields <- function(line) {
  fields <- trimws(strsplit(line, "|", fixed = TRUE)[[1]])

  if (length(fields) > 0 && identical(fields[1], "")) {
    fields <- fields[-1]
  }

  if (length(fields) > 0 && identical(fields[length(fields)], "")) {
    fields <- fields[-length(fields)]
  }

  fields
}

parse_id_label <- function(text) {
  parts <- strsplit(trimws(text), "\\s+", perl = TRUE)[[1]]
  id_num <- suppressWarnings(as.integer(parts[1]))
  id_label <- paste(parts[-1], collapse = " ")

  list(id = id_num, label = id_label)
}

empty_measurement_rows <- function(facet_name = NA_character_) {
  tibble::tibble(
    facet = character(),
    id = integer(),
    label = character(),
    raw_score = numeric(),
    count = numeric(),
    observed_avg = numeric(),
    fair_avg = numeric(),
    measure = numeric(),
    se = numeric(),
    infit_mnsq = numeric(),
    infit_zstd = numeric(),
    outfit_mnsq = numeric(),
    outfit_zstd = numeric(),
    discrimination = numeric(),
    ptmea = numeric(),
    ptexp = numeric(),
    subset = numeric(),
    is_extreme = logical(),
    extreme_type = character()
  )
}


empty_category_rows <- function() {
  tibble::tibble(
    category = numeric(),
    total_count = numeric(),
    used_count = numeric(),
    pct = numeric(),
    cumulative_pct = numeric(),
    avg_measure = numeric(),
    expected_measure = numeric(),
    outfit_mnsq = numeric(),
    threshold = numeric(),
    threshold_se = numeric(),
    category_at_measure = numeric(),
    category_at_half = numeric(),
    probable_from = character(),
    thurstone_threshold = numeric(),
    peak_probability = numeric(),
    observed_expected_residual = numeric(),
    andrich_threshold_displacement = numeric()
  )
}

list_measurement_tables <- function(lines) {
  matches <- grep("^Table 7\\.[0-9]+\\.1\\s+.+Measurement Report", lines, perl = TRUE, value = TRUE)

  if (!length(matches)) {
    return(data.frame(table_id = character(), facet_name = character(), pattern = character(), stringsAsFactors = FALSE))
  }

  table_id <- sub("^Table\\s+([0-9]+\\.[0-9]+\\.[0-9]+).*$", "\\1", matches, perl = TRUE)
  facet_name <- trimws(sub("^Table\\s+[0-9]+\\.[0-9]+\\.[0-9]+\\s+(.+?)\\s+Measurement Report.*$", "\\1", matches, perl = TRUE))

  data.frame(
    table_id = table_id,
    facet_name = facet_name,
    pattern = paste0("^Table\\s+", gsub("\\.", "\\\\.", table_id), "\\b"),
    stringsAsFactors = FALSE
  )
}


find_table_start <- function(lines, pattern) {
  start <- grep(pattern, lines, perl = TRUE)[1]
  if (is.na(start)) {
    stop("Could not find table matching pattern: ", pattern, call. = FALSE)
  }
  start
}

extract_table_section <- function(lines, table_pattern, lookahead = 400) {
  table_start <- find_table_start(lines, table_pattern)
  later_lines <- lines[(table_start + 1):length(lines)]
  next_table <- grep("^Table [0-9]", later_lines, perl = TRUE)[1]

  if (is.na(next_table)) {
    table_end <- min(length(lines), table_start + lookahead)
  } else {
    table_end <- min(table_start + next_table - 2, table_start + lookahead)
  }

  lines[table_start:table_end]
}

parse_measurement_rows <- function(lines, table_pattern, facet_name) {
  if (!any(grepl(table_pattern, lines, perl = TRUE))) {
    return(empty_measurement_rows(facet_name))
  }

  section <- extract_table_section(lines, table_pattern, lookahead = 400)
  rows <- section[grepl("^\\|", section)]

  out <- lapply(rows, function(row) {
    fields <- split_pipe_fields(row)

    if (length(fields) < 5) {
      return(NULL)
    }

    if (length(fields) == 5) {
      score_measure_tokens <- extract_numbers(gsub("[()]", "", fields[1]))
      score_tokens <- score_measure_tokens[seq_len(min(4, length(score_measure_tokens)))]
      measure_tokens <- if (length(score_measure_tokens) >= 5) score_measure_tokens[5:min(6, length(score_measure_tokens))] else numeric(0)
      fit_tokens <- extract_numbers(fields[2])
      discrim_tokens <- extract_numbers(fields[3])
      corr_tokens <- extract_numbers(fields[4])
      entity_field <- fields[5]
      subset_tokens <- numeric(0)
    } else {
      has_subset <- grepl("subset", fields[length(fields)], ignore.case = TRUE)
      entity_field <- if (has_subset && length(fields) >= 7) fields[length(fields) - 1] else fields[length(fields)]

      score_tokens <- extract_numbers(fields[1])
      measure_tokens <- extract_numbers(gsub("[()]", "", fields[2]))
      fit_tokens <- extract_numbers(fields[3])
      discrim_tokens <- extract_numbers(fields[4])
      corr_tokens <- extract_numbers(fields[5])
      subset_tokens <- if (has_subset) extract_numbers(fields[length(fields)]) else numeric(0)
    }

    if (!grepl("^[0-9]+\\s+", entity_field, perl = TRUE)) {
      return(NULL)
    }

    entity <- parse_id_label(entity_field)
    row_text <- paste(fields, collapse = " ")
    extreme_type <- if (grepl("\\bMaximum\\b", row_text)) {
      "Maximum"
    } else if (grepl("\\bMinimum\\b", row_text)) {
      "Minimum"
    } else {
      NA_character_
    }

    data.frame(
      facet = facet_name,
      id = entity$id,
      label = entity$label,
      raw_score = if (length(score_tokens) >= 1) score_tokens[1] else NA_real_,
      count = if (length(score_tokens) >= 2) score_tokens[2] else NA_real_,
      observed_avg = if (length(score_tokens) >= 3) score_tokens[3] else NA_real_,
      fair_avg = if (length(score_tokens) >= 4) score_tokens[4] else NA_real_,
      measure = if (length(measure_tokens) >= 1) measure_tokens[1] else NA_real_,
      se = if (length(measure_tokens) >= 2) measure_tokens[2] else NA_real_,
      infit_mnsq = if (length(fit_tokens) >= 1) fit_tokens[1] else NA_real_,
      infit_zstd = if (length(fit_tokens) >= 2) fit_tokens[2] else NA_real_,
      outfit_mnsq = if (length(fit_tokens) >= 3) fit_tokens[3] else NA_real_,
      outfit_zstd = if (length(fit_tokens) >= 4) fit_tokens[4] else NA_real_,
      discrimination = if (length(discrim_tokens) >= 1) discrim_tokens[1] else NA_real_,
      ptmea = if (length(corr_tokens) >= 1) corr_tokens[1] else NA_real_,
      ptexp = if (length(corr_tokens) >= 2) corr_tokens[2] else NA_real_,
      subset = if (length(subset_tokens) >= 1) subset_tokens[1] else NA_real_,
      is_extreme = !is.na(extreme_type),
      extreme_type = extreme_type,
      stringsAsFactors = FALSE
    )
  })

  out <- Filter(Negate(is.null), out)

  if (!length(out)) {
    return(empty_measurement_rows(facet_name))
  }

  tibble::as_tibble(do.call(rbind, out))
}


parse_category_rows <- function(lines) {
  if (!any(grepl("Table 8\\.1", lines, perl = TRUE))) {
    return(empty_category_rows())
  }

  section <- extract_table_section(lines, "Table 8\\.1", lookahead = 120)
  pipe_rows <- section[grepl("^\\|\\s*[0-9]", section)]

  out <- lapply(pipe_rows, function(row) {
    fields <- split_pipe_fields(row)
    if (length(fields) < 3) {
      return(NULL)
    }

    count_tokens <- extract_numbers(fields[1])
    quality_tokens <- if (length(fields) >= 2) extract_numbers(fields[2]) else numeric(0)
    threshold_tokens <- if (length(fields) >= 3) extract_numbers(gsub("[()]", "", fields[3])) else numeric(0)
    expectation_tokens <- if (length(fields) >= 4) extract_numbers(gsub("[()]", "", fields[4])) else numeric(0)
    thurstone_tokens <- if (length(fields) >= 6) extract_numbers(fields[6]) else numeric(0)
    peak_tokens <- if (length(fields) >= 7) extract_numbers(fields[7]) else numeric(0)
    diagnostic_tokens <- if (length(fields) >= 8) extract_numbers(fields[8]) else numeric(0)

    if (!length(count_tokens)) {
      return(NULL)
    }

    data.frame(
      category = if (length(count_tokens) >= 1) count_tokens[1] else NA_real_,
      total_count = if (length(count_tokens) >= 2) count_tokens[2] else NA_real_,
      used_count = if (length(count_tokens) >= 3) count_tokens[3] else NA_real_,
      pct = if (length(count_tokens) >= 4) count_tokens[4] / 100 else NA_real_,
      cumulative_pct = if (length(count_tokens) >= 5) count_tokens[5] / 100 else NA_real_,
      avg_measure = if (length(quality_tokens) >= 1) quality_tokens[1] else NA_real_,
      expected_measure = if (length(quality_tokens) >= 2) quality_tokens[2] else NA_real_,
      outfit_mnsq = if (length(quality_tokens) >= 3) quality_tokens[3] else NA_real_,
      threshold = if (length(threshold_tokens) >= 1) threshold_tokens[1] else NA_real_,
      threshold_se = if (length(threshold_tokens) >= 2) threshold_tokens[2] else NA_real_,
      category_at_measure = if (length(expectation_tokens) >= 1) expectation_tokens[1] else NA_real_,
      category_at_half = if (length(expectation_tokens) >= 2) expectation_tokens[2] else NA_real_,
      probable_from = if (length(fields) >= 5 && nzchar(trimws(fields[5]))) trimws(fields[5]) else NA_character_,
      thurstone_threshold = if (length(thurstone_tokens) >= 1) thurstone_tokens[1] else NA_real_,
      peak_probability = if (length(peak_tokens) >= 1) if (peak_tokens[1] > 1) peak_tokens[1] / 100 else peak_tokens[1] else NA_real_,
      observed_expected_residual = if (length(diagnostic_tokens) >= 1) diagnostic_tokens[1] else NA_real_,
      andrich_threshold_displacement = if (length(diagnostic_tokens) >= 2) diagnostic_tokens[2] else NA_real_,
      stringsAsFactors = FALSE
    )
  })

  out <- Filter(Negate(is.null), out)

  if (!length(out)) {
    return(empty_category_rows())
  }

  tibble::as_tibble(do.call(rbind, out))
}

parse_unexpected_rows <- function(lines, num_facets) {
  table_match <- grep("Table 4\\.1 Unexpected Responses", lines, perl = TRUE)[1]
  if (is.na(table_match)) {
    return(tibble::tibble())
  }

  section <- extract_table_section(lines, "Table 4\\.1 Unexpected Responses", lookahead = 250)
  pipe_rows <- section[grepl("^\\|\\s*[0-9]", section)]

  score_regex <- "\\s*([0-9]+)\\s+([0-9]+)\\s+([+-]?(?:[0-9]+\\.[0-9]+|[0-9]+|\\.[0-9]+))\\s+([+-]?(?:[0-9]+\\.[0-9]+|[0-9]+|\\.[0-9]+))\\s+([+-]?(?:[0-9]+\\.[0-9]+|[0-9]+|\\.[0-9]+))\\s*"
  
  facet_regex <- ""
  if (num_facets > 0) {
    if (num_facets > 1) {
      facet_regex <- paste0(rep("\\s+([0-9]+)\\s+(\\S+)", num_facets - 1), collapse = "")
    }
    facet_regex <- paste0(facet_regex, "\\s+([0-9]+)\\s+(.+?)")
  }
  
  full_regex <- paste0("^\\|", score_regex, "\\|", facet_regex, "\\s*\\|\\s*([0-9]+)\\s*$")

  out <- lapply(pipe_rows, function(row) {
    match <- regexec(full_regex, row, perl = TRUE)
    pieces <- regmatches(row, match)[[1]]

    if (length(pieces) == 0) {
      return(NULL)
    }

    df <- data.frame(
      category = as.numeric(pieces[2]),
      score = as.numeric(pieces[3]),
      expected = as.numeric(pieces[4]),
      residual = as.numeric(pieces[5]),
      standardized_residual = as.numeric(pieces[6]),
      stringsAsFactors = FALSE
    )
    
    if (num_facets > 0) {
      for (i in seq_len(num_facets)) {
        df[[paste0("facet", i, "_id")]] <- as.integer(pieces[6 + (i * 2) - 1])
        df[[paste0("facet", i, "_label")]] <- trimws(pieces[6 + (i * 2)])
      }
    }
    
    df$sequence <- as.numeric(pieces[length(pieces)])
    df
  })

  tibble::as_tibble(do.call(rbind, Filter(Negate(is.null), out)))
}

#' Read a FACETS output file
#'
#' Reads a FACETS `.out` file and parses the main FACETS tables used by the
#' package.
#'
#' @param path Path to a FACETS `.out` file.
#' @param encoding Text encoding used when reading the file.
#'
#' @return An object of class `facets_out`.
read_facets <- function(path, encoding = "UTF-8") {
  lines <- readLines(path, warn = FALSE, encoding = encoding)
  title_line <- grep("^Title\\s*=", lines, value = TRUE)[1]
  model_line <- grep("^Model\\s*=", lines, value = TRUE)[1]
  measurement_tables <- list_measurement_tables(lines)
  measurement_reports <- lapply(seq_len(nrow(measurement_tables)), function(i) {
    parse_measurement_rows(lines, measurement_tables$pattern[i], measurement_tables$facet_name[i])
  })

  if (length(measurement_reports)) {
    names(measurement_reports) <- measurement_tables$facet_name
  }

  structure(
    list(
      path = normalizePath(path, winslash = "/", mustWork = FALSE),
      title = if (!is.na(title_line)) trimws(sub("^Title\\s*=", "", title_line)) else NA_character_,
      model = if (!is.na(model_line)) trimws(sub("^Model\\s*=", "", model_line)) else NA_character_,
      measurement_facet_names = measurement_tables$facet_name,
      measurement_reports = measurement_reports,
      categories = parse_category_rows(lines),
      unexpected = parse_unexpected_rows(lines, nrow(measurement_tables))
    ),
    class = "facets_out"
  )
}

#' @export
print.facets_out <- function(x, ...) {
  summary_df <- data.frame(
    field = c("Title", "Path", "Measurement reports", "Categories", "Unexpected responses"),
    value = c(
      x$title,
      x$path,
      length(x$measurement_reports),
      nrow(x$categories),
      nrow(x$unexpected)
    ),
    stringsAsFactors = FALSE
  )

  print(summary_df, row.names = FALSE)
  invisible(x)
}
