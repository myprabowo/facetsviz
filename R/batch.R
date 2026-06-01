sanitize_output_key <- function(path) {
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  pieces <- strsplit(normalized, "/", fixed = TRUE)[[1]]
  tail_parts <- utils::tail(pieces, min(2L, length(pieces)))
  raw_key <- paste(tail_parts, collapse = "_")
  raw_key <- tools::file_path_sans_ext(raw_key)
  gsub("[^A-Za-z0-9._-]", "_", raw_key)
}

collect_out_files <- function(paths, recursive = TRUE) {
  collected <- character(0)

  for (path in paths) {
    expanded <- path.expand(path)

    if (dir.exists(expanded)) {
      collected <- c(
        collected,
        list.files(expanded, pattern = "\\.(out|txt)$", ignore.case = TRUE, recursive = recursive, full.names = TRUE)
      )
    } else if (file.exists(expanded)) {
      collected <- c(collected, expanded)
    } else {
      warning("Path not found and skipped: ", path, call. = FALSE)
    }
  }

  if (!length(collected)) {
    return(character(0))
  }

  unique(normalizePath(collected, winslash = "/", mustWork = FALSE))
}

#' Render FACETS plots for multiple output files
#'
#' @param paths One or more FACETS `.out` or `.txt` files or directories containing them.
#' @param output_root Root directory for rendered plot folders.
#' @param recursive Whether to search directories recursively.
#' @param include Plot names passed to [render_plots()].
#' @param formats File formats passed to [render_plots()].
#' @param dpi Image resolution.
#' @param top_n_unexpected Number of unexpected responses to show.
#'
#' @return A tibble describing rendered outputs.
render_batch <- function(
  paths = ".",
  output_root = "batch_figures",
  recursive = TRUE,
  include = c(
    "wright_map",
    "observed_vs_fair",
    "measure_distribution",
    "estimates",
    "category_usage",
    "probability_curves",
    "expected_score_ogive",
    "scale_structure",
    "unexpected_responses"
  ),
  formats = c("png", "tiff"),
  dpi = 600,
  top_n_unexpected = 20L
) {
  files <- collect_out_files(paths, recursive = recursive)

  if (!length(files)) {
    stop("No FACETS .out or .txt files were found for batch rendering.", call. = FALSE)
  }

  dir.create(output_root, showWarnings = FALSE, recursive = TRUE)

  results <- lapply(files, function(file) {
    key <- sanitize_output_key(file)
    output_dir <- file.path(output_root, key)

    render_plots(
      file,
      output_dir = output_dir,
      include = include,
      formats = formats,
      dpi = dpi,
      top_n_unexpected = top_n_unexpected
    )

    data.frame(
      input_file = normalizePath(file, winslash = "/", mustWork = FALSE),
      output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
      rendered_plots = length(include) * length(formats),
      stringsAsFactors = FALSE
    )
  })

  tibble::as_tibble(do.call(rbind, results))
}
