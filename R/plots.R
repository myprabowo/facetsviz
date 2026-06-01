if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".data",
    "Category",
    "Probability",
    "case",
    "category",
    "direction",
    "display_label",
    "expected",
    "facet",
    "fair_avg",
    "is_extreme",
    "label",
    "label_x",
    "label_y",
    "measure",
    "observed_avg",
    "pct_label",
    "score",
    "score_type",
    "se",
    "standardized_residual",
    "stat",
    "theta",
    "used_count",
    "value",
    "x",
    "x_end",
    "x_start",
    "y"
  ))
}


.data <- NULL
Category <- Probability <- case <- category <- direction <- display_label <- expected <- NULL
facet <- fair_avg <- is_extreme <- label <- measure <- observed_avg <- pct_label <- NULL
label_x <- label_y <- NULL
score <- score_type <- se <- standardized_residual <- stat <- theta <- used_count <- NULL
value <- x <- x_end <- x_start <- y <- NULL

as_facets_out <- function(x) {
  if (inherits(x, "facets_out")) {
    return(x)
  }

  if (is.character(x) && length(x) == 1L) {
    return(get("read_facets", mode = "function", inherits = TRUE)(x))
  }

  stop("Expected a facets_out object or a path to a FACETS .out file.", call. = FALSE)
}

wrap_text <- function(text, width = 16) {
  vapply(
    text,
    function(value) paste(strwrap(value, width = width), collapse = "\n"),
    character(1)
  )
}

geom_facets_label_repel <- function(mapping = NULL, data = NULL, ..., fill = "white", label.size = 0.2, color = "#404040", size = 3) {
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package 'ggrepel' is required for label rendering. Install it with install.packages('ggrepel').", call. = FALSE)
  }

  ggrepel::geom_label_repel(
    mapping = mapping,
    data = data,
    ...,
    fill = fill,
    label.size = label.size,
    color = color,
    size = size,
    box.padding = 0.28,
    point.padding = 0.14,
    label.padding = grid::unit(0.13, "lines"),
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = 123,
    segment.color = "#A8A8A8",
    segment.size = 0.35
  )
}

category_palette <- function(k) {
  base <- c("#2F5E78", "#52916B", "#D39A2C", "#CC6545", "#8F4F6F", "#6A5ACD")
  if (k <= length(base)) {
    return(stats::setNames(base[seq_len(k)], as.character(seq_len(k))))
  }

  stats::setNames(scales::hue_pal()(k), as.character(seq_len(k)))
}

default_plot_specs <- function() {
  list(
    wright_map = c(8.2, 6.2),
    observed_vs_fair = c(7.2, 4.8),
    measure_distribution = c(7.2, 4.8),
    estimates = c(7.2, 5.2),
    category_usage = c(7.2, 4.8),
    probability_curves = c(7.8, 5.4),
    expected_score_ogive = c(7.8, 5.0),
    scale_structure = c(8.0, 4.3),
    unexpected_responses = c(8.4, 6.0)
  )
}

save_plot_file <- function(plot, filename, width, height, dpi) {
  ext <- tolower(sub("^.*\\.", "", filename))
  args <- list(filename = filename, plot = plot, width = width, height = height, dpi = dpi, bg = "white")

  if (identical(ext, "tiff")) {
    args$compression <- "lzw"
  }

  do.call(ggplot2::ggsave, args)
}

#' Facets plotting theme
#'
#' @param base_size Base text size.
#' @param base_family Base font family.
#'
#' @return A ggplot2 theme object.
facets_theme <- function(base_size = 11, base_family = "Helvetica") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#E7E7E7", linewidth = 0.32),
      panel.grid.major.x = ggplot2::element_line(color = "#E0E5E9", linewidth = 0.35),
      axis.title = ggplot2::element_text(face = "bold", color = "#202020"),
      axis.text = ggplot2::element_text(color = "#303030"),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(color = "#202020"),
      plot.title = ggplot2::element_text(face = "bold", color = "#202020"),
      plot.subtitle = ggplot2::element_text(color = "#4A4A4A"),
      plot.margin = ggplot2::margin(12, 18, 12, 12)
    )
}

measurement_reports_list <- function(facets) {
  reports <- facets$measurement_reports

  if (is.null(reports)) {
    reports <- list()
  }

  reports
}

measurement_facet_name <- function(facets, position, fallback = NULL) {
  reports <- measurement_reports_list(facets)
  names_vec <- names(reports)

  if (length(names_vec) >= position && nzchar(names_vec[position])) {
    return(names_vec[position])
  }

  if (!is.null(fallback)) {
    return(fallback)
  }

  paste("Facet", position)
}

has_measurement_report <- function(facets, position, required_cols = c("measure")) {
  reports <- measurement_reports_list(facets)

  if (length(reports) < position) {
    return(FALSE)
  }

  df <- reports[[position]]

  if (!is.data.frame(df) || !nrow(df)) {
    return(FALSE)
  }

  if (!length(required_cols)) {
    return(TRUE)
  }

  if (!all(required_cols %in% names(df))) {
    return(FALSE)
  }

  any(stats::complete.cases(df[, required_cols, drop = FALSE]))
}

measurement_report_at <- function(facets, position, required_cols = c("measure"), fallback = NULL, purpose = "plotting") {
  if (!has_measurement_report(facets, position, required_cols = required_cols)) {
    facet_name <- measurement_facet_name(facets, position, fallback = fallback)
    stop("Required data for ", facet_name, " are not available for ", purpose, ".", call. = FALSE)
  }

  reports <- measurement_reports_list(facets)

  list(
    data = reports[[position]],
    facet_name = measurement_facet_name(facets, position, fallback = fallback)
  )
}

criteria_plot_data <- function(facets) {
  if (nrow(facets$criteria)) {
    return(list(data = facets$criteria, facet_name = measurement_facet_name(facets, 3, fallback = "Criteria")))
  }

  if (has_measurement_report(facets, 3, required_cols = c("measure"))) {
    report <- measurement_report_at(facets, 3, required_cols = c("measure"), fallback = "Criteria", purpose = "criterion plotting")

    return(list(
      data = tibble::tibble(
        id = report$data$id,
        criterion = report$data$label,
        total_score = report$data$raw_score,
        count = report$data$count,
        observed_avg = report$data$observed_avg,
        fair_avg = report$data$fair_avg,
        measure = report$data$measure,
        se = report$data$se,
        infit_mnsq = report$data$infit_mnsq,
        infit_zstd = report$data$infit_zstd,
        outfit_mnsq = report$data$outfit_mnsq,
        outfit_zstd = report$data$outfit_zstd,
        discrimination = report$data$discrimination,
        ptmea = report$data$ptmea,
        ptexp = report$data$ptexp
      ),
      facet_name = report$facet_name
    ))
  }

  stop("No criterion-style measurement data are available for plotting.", call. = FALSE)
}

available_plot_names <- function(facets) {
  availability <- c(
    wright_map = any(vapply(measurement_reports_list(facets), nrow, integer(1)) > 0),
    observed_vs_fair = has_measurement_report(facets, 1, required_cols = c("measure", "observed_avg", "fair_avg")),
    measure_distribution = has_measurement_report(facets, 2, required_cols = c("measure")),
    estimates = has_measurement_report(facets, 3, required_cols = c("measure")),
    category_usage = nrow(facets$categories) > 0 && any(!is.na(facets$categories$used_count)),
    probability_curves = any(!is.na(facets$categories$threshold)),
    expected_score_ogive = any(!is.na(facets$categories$threshold)),
    scale_structure = any(!is.na(facets$categories$threshold)),
    unexpected_responses = nrow(facets$unexpected) > 0
  )

  names(availability)[availability]
}

plot_choice_labels <- function(facets = NULL) {
  labels <- c(
    wright_map = "Measurement map",
    observed_vs_fair = "Observed vs fair averages",
    measure_distribution = "Measure distribution",
    estimates = "Difficulty estimates",
    category_usage = "Category usage",
    probability_curves = "Probability curves",
    expected_score_ogive = "Expected score ogive",
    scale_structure = "Scale structure",
    unexpected_responses = "Unexpected responses"
  )

  if (is.null(facets)) {
    return(stats::setNames(names(labels), labels))
  }

  if (has_measurement_report(facets, 1, required_cols = c("measure", "observed_avg", "fair_avg"))) {
    labels["observed_vs_fair"] <- paste0("Observed vs fair averages (report 1: ", measurement_facet_name(facets, 1, fallback = "Facet 1"), ")")
  }

  if (has_measurement_report(facets, 2, required_cols = c("measure"))) {
    second_name <- measurement_facet_name(facets, 2, fallback = "Facet 2")
    labels["measure_distribution"] <- paste0("Measure distribution (report 2: ", second_name, ")")
  }

  if (has_measurement_report(facets, 3, required_cols = c("measure"))) {
    labels["estimates"] <- paste0("Difficulty estimates (report 3: ", measurement_facet_name(facets, 3, fallback = "Facet 3"), ")")
  }

  available <- available_plot_names(facets)
  stats::setNames(available, labels[available])
}

available_table_choices <- function(facets = NULL) {
  if (is.null(facets)) {
    return(c(
      "Measurement report 1" = "measurement_1",
      "Measurement report 2" = "measurement_2",
      "Measurement report 3" = "measurement_3",
      "Categories" = "categories",
      "Unexpected responses" = "unexpected"
    ))
  }

  choices <- character(0)

  reports <- measurement_reports_list(facets)
  for (i in seq_along(reports)) {
    if (nrow(reports[[i]]) > 0) {
      choices[paste0("measurement_", i)] <- paste0("Measurement report ", i, ": ", measurement_facet_name(facets, i, fallback = paste("Facet", i)))
    }
  }

  if (has_measurement_report(facets, 3)) {
    choices["criteria"] <- paste0("Summary view: ", measurement_facet_name(facets, 3, fallback = "Facet 3"))
  }

  if (nrow(facets$categories) > 0) {
    choices["categories"] <- "Categories"
  }

  if (nrow(facets$unexpected) > 0) {
    choices["unexpected"] <- "Unexpected responses"
  }

  stats::setNames(names(choices), unname(choices))
}

measure_range_text <- function(df) {
  if (!is.data.frame(df) || !nrow(df) || !"measure" %in% names(df) || !any(!is.na(df$measure))) {
    return("Not available")
  }

  sprintf("%.2f to %.2f", min(df$measure, na.rm = TRUE), max(df$measure, na.rm = TRUE))
}

select_representative_rows <- function(df, n = 8L) {
  required <- stats::complete.cases(df[, c("measure", "observed_avg", "fair_avg")])
  usable <- df[required, , drop = FALSE]

  if (!nrow(usable)) {
    stop("No complete rows are available for the observed-vs-fair plot.", call. = FALSE)
  }

  n <- min(as.integer(n), nrow(usable))
  probs <- seq(0.05, 0.95, length.out = n)
  targets <- as.numeric(stats::quantile(usable$measure, probs = probs, na.rm = TRUE))
  chosen_ids <- integer(0)

  for (target in targets) {
    order_idx <- order(abs(usable$measure - target), -abs(usable$observed_avg - usable$fair_avg), usable$id)
    candidate_ids <- usable$id[order_idx]
    candidate_ids <- candidate_ids[!(candidate_ids %in% chosen_ids)]

    if (length(candidate_ids)) {
      chosen_ids <- c(chosen_ids, candidate_ids[1])
    }
  }

  selected <- usable[match(unique(chosen_ids), usable$id), , drop = FALSE]
  selected <- selected[order(selected$measure), , drop = FALSE]
  selected$display_label <- ifelse(nzchar(selected$label), selected$label, sprintf("ID %s", selected$id))
  selected
}

prepare_rating_scale_data <- function(x, theta = NULL, step = 0.01, padding = 3) {
  facets <- as_facets_out(x)
  thresholds <- facets$categories$threshold[!is.na(facets$categories$threshold)]

  if (!length(thresholds)) {
    stop("No Rasch-Andrich thresholds were parsed from Table 8.1.", call. = FALSE)
  }

  if (is.null(theta)) {
    theta_min <- floor(min(thresholds, na.rm = TRUE)) - padding
    theta_max <- ceiling(max(thresholds, na.rm = TRUE)) + padding
    theta <- seq(theta_min, theta_max, by = step)
  } else {
    theta <- as.numeric(theta)
    theta_min <- min(theta, na.rm = TRUE)
    theta_max <- max(theta, na.rm = TRUE)
  }

  k <- length(thresholds) + 1L
  prob <- matrix(0, nrow = length(theta), ncol = k)

  for (i in seq_along(theta)) {
    numerators <- numeric(k)
    numerators[1] <- 0

    if (k > 1L) {
      for (cat_idx in 2:k) {
        numerators[cat_idx] <- sum(theta[i] - thresholds[seq_len(cat_idx - 1L)])
      }
    }

    numerators <- numerators - max(numerators)
    numerators <- exp(numerators)
    prob[i, ] <- numerators / sum(numerators)
  }

  colnames(prob) <- as.character(seq_len(k))

  list(
    facets = facets,
    thresholds = thresholds,
    theta = theta,
    theta_min = theta_min,
    theta_max = theta_max,
    k = k,
    prob = prob,
    expected = as.vector(prob %*% seq_len(k))
  )
}

stack_positions <- function(groups, spread = 0.055) {
  out <- numeric(length(groups))
  split_idx <- split(seq_along(groups), groups)

  for (idx in split_idx) {
    n <- length(idx)
    out[idx] <- seq(-(n - 1) / 2, (n - 1) / 2, length.out = n) * spread
  }

  out
}

#' Plot representative observed versus fair averages for a facet
#'
#' @param x A `facets_out` object or path to a FACETS `.out` file.
#' @param facet_index Which measurement report to plot (default `1L`).
#' @param n Number of representative elements to show.
#'
#' @return A ggplot object.
plot_fairness <- function(x, facet_index = 1L, n = 8L) {
  facets <- as_facets_out(x)
  report <- measurement_report_at(facets, facet_index, required_cols = c("measure", "observed_avg", "fair_avg"), fallback = paste("Facet", facet_index), purpose = "observed-versus-fair plotting")
  student_plot <- select_representative_rows(report$data, n = n)
  student_plot$display_label <- factor(student_plot$display_label, levels = student_plot$display_label)

  point_df <- rbind(
    data.frame(display_label = student_plot$display_label, score = student_plot$observed_avg, score_type = "Observed average"),
    data.frame(display_label = student_plot$display_label, score = student_plot$fair_avg, score_type = "Fair score")
  )

  x_range <- range(c(student_plot$observed_avg, student_plot$fair_avg), na.rm = TRUE)
  x_min <- floor((x_range[1] - 0.2) * 4) / 4
  x_max <- ceiling((x_range[2] + 0.2) * 4) / 4

  ggplot2::ggplot(student_plot, ggplot2::aes(y = .data$display_label)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$observed_avg, xend = .data$fair_avg, yend = .data$display_label),
      linewidth = 1.15,
      color = "#A8A8A8"
    ) +
    ggplot2::geom_point(
      data = point_df,
      ggplot2::aes(x = .data$score, color = .data$score_type),
      size = 3.1
    ) +
    ggplot2::scale_color_manual(values = c("Observed average" = "#4C6D8E", "Fair score" = "#B35C34")) +
    ggplot2::scale_x_continuous(limits = c(x_min, x_max), breaks = seq(x_min, x_max, by = 0.25)) +
    ggplot2::labs(x = paste0(report$facet_name, ": observed and fair averages"), y = NULL) +
    facets_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Plot the distribution of measures for a facet
#'
#' @param x A `facets_out` object or path to a FACETS `.out` file.
#' @param facet_index Which measurement report to plot (default `2L`).
#' @param binwidth Histogram bin width in logits.
#'
#' @return A ggplot object.
plot_distribution <- function(x, facet_index = 2L, binwidth = 0.75) {
  facets <- as_facets_out(x)
  report <- measurement_report_at(facets, facet_index, required_cols = c("measure"), fallback = paste("Facet", facet_index), purpose = "distribution plotting")
  measures <- report$data$measure[!is.na(report$data$measure)]

  if (!length(measures)) {
    stop("No measures are available for plotting in ", report$facet_name, ".", call. = FALSE)
  }

  hist_breaks <- seq(floor(min(measures)) - binwidth / 2, ceiling(max(measures)) + binwidth / 2, by = binwidth)
  max_hist_count <- max(graphics::hist(measures, breaks = hist_breaks, plot = FALSE)$counts)
  mean_measure <- mean(measures)

  ggplot2::ggplot(report$data, ggplot2::aes(x = .data$measure)) +
    ggplot2::geom_histogram(binwidth = binwidth, boundary = min(hist_breaks), fill = "#6B8DAA", color = "white", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = mean_measure, color = "#B35C34", linewidth = 0.95) +
    ggplot2::geom_vline(xintercept = 0, color = "#69747F", linewidth = 0.8, linetype = "dashed") +
    ggplot2::geom_rug(sides = "b", alpha = 0.18, color = "#1F1F1F") +
    ggplot2::annotate(
      "label",
      x = mean_measure - 0.15,
      y = max_hist_count + 0.8,
      label = sprintf("Mean = %.2f", mean_measure),
      hjust = 1,
      vjust = 1,
      fill = "white",
      linewidth = 0.2,
      color = "#B35C34",
      size = 3.3
    ) +
    ggplot2::labs(x = paste0(report$facet_name, " measure (logits)"), y = paste("Number of", tolower(report$facet_name))) +
    ggplot2::scale_x_continuous(breaks = seq(floor(min(measures)), ceiling(max(measures)), by = 1)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.14))) +
    facets_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "#E6E6E6", linewidth = 0.35))
}

#' Plot measure estimates with standard errors for a facet
#'
#' @param x A `facets_out` object or path to a FACETS `.out` file.
#' @param facet_index Which measurement report to plot (default `3L`).
#'
#' @return A ggplot object.
plot_estimates <- function(x, facet_index = 3L) {
  facets <- as_facets_out(x)
  report <- measurement_report_at(facets, facet_index, required_cols = c("measure", "se"), fallback = paste("Facet", facet_index), purpose = "estimates plotting")
  df <- report$data[order(report$data$measure), , drop = FALSE]

  if (!nrow(df)) {
    stop("No measurement rows with standard errors are available for plotting.", call. = FALSE)
  }

  df$display_label <- factor(wrap_text(df$label, width = 18), levels = wrap_text(df$label, width = 18))

  ggplot2::ggplot(df, ggplot2::aes(x = .data$measure, y = .data$display_label)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.8, linetype = "dashed", color = "#69747F") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = .data$measure - .data$se, xmax = .data$measure + .data$se), height = 0.18, linewidth = 0.7, color = "#69747F") +
    ggplot2::geom_point(size = 3.4, color = "#204D73") +
    ggplot2::labs(x = paste0(report$facet_name, " measure (logits)"), y = NULL) +
    facets_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Plot rating-scale category usage frequencies
#'
#' @param x A `facets_out` object or path to a FACETS `.out` file.
#'
#' @return A ggplot object.
plot_usage <- function(x) {
  facets <- as_facets_out(x)
  df <- facets$categories
  df$category <- factor(df$category, levels = df$category)
  df$pct_label <- scales::percent(df$used_count / sum(df$used_count), accuracy = 1)
  palette <- category_palette(nrow(df))

  ggplot2::ggplot(df, ggplot2::aes(x = .data$category, y = .data$used_count, fill = .data$category)) +
    ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(.data$used_count, "\n(", .data$pct_label, ")")), vjust = -0.2, size = 3.3, color = "#202020") +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(), expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(x = "Category", y = "Number of responses") +
    facets_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "#E6E6E6", linewidth = 0.35))
}

#' Plot rating-scale category probability curves
#'
#' @param x A `facets_out` object or path to a FACETS `.out` file.
#' @param theta Optional vector of theta values.
#'
#' @return A ggplot object.
plot_probability <- function(x, theta = NULL) {
  curve_data <- prepare_rating_scale_data(x, theta = theta)
  palette <- category_palette(curve_data$k)
  prob_df <- tibble::as_tibble(curve_data$prob)
  prob_df$theta <- curve_data$theta
  prob_df <- tidyr::pivot_longer(prob_df, cols = -theta, names_to = "Category", values_to = "Probability")
  prob_df$Category <- factor(prob_df$Category, levels = as.character(seq_len(curve_data$k)))
  threshold_df <- data.frame(
    theta = curve_data$thresholds,
    Probability = rep(1.03, length(curve_data$thresholds)),
    label = paste0("d", seq_along(curve_data$thresholds), " = ", round(curve_data$thresholds, 2))
  )

  ggplot2::ggplot(prob_df, ggplot2::aes(x = .data$theta, y = .data$Probability, color = .data$Category)) +
    ggplot2::geom_vline(xintercept = curve_data$thresholds, linetype = "dotted", color = "#BFBFBF", linewidth = 0.45) +
    ggplot2::geom_line(linewidth = 1.05, alpha = 0.92) +
    geom_facets_label_repel(
      data = threshold_df,
      ggplot2::aes(x = .data$theta, y = .data$Probability, label = .data$label),
      inherit.aes = FALSE,
      direction = "y",
      nudge_y = 0.03,
      color = "#707070",
      size = 2.8
    ) +
    ggplot2::scale_color_manual(values = palette, labels = paste("Category", seq_len(curve_data$k))) +
    ggplot2::scale_x_continuous(breaks = seq(floor(curve_data$theta_min), ceiling(curve_data$theta_max), by = 1), expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.2), labels = scales::label_number(accuracy = 0.1), expand = ggplot2::expansion(mult = c(0.01, 0.08))) +
    ggplot2::labs(x = "Person measure (logits)", y = "Probability") +
    ggplot2::coord_cartesian(clip = "off") +
    facets_theme() +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1, override.aes = list(linewidth = 2)))
}

#' Plot the expected-score ogive implied by Table 8.1 thresholds
#'
#' @param x A `facets_out` object or path to a FACETS `.out` file.
#' @param theta Optional vector of theta values.
#'
#' @return A ggplot object.
plot_ogive <- function(x, theta = NULL) {
  curve_data <- prepare_rating_scale_data(x, theta = theta)
  ogive_df <- data.frame(theta = curve_data$theta, expected = curve_data$expected)
  crossing_levels <- seq(1.5, curve_data$k - 0.5, by = 1)

  crossing_df <- data.frame(
    y = crossing_levels,
    x = vapply(crossing_levels, function(level) curve_data$theta[which.min(abs(curve_data$expected - level))], numeric(1)),
    label = paste0("E = ", crossing_levels)
  )
  crossing_df$label_x <- pmin(crossing_df$x + 0.45, curve_data$theta_max - 0.15)
  crossing_df$label_y <- pmin(crossing_df$y + 0.28, curve_data$k + 0.02)

  ggplot2::ggplot(ogive_df, ggplot2::aes(x = .data$theta, y = .data$expected)) +
    ggplot2::annotate(
      "rect",
      xmin = curve_data$theta_min - 0.2,
      xmax = curve_data$theta_max + 0.2,
      ymin = seq(1, curve_data$k - 1, by = 1),
      ymax = seq(2, curve_data$k, by = 1),
      fill = rep(c("#F7F7F7", "#FFFFFF"), length.out = curve_data$k - 1),
      alpha = 0.6
    ) +
    ggplot2::geom_vline(xintercept = curve_data$thresholds, linetype = "dotted", color = "#CDCDCD", linewidth = 0.45) +
    ggplot2::geom_line(linewidth = 1.2, color = "#2C5F8A") +
    ggplot2::geom_point(data = crossing_df, ggplot2::aes(x = .data$x, y = .data$y), inherit.aes = FALSE, shape = 21, size = 2.8, fill = "white", color = "#888888", stroke = 0.7) +
    ggplot2::geom_segment(
      data = crossing_df,
      ggplot2::aes(x = .data$x, y = .data$y, xend = .data$label_x - 0.07, yend = .data$label_y - 0.05),
      inherit.aes = FALSE,
      color = "#A8A8A8",
      linewidth = 0.35
    ) +
    ggplot2::geom_label(
      data = crossing_df,
      ggplot2::aes(x = .data$label_x, y = .data$label_y, label = .data$label),
      inherit.aes = FALSE,
      hjust = 0,
      vjust = 0,
      fill = "#FFFFFF",
      alpha = 1,
      linewidth = 0.3,
      color = "#707070",
      size = 2.7
    ) +
    ggplot2::scale_x_continuous(breaks = seq(floor(curve_data$theta_min), ceiling(curve_data$theta_max), by = 1), expand = ggplot2::expansion(mult = 0.02)) +
    ggplot2::scale_y_continuous(breaks = seq_len(curve_data$k), labels = as.character(seq_len(curve_data$k)), limits = c(0.9, curve_data$k + 0.1), expand = ggplot2::expansion(mult = 0)) +
    ggplot2::labs(x = "Person measure (logits)", y = "Expected score") +
    ggplot2::coord_cartesian(clip = "off") +
    facets_theme()
}

#' Plot mode, median, and mean category regions from rating-scale thresholds
#'
#' @param x A `facets_out` object or path to a FACETS `.out` file.
#' @param theta Optional vector of theta values.
#'
#' @return A ggplot object.
plot_structure <- function(x, theta = NULL) {
  curve_data <- prepare_rating_scale_data(x, theta = theta)
  palette <- category_palette(curve_data$k)
  mode_cat <- apply(curve_data$prob, 1, which.max)
  cum_prob <- t(apply(curve_data$prob, 1, cumsum))
  median_cat <- apply(cum_prob, 1, function(values) min(which(values >= 0.5)))
  mean_cat <- round(curve_data$expected)
  mean_cat[mean_cat < 1] <- 1
  mean_cat[mean_cat > curve_data$k] <- curve_data$k

  build_runs <- function(categories, stat_label) {
    runs <- rle(categories)
    end_idx <- cumsum(runs$lengths)
    start_idx <- c(1, end_idx[-length(end_idx)] + 1)

    data.frame(
      stat = stat_label,
      category = as.character(runs$values),
      x_start = curve_data$theta[start_idx],
      x_end = curve_data$theta[end_idx],
      stringsAsFactors = FALSE
    )
  }

  scale_df <- rbind(
    build_runs(mode_cat, "Mode"),
    build_runs(median_cat, "Median"),
    build_runs(mean_cat, "Mean")
  )

  scale_df$stat <- factor(scale_df$stat, levels = c("Mode", "Median", "Mean"))
  scale_df$category <- factor(scale_df$category, levels = as.character(seq_len(curve_data$k)))
  threshold_df <- data.frame(x = curve_data$thresholds, y = 3.72, label = paste0("d", seq_along(curve_data$thresholds), "\n", round(curve_data$thresholds, 2)))

  ggplot2::ggplot(scale_df) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = .data$x_start, xmax = .data$x_end, ymin = as.numeric(.data$stat) - 0.38, ymax = as.numeric(.data$stat) + 0.38, fill = .data$category),
      alpha = 0.88
    ) +
    ggplot2::geom_hline(yintercept = c(1.5, 2.5), color = "white", linewidth = 1.15) +
    ggplot2::geom_vline(xintercept = curve_data$thresholds, linetype = "dashed", color = "#505050", linewidth = 0.45) +
    geom_facets_label_repel(
      data = threshold_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      inherit.aes = FALSE,
      direction = "y",
      nudge_y = 0.06,
      color = "#404040",
      size = 2.7,
      lineheight = 0.85
    ) +
    ggplot2::geom_text(ggplot2::aes(x = (.data$x_start + .data$x_end) / 2, y = as.numeric(.data$stat), label = .data$category), color = "white", fontface = "bold", size = 3.5) +
    ggplot2::scale_fill_manual(values = palette, labels = paste("Category", seq_len(curve_data$k))) +
    ggplot2::scale_y_continuous(breaks = 1:3, labels = c("Mode", "Median", "Mean"), expand = ggplot2::expansion(mult = c(0.1, 0.25))) +
    ggplot2::scale_x_continuous(breaks = seq(floor(curve_data$theta_min), ceiling(curve_data$theta_max), by = 1), expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::labs(x = "Person measure (logits)", y = NULL) +
    ggplot2::coord_cartesian(clip = "off") +
    facets_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), legend.position = "bottom") +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, override.aes = list(alpha = 1)))
}

#' Plot the strongest unexpected responses from Table 4.1
#'
#' @param x A `facets_out` object or path to a FACETS `.out` file.
#' @param top_n Number of rows to show.
#'
#' @return A ggplot object.
plot_unexpected <- function(x, top_n = 20L) {
  facets <- as_facets_out(x)
  df <- facets$unexpected

  if (!nrow(df)) {
    stop("No unexpected responses were parsed from Table 4.1.", call. = FALSE)
  }

  order_idx <- order(abs(df$standardized_residual), decreasing = TRUE)
  df <- df[order_idx[seq_len(min(top_n, nrow(df)))], , drop = FALSE]
  label_cols <- grep("^facet[0-9]+_label$", names(df), value = TRUE)
  if (length(label_cols)) {
    df$case <- do.call(paste, c(lapply(label_cols, function(col) wrap_text(df[[col]], width = 20)), sep = " | "))
  } else {
    df$case <- paste("Response", seq_len(nrow(df)))
  }
  df$case <- factor(df$case, levels = rev(df$case))
  df$direction <- ifelse(df$standardized_residual >= 0, "Higher than expected", "Lower than expected")

  ggplot2::ggplot(df, ggplot2::aes(x = .data$standardized_residual, y = .data$case, fill = .data$direction)) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8, color = "#69747F") +
    ggplot2::scale_fill_manual(values = c("Higher than expected" = "#B35C34", "Lower than expected" = "#4C6D8E")) +
    ggplot2::labs(x = "Standardized residual", y = NULL) +
    facets_theme(base_size = 10.5) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

#' Plot a compact Wright map across available facets
#'
#' @param x A `facets_out` object or path to a FACETS `.out` file.
#'
#' @return A ggplot object.
plot_wright <- function(x) {
  facets <- as_facets_out(x)
  thresholds <- facets$categories$threshold[!is.na(facets$categories$threshold)]
  reports <- measurement_reports_list(facets)
  nonempty_idx <- which(vapply(reports, nrow, integer(1)) > 0)

  if (!length(nonempty_idx)) {
    stop("No measurement report rows are available for the Wright map.", call. = FALSE)
  }

  shown_idx <- nonempty_idx[seq_len(min(3L, length(nonempty_idx)))]
  shown_names <- vapply(shown_idx, function(i) measurement_facet_name(facets, i, fallback = paste("Facet", i)), character(1))
  report_frames <- lapply(seq_along(shown_idx), function(pos) {
    idx <- shown_idx[pos]
    report <- reports[[idx]]
    data.frame(
      facet = shown_names[pos],
      facet_position = pos,
      measure = report$measure,
      label = report$label,
      id = report$id,
      is_extreme = if ("is_extreme" %in% names(report)) report$is_extreme else FALSE,
      row.names = NULL,
      stringsAsFactors = FALSE
    )
  })

  entity_df <- do.call(rbind, report_frames)

  entity_df <- entity_df[!is.na(entity_df$measure), , drop = FALSE]
  entity_df$facet <- factor(entity_df$facet, levels = shown_names)
  entity_df$facet_x <- entity_df$facet_position
  entity_df$is_extreme <- factor(as.character(entity_df$is_extreme), levels = c("FALSE", "TRUE"))
  entity_df$group_key <- paste(entity_df$facet, sprintf("%.4f", entity_df$measure), sep = "::")
  entity_df <- entity_df[order(entity_df$facet, entity_df$measure, entity_df$id), , drop = FALSE]
  entity_df$x <- entity_df$facet_x + stack_positions(entity_df$group_key)

  labeled_facet <- shown_names[length(shown_names)]
  labeled_df <- entity_df[entity_df$facet == labeled_facet, , drop = FALSE]
  labeled_df$label <- paste0(wrap_text(labeled_df$label, width = 16), "\n", sprintf("%.2f logits", labeled_df$measure))
  labeled_df$label_x <- length(shown_names) + 0.34
  threshold_df <- data.frame(
    x = rep(length(shown_names) + 0.86, length(thresholds)),
    y = thresholds,
    label = paste0("d", seq_along(thresholds), " = ", round(thresholds, 2))
  )
  facet_palette <- stats::setNames(c("#4C6D8E", "#B35C34", "#204D73")[seq_along(shown_names)], shown_names)
  band_fills <- c("#F7FAFC", "#FCF8F5", "#F5F9FC")
  band_annotations <- lapply(seq_along(shown_names), function(i) {
    ggplot2::annotate("rect", xmin = i - 0.28, xmax = i + 0.28, ymin = -Inf, ymax = Inf, fill = band_fills[i], alpha = 0.7)
  })

  plot_obj <- ggplot2::ggplot()
  for (annotation in band_annotations) {
    plot_obj <- plot_obj + annotation
  }

  plot_obj +
    ggplot2::geom_hline(yintercept = thresholds, linetype = "dotted", color = "#D3D3D3", linewidth = 0.42) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#6D7781", linewidth = 0.75) +
    ggplot2::geom_point(
      data = entity_df,
      ggplot2::aes(x = .data$x, y = .data$measure, color = .data$facet, shape = .data$is_extreme),
      size = 2.2,
      alpha = 0.7
    ) +
    ggplot2::geom_point(
      data = labeled_df,
      ggplot2::aes(x = .data$x, y = .data$measure),
      color = facet_palette[[length(facet_palette)]],
      size = 3.5
    ) +
    ggplot2::geom_segment(
      data = labeled_df,
      ggplot2::aes(x = length(shown_names) + 0.08, xend = .data$label_x - 0.04, y = .data$measure, yend = .data$measure),
      color = "#A8A8A8",
      linewidth = 0.35
    ) +
    geom_facets_label_repel(
      data = labeled_df,
      ggplot2::aes(x = .data$label_x, y = .data$measure, label = .data$label),
      inherit.aes = FALSE,
      direction = "y",
      force = 1.2,
      hjust = 0,
      size = 2.85,
      color = facet_palette[[length(facet_palette)]]
    ) +
    geom_facets_label_repel(
      data = threshold_df,
      ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
      inherit.aes = FALSE,
      direction = "y",
      force = 1,
      hjust = 0,
      size = 2.7,
      color = "#707070"
    ) +
    ggplot2::annotate("label", x = length(shown_names) + 0.34, y = Inf, label = labeled_facet, vjust = 1.6, fill = "white", linewidth = 0.2, color = facet_palette[[length(facet_palette)]], fontface = "bold", size = 3) +
    ggplot2::annotate("label", x = length(shown_names) + 0.86, y = Inf, label = "Thresholds", vjust = 1.6, fill = "white", linewidth = 0.2, color = "#707070", fontface = "bold", size = 3) +
    ggplot2::scale_color_manual(values = facet_palette, name = "Facet") +
    ggplot2::scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), labels = c(`FALSE` = "Estimated", `TRUE` = "Extreme"), name = "Status") +
    ggplot2::scale_x_continuous(breaks = seq_along(shown_names), labels = shown_names, limits = c(0.65, length(shown_names) + 1.12), expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
    ggplot2::labs(x = NULL, y = "Measure (logits)") +
    ggplot2::coord_cartesian(clip = "off") +
    facets_theme() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "top",
      plot.margin = ggplot2::margin(16, 72, 12, 12)
    )
}

#' Render a standard set of FACETS visualizations to disk
#'
#' @param x A `facets_out` object or path to a FACETS `.out` file.
#' @param output_dir Output directory for image files.
#' @param include Character vector of plot names to render.
#' @param formats File formats to write, such as `"png"` or `"tiff"`.
#' @param dpi Image resolution.
#' @param top_n_unexpected Number of unexpected responses to show when rendering that plot.
#'
#' @return Invisibly, a named list of generated file paths.
render_plots <- function(
  x,
  output_dir = "figures",
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
  facets <- as_facets_out(x)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  builders <- list(
    wright_map = function() plot_wright(facets),
    observed_vs_fair = function() plot_fairness(facets),
    measure_distribution = function() plot_distribution(facets),
    estimates = function() plot_estimates(facets),
    category_usage = function() plot_usage(facets),
    probability_curves = function() plot_probability(facets),
    expected_score_ogive = function() plot_ogive(facets),
    scale_structure = function() plot_structure(facets),
    unexpected_responses = function() plot_unexpected(facets, top_n = top_n_unexpected)
  )
  specs <- default_plot_specs()

  if (!all(include %in% names(builders))) {
    unknown <- include[!(include %in% names(builders))]
    stop("Unknown plot name(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }

  available <- available_plot_names(facets)
  skipped <- include[!(include %in% available)]

  if (length(skipped)) {
    warning("Skipping unavailable plot(s): ", paste(skipped, collapse = ", "), call. = FALSE)
    include <- include[include %in% available]
  }

  if (!length(include)) {
    stop("None of the requested plots are available for this FACETS output.", call. = FALSE)
  }

  written <- vector("list", length(include))
  names(written) <- include

  for (name in include) {
    plot_obj <- builders[[name]]()
    dims <- specs[[name]]
    files <- character(0)

    for (fmt in formats) {
      filename <- file.path(output_dir, paste0(name, ".", fmt))
      save_plot_file(plot_obj, filename, width = dims[1], height = dims[2], dpi = dpi)
      files <- c(files, filename)
    }

    written[[name]] <- files
  }

  invisible(written)
}
