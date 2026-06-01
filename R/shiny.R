plot_choices <- function() {
  c(
    "Measurement map" = "wright_map",
    "Observed vs fair averages" = "observed_vs_fair",
    "Measure distribution" = "measure_distribution",
    "Difficulty estimates" = "estimates",
    "Category usage" = "category_usage",
    "Probability curves" = "probability_curves",
    "Expected score ogive" = "expected_score_ogive",
    "Scale structure" = "scale_structure",
    "Unexpected responses" = "unexpected_responses"
  )
}

shiny_measurement_reports_list <- function(...) {
  get("measurement_reports_list", mode = "function", inherits = TRUE)(...)
}

shiny_measure_range_text <- function(...) {
  get("measure_range_text", mode = "function", inherits = TRUE)(...)
}

shiny_available_plot_names <- function(...) {
  get("available_plot_names", mode = "function", inherits = TRUE)(...)
}

shiny_plot_choice_labels <- function(...) {
  get("plot_choice_labels", mode = "function", inherits = TRUE)(...)
}

shiny_available_table_choices <- function(...) {
  get("available_table_choices", mode = "function", inherits = TRUE)(...)
}

table_choices <- function() {
  c(
    "Measurement report 1" = "measurement_1",
    "Measurement report 2" = "measurement_2",
    "Measurement report 3" = "measurement_3",
    "Categories" = "categories",
    "Unexpected responses" = "unexpected"
  )
}

overview_table <- function(facets) {
  reports <- shiny_measurement_reports_list(facets)
  report_names <- names(reports)

  report_summary <- do.call(
    rbind,
    lapply(seq_along(reports), function(i) {
      data.frame(
        Summary = c(
          paste0("Measurement report ", i, " (", report_names[[i]], ")"),
          paste0("Measure range ", i)
        ),
        Value = c(
          nrow(reports[[i]]),
          shiny_measure_range_text(reports[[i]])
        ),
        stringsAsFactors = FALSE
      )
    })
  )

  if (is.null(report_summary)) {
    report_summary <- data.frame(Summary = character(), Value = character(), stringsAsFactors = FALSE)
  }

  data.frame(
    Summary = c(
      "Title",
      "Model",
      report_summary$Summary,
      "Categories",
      "Unexpected responses"
    ),
    Value = c(
      facets$title,
      facets$model,
      report_summary$Value,
      nrow(facets$categories),
      nrow(facets$unexpected)
    ),
    stringsAsFactors = FALSE
  )
}

plot_unavailability_message <- function(facets, plot_name) {
  available <- shiny_available_plot_names(facets)

  if (plot_name %in% available) {
    return("")
  }

  switch(
    plot_name,
    wright_map = "No measurement report is available for a Wright map.",
    observed_vs_fair = "The first measurement report does not have enough observed and fair averages for this plot.",
    measure_distribution = "This FACETS file does not have a second measurement report for the distribution plot.",
    estimates = "This FACETS file does not have a third measurement report for the difficulty plot.",
    category_usage = "Table 8.1 category statistics are not available.",
    probability_curves = "Rasch-Andrich thresholds are not available in Table 8.1.",
    expected_score_ogive = "Rasch-Andrich thresholds are not available in Table 8.1.",
    scale_structure = "Rasch-Andrich thresholds are not available in Table 8.1.",
    unexpected_responses = "Unexpected responses are not available in Table 4.1.",
    "The requested plot is not available for this FACETS file."
  )
}

selected_plot <- function(facets, plot_name, top_n_unexpected = 20L) {
  if (!(plot_name %in% shiny_available_plot_names(facets))) {
    stop(plot_unavailability_message(facets, plot_name), call. = FALSE)
  }

  switch(
    plot_name,
    wright_map = plot_wright(facets),
    observed_vs_fair = plot_fairness(facets),
    measure_distribution = plot_distribution(facets),
    estimates = plot_estimates(facets),
    category_usage = plot_usage(facets),
    probability_curves = plot_probability(facets),
    expected_score_ogive = plot_ogive(facets),
    scale_structure = plot_structure(facets),
    unexpected_responses = plot_unexpected(facets, top_n = top_n_unexpected),
    stop("Unknown plot name: ", plot_name, call. = FALSE)
  )
}

selected_table <- function(facets, table_name) {
  if (grepl("^measurement_", table_name)) {
    index <- as.integer(sub("^measurement_", "", table_name))
    reports <- shiny_measurement_reports_list(facets)

    if (length(reports) < index) {
      stop("The requested measurement report is not available.", call. = FALSE)
    }

    return(reports[[index]])
  }

  switch(
    table_name,
    criteria = {
      reports <- shiny_measurement_reports_list(facets)
      if (length(reports) >= 3) reports[[3]] else stop("No third measurement report available.", call. = FALSE)
    },
    categories = facets$categories,
    unexpected = facets$unexpected,
    stop("Unknown table name: ", table_name, call. = FALSE)
  )
}

build_facets_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the app. Install it with install.packages('shiny').", call. = FALSE)
  }

  theme <- NULL
  if (requireNamespace("bslib", quietly = TRUE)) {
    theme <- bslib::bs_theme(version = 5, bootswatch = "flatly")
  }

  www_dir <- system.file("www", package = "facetsviz")
  logo_src <- if (nzchar(www_dir) && file.exists(file.path(www_dir, "logo.png"))) {
    "facetsviz_www/logo.png"
  } else {
    ""
  }

  ui <- shiny::fluidPage(
    theme = theme,
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(paste0(
          ".facetsviz-download { margin-bottom: 10px; }",
          ".facetsviz-download > .btn { display: block; width: 100%; }",
          ".facetsviz-header { display: flex; align-items: center; gap: 14px; margin-bottom: 20px; padding-bottom: 12px; border-bottom: 1px solid #dee2e6; }",
          ".facetsviz-header img { height: 54px; width: auto; }",
          ".facetsviz-header h2 { margin: 0; font-size: 1.6rem; font-weight: 600; color: #1a2e4a; }",
          ".facetsviz-header small { display: block; font-size: 0.8rem; color: #6c757d; font-weight: 400; margin-top: 2px; }"
        ))
      )
    ),
    shiny::div(
      class = "facetsviz-header",
      if (nzchar(logo_src)) shiny::tags$img(src = logo_src, alt = "facetsviz logo") else NULL,
      shiny::tags$div(
        shiny::tags$h2("facetsviz"),
        shiny::tags$small("FACETS Output Explorer")
      )
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("out_file", "Upload FACETS output file (.out or .txt)", accept = c(".out", ".txt")),
        shiny::selectInput("plot_name", "Plot", choices = plot_choices()),
        shiny::conditionalPanel(
          condition = "input.plot_name == 'unexpected_responses'",
          shiny::numericInput("top_n_unexpected", "Unexpected responses to show", value = 20, min = 5, max = 100, step = 1)
        ),
        shiny::selectInput("table_name", "Table", choices = table_choices()),
        shiny::div(class = "facetsviz-download", shiny::downloadButton("download_plot", "Download current plot", class = "btn-primary")),
        shiny::div(class = "facetsviz-download", shiny::downloadButton("download_all_plots", "Download all plots (ZIP)", class = "btn-primary")),
        shiny::div(class = "facetsviz-download", shiny::downloadButton("download_table", "Download selected table", class = "btn-primary"))
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Overview", shiny::tableOutput("overview")),
          shiny::tabPanel("Plot", 
            shiny::h4(shiny::textOutput("plot_title"), style = "margin-top: 15px; margin-bottom: 10px; font-weight: bold;"),
            shiny::plotOutput("plot", height = "680px")
          ),
          shiny::tabPanel("Data", DT::DTOutput("table_preview")),
          shiny::tabPanel(
            "About",
            shiny::tags$p("Upload a FACETS .out file to inspect parsed tables and render the main diagnostics used by facetsviz."),
            shiny::tags$p("To publish online, deploy this repository root or any directory that contains app.R together with the facetsviz folder.")
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    facets_data <- shiny::reactive({
      shiny::req(input$out_file)
      read_facets(input$out_file$datapath)
    })

    shiny::observeEvent(facets_data(), {
      facets <- facets_data()
      plot_options <- shiny_plot_choice_labels(facets)
      table_options <- shiny_available_table_choices(facets)

      if (!length(plot_options)) {
        plot_options <- c("No plot available" = "")
      }

      if (!length(table_options)) {
        table_options <- c("No table available" = "")
      }

      shiny::updateSelectInput(
        session,
        "plot_name",
        choices = plot_options,
        selected = unname(plot_options[[1]])
      )

      shiny::updateSelectInput(
        session,
        "table_name",
        choices = table_options,
        selected = unname(table_options[[1]])
      )
    }, ignoreInit = FALSE)

    current_plot <- shiny::reactive({
      shiny::req(nzchar(input$plot_name))
      selected_plot(facets_data(), input$plot_name, top_n_unexpected = input$top_n_unexpected)
    })

    output$plot_title <- shiny::renderText({
      shiny::req(nzchar(input$plot_name))
      options <- shiny_plot_choice_labels(facets_data())
      names(options)[options == input$plot_name]
    })

    current_table <- shiny::reactive({
      shiny::req(nzchar(input$table_name))
      selected_table(facets_data(), input$table_name)
    })

    output$overview <- shiny::renderTable({
      overview_table(facets_data())
    }, striped = TRUE, bordered = TRUE, spacing = "m")

    output$plot <- shiny::renderPlot({
      shiny::validate(
        shiny::need(nzchar(input$plot_name), "No plot is available for this FACETS file."),
        shiny::need(input$plot_name %in% shiny_available_plot_names(facets_data()), plot_unavailability_message(facets_data(), input$plot_name))
      )

      current_plot()
    }, res = 110)

    output$table_preview <- DT::renderDT({
      shiny::validate(
        shiny::need(nzchar(input$table_name), "No table is available for this FACETS file.")
      )

      current_table()
    }, options = list(pageLength = 20, lengthMenu = c(5, 10, 20, 50, 100)), rownames = FALSE)

    output$download_plot <- shiny::downloadHandler(
      filename = function() paste0(input$plot_name, ".png"),
      content = function(file) {
        if (!(input$plot_name %in% shiny_available_plot_names(facets_data()))) {
          stop(plot_unavailability_message(facets_data(), input$plot_name), call. = FALSE)
        }

        ggplot2::ggsave(file, current_plot(), width = 9, height = 6, dpi = 300, bg = "white")
      }
    )

    output$download_all_plots <- shiny::downloadHandler(
      filename = function() {
        source_name <- tools::file_path_sans_ext(basename(input$out_file$name))
        paste0(source_name, "_facets_plots.zip")
      },
      content = function(file) {
        shiny::req(input$out_file)

        source_name <- tools::file_path_sans_ext(basename(input$out_file$name))
        render_dir <- file.path(tempdir(), paste0("facetsviz-", source_name, "-", as.integer(Sys.time())))
        dir.create(render_dir, recursive = TRUE, showWarnings = FALSE)

        rendered <- unlist(
          render_plots(
            facets_data(),
            output_dir = render_dir,
            include = shiny_available_plot_names(facets_data()),
            formats = "png",
            dpi = 300,
            top_n_unexpected = input$top_n_unexpected
          ),
          use.names = FALSE
        )

        old_wd <- setwd(render_dir)
        on.exit(setwd(old_wd), add = TRUE)
        utils::zip(zipfile = file, files = basename(rendered))
      }
    )

    output$download_table <- shiny::downloadHandler(
      filename = function() paste0(input$table_name, ".csv"),
      content = function(file) {
        utils::write.csv(current_table(), file, row.names = FALSE)
      }
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}

#' Launch the facetsviz Shiny app
#'
#' @param launch If `TRUE`, run the app immediately. If `FALSE`, return the app object.
#' @param host Host passed to [shiny::runApp()] when launching.
#' @param port Port passed to [shiny::runApp()] when launching.
#'
#' @return A shiny app object when `launch = FALSE`, otherwise the result of [shiny::runApp()].
run_app <- function(launch = TRUE, host = "127.0.0.1", port = NULL) {
  www_dir <- system.file("www", package = "facetsviz")
  if (nzchar(www_dir)) {
    shiny::addResourcePath("facetsviz_www", www_dir)
  }

  app <- build_facets_app()

  if (!launch) {
    return(app)
  }

  shiny::runApp(app, host = host, port = port, launch.browser = TRUE)
}
