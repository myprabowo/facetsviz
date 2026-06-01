fixture_path <- function(name) {
  testthat::test_path("fixtures", name)
}

test_that("plot builders return ggplot objects", {
  facets <- read_facets(fixture_path("sample_facets.out"))

  expect_s3_class(plot_wright(facets), "ggplot")
  expect_s3_class(plot_distribution(facets), "ggplot")
  expect_s3_class(plot_probability(facets), "ggplot")
})

test_that("render_batch writes expected files", {
  out_dir <- tempfile("facetsviz-batch-")
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)

  result <- render_batch(
    paths = fixture_path("sample_facets.out"),
    output_root = out_dir,
    include = c("wright_map", "category_usage"),
    formats = "png"
  )

  expect_equal(nrow(result), 1)
  png_files <- list.files(result$output_dir[1], pattern = "\\.png$", full.names = TRUE)
  expect_equal(length(png_files), 2)
  expect_true(all(file.exists(png_files)))
})

test_that("general FACETS files expose only available plots", {
  baseball <- read_facets(fixture_path("Baseball.out"))
  mile3 <- read_facets(fixture_path("3mile.out"))

  expect_true(all(c("wright_map", "observed_vs_fair", "measure_distribution") %in% available_plot_names(baseball)))
  expect_false("estimates" %in% available_plot_names(baseball))
  expect_true(all(c("wright_map", "observed_vs_fair", "category_usage") %in% available_plot_names(mile3)))
  expect_false("measure_distribution" %in% available_plot_names(mile3))
})
