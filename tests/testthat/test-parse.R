fixture_path <- function(name) {
  testthat::test_path("fixtures", name)
}

test_that("read_facets parses the fixture tables correctly", {
  facets <- read_facets(fixture_path("sample_facets.out"))

  expect_s3_class(facets, "facets_out")
  expect_equal(facets$title, "Sample FACETS Output")
  expect_equal(nrow(facets$measurement_reports[[1]]), 2)
  expect_equal(nrow(facets$measurement_reports[[2]]), 2)
  expect_equal(nrow(facets$measurement_reports[[3]]), 2)
  expect_equal(nrow(facets$categories), 5)
  expect_equal(nrow(facets$unexpected), 2)
  expect_equal(facets$measurement_reports[[1]]$label[1], "Student068")
  expect_equal(round(facets$measurement_reports[[2]]$measure[1], 2), 3.77)
  expect_equal(round(facets$categories$threshold[2], 2), -2.01)
})

test_that("read_facets parses general FACETS layouts", {
  baseball <- read_facets(fixture_path("Baseball.out"))
  mile3 <- read_facets(fixture_path("3mile.out"))

  expect_identical(baseball$measurement_facet_names, c("Teams", "Games played"))
  expect_equal(unname(vapply(baseball$measurement_reports, nrow, integer(1))), c(8L, 3L))
  expect_equal(nrow(baseball$categories), 6)

  expect_identical(mile3$measurement_facet_names, c("Group/time points"))
  expect_equal(unname(vapply(mile3$measurement_reports, nrow, integer(1))), 8L)
  expect_equal(nrow(mile3$categories), 3)
})
