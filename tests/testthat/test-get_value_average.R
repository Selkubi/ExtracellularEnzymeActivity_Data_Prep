# Create sample data
# The average of each Xth element of the vectors is what the get_value_average gives.
create_sample_data <- function() {
  list(
    emmGrid1 = data.frame(
      contrast = c("C1 - C2", "C1 - C3", "C2 - C3"),
      p.value = c(0.0103, 0.0001, 0.0114)
    ),
    emmGrid2 = data.frame(
      contrast = c("C1 - C2", "C1 - C3", "C2 - C3"),
      p.value = c(0.1484, 0.6249, 0.5744)
    ),
    emmGrid3 = data.frame(
      contrast = c("C1 - C2", "C1 - C3", "C2 - C3"),
      p.value = c(0.9171, 0.0082, 0.0203)
    )
  )
}

testthat::test_that("get_value_average calculates the average p-values correctly", {
  data <- create_sample_data()
  p_values_list <- lapply(data, function(df) df$p.value)

  expected_result <- purrr::map_dbl(transpose(p_values_list), mean) |> signif(2)

  result <- EEAanalysis::get_value_average("p.value", p_values_list)

  testthat::expect_equal(result, expected_result)
})

testthat::test_that("get_value_average calculates the average p-values correctly", {
  data_with_na <- create_sample_data()
  data_with_na[[1]][3,"p.value"] <- NA

  expect_error(get_value_average("p.value", data_with_na))
})
