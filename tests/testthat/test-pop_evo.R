test_that("fetch_survival can fetch a value", {
  expect_identical(
    {
      example_pop_dyn %>%
        dplyr::filter(
          species == "deer",
          locality == "Wallonia"
        ) %>%
        fetch_survival(selected_lifestage = "adult")
    },
    0.9
  )
})

test_that("Error on missing arguments", {
  expect_error(
    {
      viz_pop_evo(example_pop_dyn)
    },
    "The following arguments are required, but missing: selected_species, selected_locality",
    fixed = TRUE
  )
})

test_that("Error on missing columns", {
  expect_error({
    input <- dplyr::select(example_pop_dyn, -survival)
    viz_pop_evo(
      input_df = input,
      selected_species = "wild boar",
      selected_locality = "Flanders",
      n = c(10, 20, 10),
      years = 5,
      colours = colstring_to_hex("blue", "green", "red"),
      show_labels = FALSE
    )
  })
})


test_that("Check if output plot is valid", {
  plot <-
    viz_pop_evo(
      example_pop_dyn,
      "wild boar",
      "Sweden",
      c(100, 200, 20),
      years = 50,
      show_labels = TRUE,
      colours = colstring_to_hex("blue", "green", "red")
    )
  expect_identical(purrr::pluck(plot, "labels", "x"), "years")
  expect_identical(purrr::pluck(plot, "labels", "y"), "n")
  expect_identical(purrr::pluck(plot, "labels", "label"), "lifestage")
  expect_true(grepl("lambda", purrr::pluck(plot, "labels", "subtitle")))
})

test_that("Check if we can turn the labels off", {
  plot <-
    viz_pop_evo(
      example_pop_dyn,
      "wild boar",
      "Sweden",
      c(100, 200, 20),
      years = 50,
      show_labels = FALSE,
      colours = colstring_to_hex("blue", "green", "red")
    )
  expect_false("label" %in% names(purrr::pluck(plot, "labels")))
})


test_that("User entry should intercept and replace by default", {
  expect_warning(check_user_entry(NULL,
    "colour",
    4,
    "lifestages",
    default_value = "#000000"
  ))
  expect_identical(
    suppressWarnings(
      check_user_entry(NULL, "colour", 4, "lifestages", default_value = "#000000")
    ),
    rep("#000000", 4)
  )
  expect_warning(check_user_entry(rep("#CD3278", 3),
    "colour",
    4,
    "lifestages",
    default_value = "#000000"
  ))
})
