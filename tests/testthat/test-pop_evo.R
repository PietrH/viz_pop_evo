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
      show_labels = F
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
      show_labels = T,
      colours = colstring_to_hex("blue", "green", "red")
    )
  expect_identical(purrr::pluck(plot, "labels", "x"), "years")
  expect_identical(purrr::pluck(plot, "labels", "y"), "n")
  expect_identical(purrr::pluck(plot, "labels", "label"), "lifestage")
  expect_true(grepl("lambda", purrr::pluck(plot, "labels", "subtitle")))
})

test_that("Create correct and valid population matrix", {
  species_dynamics <-
    dplyr::filter(
      example_pop_dyn,
      species == "deer",
      locality == "Wallonia"
    )
  lifestages <- unique(species_dynamics$lifestage)
  expect_identical(
    create_population_matrix(species_dynamics, lifestages),
    popbio::matrix2(
      c(0, 0, 0.95, 0.7, 0.45, 0, 0, 0, 0, 0.7, 0, 0, 0, 0, 0.9, 0.5),
      stages = c("juvenile", "subadult", "adult", "senescent")
    )
  )
})

test_that("Warn for colour", {
  expect_warning(
    viz_pop_evo(
      example_pop_dyn,
      "wild boar",
      "Flanders",
      n = c(50, 50, 40),
      show_labels = F
    ),
    "No provided colours, defaulting to #000000",
    fixed = TRUE
  )
  expect_warning(
    viz_pop_evo(
      example_pop_dyn,
      "wild boar",
      "Flanders",
      n = c(50, 50, 40),
      colours = c("#FF0000", "#0000FF"),
      show_labels = F
    )
  )
})

test_that("Warn for n", {
  expect_warning(
    viz_pop_evo(
      example_pop_dyn,
      "wild boar",
      "Flanders",
      show_labels = F,
      colours = rep("#000000",3)
    ))
  expect_warning(viz_pop_evo(
    example_pop_dyn,
    "wild boar",
    "Flanders",
    show_labels = F,
    colours = rep("#000000",3),
    n = c(4,4)
  ))
  expect_warning(viz_pop_evo(
    example_pop_dyn,
    "wild boar",
    "Flanders",
    show_labels = F,
    colours = rep("#000000",3),
    n = c(4,4,9,19,29)
  ))
})

