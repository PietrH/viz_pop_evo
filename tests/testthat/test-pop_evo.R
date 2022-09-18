test_that("fetch_survival can fetch a value", {
  expect_identical(
    data.table::fread(file.path("..","..","data", "pop_dyn.csv")) %>%
      dplyr::filter(species == "deer",
                    locality == "Wallonia") %>%
      fetch_survival(selected_lifestage = "adult"),
    0.9
  )
})

test_that("Error on missing arguments",
  expect_error(
    viz_pop_evo(data.table::fread(file.path("..","..","data", "pop_dyn.csv"))),
"The following arguments are required, but missing: selected_species, selected_locality",
fixed = TRUE
                 ))

test_that("Error on missing columns",
          expect_error({
            input <- data.table::fread(
              file.path("..",
                        "..",
                        "data",
                        "pop_dyn.csv"),
              select = c("locality","species","lifestage","survival")
            )

            viz_pop_evo(
              input_df = input,
              selected_species = "wild boar",
              selected_locality = "Flanders",
              n = c(10, 20, 10),
              years = 5,
              colours = colstring_to_hex("blue", "green", "red"),
              show_labels = F
            )
          }

          ))


test_that("Check if output plot is valid",{
  input <-input <- data.table::fread(
    file.path("..",
              "..",
              "data",
              "pop_dyn.csv"))
  plot <- viz_pop_evo("wild boar","Sweden",c(100,200,20),years = 50,show_labels = T)

}
          )