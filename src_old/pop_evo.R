# From input csv, create a population matrix, calculates population demoraphic
# evolution, and returns a line graph

# load libraries ----------------------------------------------------------

# library(data.table) # CRAN v1.14.2
library(dplyr) # CRAN v1.0.7
library(popbio) # CRAN v2.7
library(ggplot2) # CRAN v3.3.5
library(docstring)

# major purrr dependency, can soft data.table dependency

# load input data ---------------------------------------------------------


# input <-
#   tibble(
#     locality = c(
#       "Flanders",
#       "Flanders",
#       "Flanders",
#       "Sweden",
#       "Sweden",
#       "Sweden",
#       "Wallonia",
#       "Wallonia",
#       "Wallonia",
#       "Wallonia"
#     ),
#     species = c(
#       "wild boar",
#       "wild boar",
#       "wild boar",
#       "wild boar",
#       "wild boar",
#       "wild boar",
#       "deer",
#       "deer",
#       "deer",
#       "deer"
#     ),
#     lifestage = c(
#       "juvenile",
#       "subadult",
#       "adult",
#       "juvenile",
#       "subadult",
#       "adult",
#       "juvenile",
#       "subadult",
#       "adult",
#       "senescent"
#     ),
#     reproduction = c(0.59, 1.76, 2.29, 0.13, 0.56, 1.64, 0, 0, 0.95, 0.7),
#     survival = c(0.52, 0.6, 0.71, 0.25, 0.31, 0.58, 0.45, 0.7, 0.9, 0.5)
#   )

# check number of different possible lifestages
# data.table::uniqueN(input$lifestage)

## write csv out -----------------------------------------------------------

# data.table::fwrite(input, file.path("data", "pop_dyn.csv"), sep = ";")


## read csv in -------------------------------------------------------------

input <- data.table::fread(file.path("data", "pop_dyn.csv"))



# helper functions --------------------------------------------------------


# helper function to fetch a survival value for a specific lifestage from a
# dynamic_species intermediary dataframe


fetch_survival <- function(input_df, selected_lifestage) {

  #' Helper Function Fetch a survival value for a specific lifestage from a
  #' dynamic_species intermediary dataframe
  #'
  #' @param input_df Input dataframe that contains at least the columns survival
  #'  and lifestage, ideally one that's been filtered already to a specific
  #'  species and location
  #' @param selected_lifestage The lifestage to get, actually \code{dplyr::pull}
  #' , the survival value from
  #'
  #' @return one or more values of the same class as in the input dataframe
  #'
  #' @examples
  #' 
  #' data.table::fread(file.path("data", "pop_dyn.csv")) %>%
  #' dplyr::filter(species == "deer",
  #' locality == "Wallonia") %>% 
  #' fetch_locality

  input_df %>%
    dplyr::filter(lifestage == selected_lifestage) %>%
    dplyr::pull(survival)
}

# helper function to create the second to nth row of the projection/population
# matrix, first row is fertility
build_matrix_row <- function(input_df, selected_lifestage) {
  lifestages <- pull(input_df, lifestage)

  # using seq_along instead of grep to force exact string matching, fixed = TRUE
  selected_lifestage_index <-
    seq_along(lifestages)[lifestages == selected_lifestage]

  # is not cutting it
  if (selected_lifestage_index + 1 == length(lifestages)) {
    c(
      rep(0, selected_lifestage_index - 1),
      fetch_survival(input_df, selected_lifestage),
      fetch_survival(input_df, lifestages[selected_lifestage_index + 1])
    )
  } else {
    c(
      rep(0, grep(selected_lifestage, lifestages) - 1),
      fetch_survival(input_df, selected_lifestage),
      rep(0, length(lifestages) - grep(selected_lifestage, lifestages))
    )
  }
}

# helper function to create the population matrix
create_population_matrix <- function(species_dynamics, lifestages) {
  # force class to numeric (not integer) to avoid trouble with popbio
  c(
    pull(species_dynamics, reproduction),
    purrr::map(head(lifestages, -1),
      build_matrix_row,
      input_df = species_dynamics
    )
  ) %>%
    purrr::flatten() %>%
    as.numeric() %>%
    popbio::matrix2(lifestages)
}


# visualisation as a function ---------------------------------------------

# small function to turn known r color strings into hex codes
colstring_to_hex <-
  function(...) {
    rgb(t(col2rgb(c(...))), max = 255)
  }

# helper function to check if a user entered the expected number of values, if not,
# return a default value an expected amount of times
check_user_entry <-
  function(value,
           value_name,
           expected_number_of_values,
           expected_number_name,
           default_value) {
    
    if (is.null(value)) {
      warning(glue::glue("No provided {value_name}, defaulting to {default_value}"),
              call. = FALSE)
      return(rep(default_value, expected_number_of_values))
    } else {
      if (length(value) != expected_number_of_values) {
        warning(
          glue::glue(
            "Provided different number of {value_name}: {length(value)} ",
            "then the number of {expected_number_name}: {expected_number_of_values}",
            " defaulting to {default_value}"
          ),
          call. = FALSE
        )
        return(rep(default_value, expected_number_of_values))
      }
    }

    # if all is in order, return the input value as is
    return(value)
  }

# Main function, build the population matrix, calculate the population ---------
# evolution, and plot it


vizpopevo <-
  function(input_df,
           selected_species,
           selected_locality,
           n = NULL,
           years = 10,
           colours = NULL,
           show_labels = TRUE) {
    
    #'Build a population matrix, project the evolution of the population, and
    #'visualise the results
    #'
    #'@param input_df Input dataframe, with columns \code{locality}, \code{species}
    #'  , \code{lifestage}, \code{reproduction} and \code{survival}
    #'@param selected_species The species from the input_df for which the model
    #'  should be visualized
    #'@param selected_locality The locality for the species that should be
    #'  visualized
    #'@param n A vector of integers, specifying the number of individuals per
    #'  lifestage. If not provided, the function will default to 100 individuals for
    #'  every lifestage
    #'@param years The number of years the model should run for, really the number
    #'  of equally spaced points in time between the lifestages
    #'@param colours A character vector of hex codes of equal length to the number
    #'  of lifestages, these colours are used in the plot this function generates.
    #'@seealso [colstring_to_hex()] which can be used to convert built-in R colours
    #'  to hex codes. If no colours are provided (the default), the function will
    #'  return all resulting curves in black
    #'@param show_labels Either \code{TRUE} or \code{FALSE}, optionally show extra
    #'  labels on the output plot. This might be useful if no colours are provided
    #'
    #'@return a ggplot object that shows the projection of the population for
    #'  \code{years} steps
    #'@export
    #'
    #' @examples
    #' ```
    #' data <- data.table::fread(file.path("data", "pop_dyn.csv"))
    #' vizpopevo(data,
    #'             "wild boar",
    #'             "Flanders",
    #'             colours = colstring_to_hex("cyan","yellow2","plum4"),
    #'              show_labels = FALSE)
    #' ```

    # check if all necessary arguments have been provided
    req_arg <- c("input_df", "selected_species", "selected_locality")
    passed <- names(as.list(match.call())[-1])
    # from https://stackoverflow.com/a/38758257
    if (any(!req_arg %in% passed)) {
      stop(paste(
        "The following arguments are required, but missing:",
        paste(setdiff(req_arg, passed), collapse = ", ")
      ))
    }


    # check if input data contains all the required columns
    required <-
      c("locality", "species", "lifestage", "reproduction", "survival")
    missing_columns <- required[!required %in% names(input_df)]
    assertthat::assert_that(
      length(missing_columns) == 0,
      msg = glue::glue(
        "{deparse(substitute(input_df))}",
        " is missing ",
        "{ifelse(length(missing_columns)>1,'some columns','a column')}",
        ": {paste(missing_columns,collapse = ', ')}"
      )
    )


    # filter down our input_df to only the species and locality specified
    species_dynamics <-
      dplyr::filter(
        input_df,
        species == selected_species,
        locality == selected_locality
      )
    # extract the different values for lifestages, they are assumed to be
    # arranged from young to old, then count them.
    lifestages <- unique(species_dynamics$lifestage)
    number_of_lifestages <- length(lifestages)

    pop_matrix <- create_population_matrix(species_dynamics, lifestages)

    # set default colour and number of individuals
    colours <-
      check_user_entry(
        colours,
        "colours",
        number_of_lifestages,
        "lifestages",
        "#000000"
      )
    n <-
      check_user_entry(
        n,
        "number of individuals per lifestage",
        number_of_lifestages,
        "lifestages",
        100
      )

    # iterations starts counting from the starting position, so 10 iterations
    # will result in timeseries frames 0 to 9
    pop_evolution <-
      popbio::pop.projection(pop_matrix,
        n = n,
        iterations = years + 1
      )

    pop_evolution_stages <-
      pop_evolution %>%
      purrr::pluck("stage.vectors") %>%
      as_tibble(rownames = "lifestage") %>%
      tidyr::pivot_longer(
        cols = where(is.double),
        names_to = "iteration"
      ) %>%
      mutate(
        years = as.integer(iteration),
        n = value
      )

    out_plot <-
      ggplot2::ggplot(pop_evolution_stages) +
      ggplot2::aes(x = years, y = n, colour = lifestage) +
      ggplot2::geom_line(size = 1.25) +
      ggplot2::scale_color_manual(values = purrr::set_names(colours, lifestages)) +
      ggplot2::ggtitle(glue::glue("Projection of the {selected_species}",
                                  " population from {selected_locality}"),
                       sprintf("lambda = %.6g", pop_evolution$lambda)) +
      ggplot2::theme_minimal()

    # let's try to add point symbols to the lines to distinguish between lifestages
    # ggplot2::ggplot(pop_evolution_stages) +
    #   ggplot2::aes(x = years, y = n, colour = lifestage, shape = lifestage) +
    #   geom_point(size = 4) +
    #   ggplot2::geom_line(size = 1.25) +
    #   ggplot2::scale_color_manual(values = purrr::set_names(colours,lifestages),
    #                               guide = "none") +
    #   ggplot2::labs(subtitle = sprintf("lambda = %.6g",pop_evolution$lambda)) +
    #   ggplot2::theme_minimal()

    # only do it when black?
    # ggplot2::ggplot(pop_evolution_stages) +
    #   ggplot2::aes(x = years, y = n, colour = lifestage, shape = lifestage) +
    #   geom_point(size = 4,colour="black") +
    #   ggplot2::geom_line(size = 1.25,colour="black")

    if (show_labels) {

      # I don't know of a good spot to put the label annotation, so I've put it
      # about 75% in on the plot for now
      annotation <-
        pop_evolution_stages %>%
        dplyr::group_by(lifestage) %>%
        dplyr::summarise(
          x75 = purrr::pluck(years, floor(0.75 * length(years))),
          y75 = purrr::pluck(value, floor(0.75 * length(value)))
        )

      out_plot +
        ggplot2::geom_label(
          data = annotation,
          ggplot2::aes(x = x75, y = y75, label = lifestage),
          inherit.aes = FALSE
        )
    } else {
      out_plot
    }
  }

