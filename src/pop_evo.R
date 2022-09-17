# From input csv, create a population matrix, calculates population demoraphic
# evolution, and returns a line graph

# load libraries ----------------------------------------------------------

library(data.table) # CRAN v1.14.2
library(dplyr)      # CRAN v1.0.7
library(popbio)     # CRAN v2.7

# load input data ---------------------------------------------------------


input <-
  tibble(
    locality = c("Flanders", "Flanders", "Flanders", "Sweden", "Sweden", "Sweden", "Wallonia", "Wallonia", "Wallonia", "Wallonia"),
    species = c("wild boar", "wild boar", "wild boar", "wild boar", "wild boar", "wild boar", "deer", "deer", "deer", "deer"),
    lifestage = c("juvenile", "subadult", "adult", "juvenile", "subadult", "adult", "juvenile", "subadult", "adult", "senescent"),
    reproduction = c(0.59, 1.76, 2.29, 0.13, 0.56, 1.64, 0, 0, 0.95, 0.7),
    survival = c(0.52, 0.6, 0.71, 0.25, 0.31, 0.58, 0.45, 0.7, 0.9, 0.5)
  ) %>%
  setDT()

# check number of different possible lifestages
# data.table::uniqueN(input$lifestage)

## write csv out -----------------------------------------------------------

data.table::fwrite(input, file.path("data", "pop_dyn.csv"), sep = ";")


# build population matrix -------------------------------------------------


# some experimentation of what approach to take first ---------------------


# filter(input, species == "wild boar", locality == "Flanders") %>%
#   tidyr::pivot_wider(names_from = lifestage,values_from = reproduction)

# let's try flemish boars first

input_flemish_boar <- filter(input, species == "wild boar", locality == "Flanders")

lifestages <- input_flemish_boar %>%
  pull(lifestage)
# 
# # fertilities always in first row
# input_flemish_boar %>%
#   pull(reproduction) %>%
#   t()
# 
# 
# 
# 
# ## alternative
# pop_df <-
#   input_flemish_boar %>%
#   select(reproduction, lifestage) %>%
#   data.table::transpose(make.names = "lifestage")
# 
# # get the index of the lifestage in consideration
# # grep("subadult",lifestages)
#

# function to fetch the survival parameter for a specific lifestage (when
# already filtered down on locality)
# fetch_survival <- function(selected_lifestage) {
#   input_flemish_boar %>%
#     filter(lifestage == selected_lifestage) %>%
#     pull(survival)
# }

fetch_survival <- function(input_df,selected_lifestage) {
  input_df %>%
    filter(lifestage == selected_lifestage) %>%
    pull(survival)
}
# 
# out <- pop_df
# 
# 
# pop_df %>%
#   add_row(juvenile = fetch_survival("juvenile")) %>%
#   add_row(subadult = fetch_survival("subadult")) %>%
#   add_row(adult = fetch_survival("adult"))

# using matrix2
# matrix2(
# 
#   c(
#     input_flemish_boar$reproduction,
#     pull(filter(input_flemish_boar, lifestage == "juvenile"), survival), 0, 0,
#     0, pull(filter(input_flemish_boar, lifestage == "subadult"), survival), pull(filter(input_flemish_boar, lifestage == "adult"), survival)
#   ),
#   stages = lifestages
# )

# now but we don't want to count the 0's manually
matrix2(
  c(
    input_flemish_boar$reproduction,
    c(rep(0, grep("juvenile", lifestages) - 1), fetch_survival("juvenile"), rep(0, length(lifestages) - grep("juvenile", lifestages))),
    c(rep(0, grep("subadult", lifestages) - 1), fetch_survival("subadult"), fetch_survival("adult"))
  ),
  stages = lifestages
)

# can we somehow do figure out when to switch for the last line?

# function to create the second to nth row of the projection/population matrix,
# first row is fertility
build_matrix_row <- function(input_df, selected_lifestage) {
  lifestages <- pull(input_df,lifestage)
  
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

# create matrix
c(
  input_flemish_boar$reproduction,
  purrr::map(lifestages[1:length(lifestages) - 1],
             build_matrix_row,
             input_df = input_flemish_boar)) %>%
  purrr::flatten() %>%
  matrix2(lifestages)

# function to create the whole matrix in a single go

create_population_matrix <- function(input_df,selected_species,selected_locality){
  
  species_dynamics <-
    dplyr::filter(input_df,species == selected_species,locality == selected_locality)
  lifestages <- pull(species_dynamics,lifestage)

# head(lifestages,-1)
# force class to numeric (not integer) to allow for 
  c(pull(species_dynamics,reproduction),
    purrr::map(lifestages[1:length(lifestages) - 1],
               build_matrix_row,
               input_df = species_dynamics)) %>%
    purrr::flatten() %>%
    as.numeric() %>% 
    matrix2(lifestages)
  
}


# can we calculate population evolution -----------------------------------

deer_matrix <-
  create_population_matrix(input,
                           selected_species = "deer",
                           selected_locality = "Wallonia")

# n is number of animals per stage

# iterations is the number of "years" assuming that every generation only takes
# one year
pop.projection(deer_matrix,
               n = rep(100,nrow(deer_matrix)),
               iterations = 10)
