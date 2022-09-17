
library(data.table)
library(dplyr)

input <- 
  tibble(
    locality = c("Flanders","Flanders","Flanders","Sweden","Sweden","Sweden","Wallonia","Wallonia","Wallonia","Wallonia"),
    species = c("wild boar","wild boar","wild boar","wild boar","wild boar","wild boar","deer","deer","deer","deer"),
    lifestage = c("juvenile","subadult","adult","juvenile","subadult","adult","juvenile","subadult","adult","senescent"),
    reproduction = c(0.59,1.76,2.29,0.13,0.56,1.64,0,0,0.95,0.7),
    survival = c(0.52,0.6,0.71,0.25,0.31,0.58,0.45,0.7,0.9,0.5)
  ) %>% 
  setDT()

data.table::uniqueN(input$lifestage)
data.table::fwrite(input,file.path("data","pop_dyn.csv"),sep = ";")

# filter(input, species == "wild boar", locality == "Flanders") %>% 
#   tidyr::pivot_wider(names_from = lifestage,values_from = reproduction)

# let's try flemish boar

input_flemish_boar <- filter(input, species == "wild boar", locality == "Flanders")

lifestages <- input_flemish_boar %>% 
  pull(lifestage)

# fertilities always in first row
input_flemish_boar %>%
  pull(reproduction) %>%
  t()




## alternative
pop_df <- 
  input_flemish_boar %>%
  select(reproduction,lifestage) %>%
  data.table::transpose(make.names = "lifestage")

# get the index of the lifestage in consideration
# grep("subadult",lifestages)

fetch_survival <- function(selected_lifestage){
  input_flemish_boar %>% 
    filter(lifestage == selected_lifestage) %>% 
    pull(survival)
}

out <- pop_df


pop_df %>% 
  add_row(juvenile = fetch_survival("juvenile")) %>% 
  add_row(subadult = fetch_survival("subadult")) %>% 
  add_row(adult = fetch_survival("adult"))

# using matrix2
matrix2(
  
  c(
    input_flemish_boar$reproduction,
    pull(filter(input_flemish_boar,lifestage == "juvenile"),survival),0,0,
    0,pull(filter(input_flemish_boar,lifestage == "subadult"),survival),pull(filter(input_flemish_boar,lifestage == "adult"),survival)
  ),
  stages = lifestages)

# now but we don't want to count the 0's manually
matrix2(
  c(
    input_flemish_boar$reproduction,
    c(rep(0,grep("juvenile",lifestages)-1),fetch_survival("juvenile"),rep(0,length(lifestages)-grep("juvenile",lifestages))),
    c(rep(0,grep("subadult",lifestages)-1),fetch_survival("subadult"),fetch_survival("adult"))
  ),
  stages = lifestages)

# can we somehow do figure out when to switch for the last line?

# function to create the second to nth row of the projection/population matrix,
# first row is fertility
build_matrix_row <- function(selected_lifestage){
  
  if(grep(selected_lifestage, lifestages) +1 == length(lifestages)) {
    c(
      rep(0, grep(selected_lifestage, lifestages) - 1),
      fetch_survival(selected_lifestage),
      fetch_survival(lifestages[grep(selected_lifestage, lifestages) + 1])
    )
  } else {
    c(rep(0, grep(selected_lifestage, lifestages) - 1),
      fetch_survival(selected_lifestage),
      rep(0, length(lifestages) - grep(selected_lifestage, lifestages)))
  }
}

# create matrix
c(input_flemish_boar$reproduction,
  purrr::map(lifestages[1:length(lifestages) - 1], build_matrix_row)) %>%
  purrr::flatten() %>%
  matrix2(lifestages)
