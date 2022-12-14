
# vizpopevo

<!-- badges: start -->
<!-- badges: end -->

This package provides one main function: `viz_pop_evo()`, and a number of helpers. Currently the main function really does a bit much, it loads and filters the input data, transforms it to a population matrix, runs the model and returns a visualization of this model. It offers a number of defaults for optional arguments, and will warn you when required arguments are missing. It also checks the input dataframe. It offers a small amount of customizability in regards to the output plot, you can set the colours of the resulting curves, and you can turn curve labels on or off.

## Installation

You can install the development version of vizpopevo from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PietrH/viz_pop_evo")
```

## Example

Example of the main function in action, an example dataset is provided:


``` r
library(vizpopevo)

# use a helper to translate built-in R colours to hex colour codes
viz_pop_evo(example_pop_dyn,
  selected_species = "wild boar",
  selected_locality = "Flanders",
  n = c(40,1,2),
  years = 7,
  colours = colstring_to_hex("green2","yellow2","cyan"),
  show_labels = FALSE)

```
Would result in the very stylish: 
![image](https://user-images.githubusercontent.com/48065851/190928353-15b2288d-0903-4642-aead-0fbd25f68661.png)


## Test coverage
``` r
> test_local()
√ | F W S  OK | Context
√ |        11 | pop_evo [0.1s]                                                            

== Results ===============================================================================
Duration: 0.1 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 11 ]
> covr::package_coverage()
vizpopevo Coverage: 100.00%
R/pop_evo.R: 100.00%
```
