

<!-- badges: start -->
<!-- badges: end -->

# danubeoccurR <img src="man/figures/logo.png" align="right" height="139" alt="" />

This R package provides a set of tools to download, clean, and standardize species occurrence records, with a focus on fish species in the Danube River Basin. While primarily designed for Danube fish data, the package can be applied to species occurrence data more broadly. It includes functions for both experienced R users and those with less coding experience, offering user-friendly interfaces and interactive features via Shiny apps, making it accessible to a wider audience.

## Installation

You can install the development version of `danubeoccurR` from GitHub using the `remotes` package.

First, ensure you have `remotes` installed:

```r
install.packages("remotes")

remotes::install_github("ytorres-cambas/danubeoccurR")

```

#### 3. Usage Example

## Usage

Here is an example of how to snap points on a map using the `snap_points_on_map()` function. First, prepare a data frame containing the coordinates (latitude and longitude) of the points you'd like to snap. In this example, we have two locations: Vienna and Belgrade.

```r
# Load your package
library(danubeoccurR)

# Define coordinates for Vienna and Belgrade
coords <- data.frame(
  id = 1:2,
  lat = c(48.2082, 44.8176),   # Vienna, Belgrade
  lng = c(16.3738, 20.4633)    # Vienna, Belgrade
)

# Snap the points to the nearest valid locations on the map
updated_coords <- snap_points_on_map(coords)

# Print data frame with original and new coordinates
print(updated_coords)
```
The `snap_points_on_map()` function will return an updated data frame with the snapped coordinates, adjusting the original points to valid locations based on the underlying map data.
#### 4. License

You can include a section on the license under which your package is distributed. For example:

## License

This package is licensed under the MIT License.

## Acknowledgments

Funding was provided by the European Union’s Horizon Europe research and innovation programme through the project DANUBE4all (grant agreement no. 101093985).
