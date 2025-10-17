
# crookR

`crookR` is a tool to simulate “crook” deformations on tree stems from
LiDAR-derived stem point clouds.

## Installation

You can install the package like:

``` r
install.packages("crookR")

```

## Example

``` r
library(crookR)
library(lidR)

data(example_stem)

Crooked_Stem <- crook_deform(example_stem)

#Turn the data.table into a LAS file for visualization
Crooked_Stem_PC <- LAS(Crooked_Stem)


plot(Crooked_Stem_PC)
```
