
# crookR

`crookR` is a tool to simulate “crook” deformations on tree stems from
LiDAR-derived stem point clouds.

## Installation

You can install the package like:

``` r
install.packages("crookR")
#> Installing package into 'C:/Users/rads0001/AppData/Local/Temp/RtmpOmwD5A/temp_libpatha8504bf4204f'
#> (as 'lib' is unspecified)
#> Warning: package 'crookR' is not available for this version of R
#> 
#> A version of this package for your version of R might be available elsewhere,
#> see the ideas at
#> https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
```

## Example

``` r
library(crookR)
library(lidR)

data(example_stem)

Crooked_Stem <- crook_deform(example_stem)

#Turn the data.table into a LAS file for visualization
Crooked_Stem_PC <- LAS(Crooked_Stem)
#> Creation of a LAS object from data but without a header:
#> Scale factors were set to 0.001 and XYZ coordinates were quantized to fit the scale factors.

plot(Crooked_Stem_PC)
```
