
# mazing

<img src=inst/mazing.png height="300">

This package provides utilities for generating and plotting random
mazes. The mazes are based on matrices, so can only consist of vertical
and horizontal lines along a regular grid. But there is no need to use
every possible space, so they can take on many different shapes.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("kstreet13/mazing")
```

## Example

``` r
library(mazing)

mat <- matrix(1, nrow = 19, ncol = 24)
mat <- cbind(0,0,0,0,mat,0,0,0,0)
mat[10,] <- 1

m <- as.maze(mat)
plot(m, walls = TRUE)

p <- solve_maze(m, start = 'left', end = 'right')
lines(p, col = 2, lwd = 3)
```

![](README_files/figure-gfm/example-1.png)<!-- -->

## Issues and bug reports

Please use <https://github.com/kstreet13/mazing/issues> to submit
issues, bug reports, and comments.
