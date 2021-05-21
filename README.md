
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

    ##      checking for file ‘/private/var/folders/bs/ywxfn51s0d7cb61dk5r5ys1h0000gn/T/RtmpdsqEsd/remotes65979780914/kstreet13-mazing-5316aab/DESCRIPTION’ ...  ✓  checking for file ‘/private/var/folders/bs/ywxfn51s0d7cb61dk5r5ys1h0000gn/T/RtmpdsqEsd/remotes65979780914/kstreet13-mazing-5316aab/DESCRIPTION’ (361ms)
    ##   ─  preparing ‘mazing’:
    ##      checking DESCRIPTION meta-information ...  ✓  checking DESCRIPTION meta-information
    ##   ─  checking for LF line-endings in source and make files and shell scripts
    ##   ─  checking for empty or unneeded directories
    ##   ─  building ‘mazing_0.1.tar.gz’
    ##      
    ## 

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
