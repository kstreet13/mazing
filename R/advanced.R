#################
### advanced maze manipulation
#################
# if you've made it this far:
# These operate on mazes in the binary representation and I used these to make a
# maze-within-a-maze.
# expand: double the size of a maze (each wall and path becomes two rows/columns
# wide).
expand <- function(m){
    m2 <- matrix(NA, nrow = 2*nrow(m), ncol = 2*ncol(m))
    for(i in 1:ncol(m)){
        m2[,2*i-1] <- m2[,2*i] <- rep(m[,i], each = 2)
    }
    return(m2)
}
# seep: let the paths "seep" into the walls. For making a maze-within-a-maze,
# the walls are not interesting, but the paths are *very* interesting, so
# there's no reason not to make the paths wider than the walls. Expand twice,
# seep once.
seep <- function(m, what = 1){
    m2 <- m
    for(i in 2:nrow(m)){
        m2[i, ][m[i-1, ]==what] <- what
    }
    for(i in 1:(nrow(m)-1)){
        m2[i, ][m[i+1, ]==what] <- what
    }
    for(j in 2:ncol(m)){
        m2[,j][m[,j-1]==what] <- what
    }
    for(j in 1:(ncol(m)-1)){
        m2[,j][m[,j+1]==what] <- what
    }
    return(m2)
}
# condense: this one's for making mazes from images (ie. PNG files via readPNG).
# They might be too big, so you can condense them by a factor of 2 and take the
# average within each cell (previously 4 cells)
condense <- function(m, fun = median){
    m2 <- matrix(NA, nrow = ceiling(nrow(m)/2), ncol = ceiling(ncol(m)/2))
    for(i in 1:(nrow(m2))){
        for(j in 1:(ncol(m2))){
            i.2 <- (2*i-1):(2*i)
            j.2 <- (2*j-1):(2*j)
            if(i == nrow(m2)){
                i.2 <- (nrow(m)-1):nrow(m)
            }
            if(j == ncol(m2)){
                j.2 <- (ncol(m)-1):ncol(m)
            }
            m2[i,j] <- fun(m[i.2 , j.2])
        }
    }
    return(round(m2))
}



