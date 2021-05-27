#' @title Double the dimensions of a matrix
#' @name expand_matrix
#' @description 
#' A function to double the size of a matrix, with each cell in the original
#' becoming four cells in the expanded matrix.
#' 
#' @param m A \code{matrix}.
#' 
#' @return 
#' A matrix with the same distribution of values as the original, but doubled in
#' size along both dimensions.
#' 
#' @examples
#' m <- matrix(1:6, nrow = 2)
#' expand_matrix(m)
#' 
#' @export
expand_matrix <- function(m){
    m2 <- matrix(NA, nrow = 2*nrow(m), ncol = 2*ncol(m))
    for(i in 1:ncol(m)){
        m2[,2*i-1] <- m2[,2*i] <- rep(m[,i], each = 2)
    }
    return(m2)
}


#' @title Spread a value within a matrix
#' @name widen_paths
#' @description 
#' A function to spread a particular value into neighboring cells of a matrix.
#' When that matrix is a binary representation of a maze, this can be used to
#' widen the paths of the maze (subsequently narrowing the walls).
#'
#' @param m A \code{matrix}.
#' @param value The value to be expanded into neighboring cells. For widening
#'   the paths of the maze, this is the value that represents paths.
#' @param blocked Values that should be preserved, regardless of whether or not
#'   the expansion would normally encroach on their cells.
#' @param square_corners Whether or not to include diagonals in the expansion.
#'   Including diagonals will preserve square corners, whereas excluding them
#'   (default) creates more rounded looking corners.
#' 
#' @details The idea of this function is to let the paths of a maze "seep" into
#'   the walls. For making a maze-within-a-maze, the walls of the big maze are
#'   generally not as interesting as the paths, which contain smaller paths.
#'   Hence why we might want to make the paths wider than the walls. Remember
#'   that this will affect walls from both sides, so it is necessary to expand
#'   the matrix twice (via \code{\link{expand_matrix}}) before widening the
#'   paths for the first time.
#'   
#' @return 
#' A matrix with the same dimensions as the original, in which cells that border a cell of a particular \code{value} have been assigned that \code{value}.
#' 
#' @examples
#' m <- matrix(0, nrow = 5, ncol = 5)
#' m[3,3] <- 1
#' widen_paths(m)
#' widen_paths(m, square_corners = TRUE)
#' 
#' m[,2] <- 2
#' widen_paths(m, blocked = 2)
#' 
#' @export
widen_paths <- function(m, value = 1, blocked = NULL, square_corners = FALSE){
    m2 <- m
    if(square_corners){
        for(i in nrow(m2):2){
            m2[i, ][m2[i-1, ]==value] <- value
        }
        for(i in 1:(nrow(m2)-1)){
            m2[i, ][m2[i+1, ]==value] <- value
        }
        for(j in ncol(m2):2){
            m2[,j][m2[,j-1]==value] <- value
        }
        for(j in 1:(ncol(m2)-1)){
            m2[,j][m2[,j+1]==value] <- value
        }
    }else{
        for(i in 2:nrow(m)){
            m2[i, ][m[i-1, ]==value] <- value
        }
        for(i in 1:(nrow(m)-1)){
            m2[i, ][m[i+1, ]==value] <- value
        }
        for(j in 2:ncol(m)){
            m2[,j][m[,j-1]==value] <- value
        }
        for(j in 1:(ncol(m)-1)){
            m2[,j][m[,j+1]==value] <- value
        }
    }
    m2[m %in% blocked] <- m[m %in% blocked]
    return(m2)
}

# condense: this one's for making mazes from images (ie. PNG files via readPNG).
# They might be too big, so you can condense them by a factor of 2 and take the
# average within each cell (previously 4 cells)

#' @title Halve the dimensions of a matrix
#' @name condense_matrix
#' @description 
#' A function to decrease the size of a matrix while attempting to preserve the
#' original values.
#'
#' @param m A \code{matrix}.
#' @param fun A function for summarizing four values that condenses them down to
#'   one value. Default is \code{\link[stats]{median}}.
#' 
#' @details This function can be used for image manipulation and is included for
#'   the purpose of making mazes from images. PNG images can be read in as
#'   arrays via \code{png::readPNG} and if the image is too big, this funtion
#'   can help get it down to a more manageable size.
#'   
#' @return 
#' A matrix with dimensions half the size of the original matrix, where each
#' value is a summary of the four corresponding values in the original.
#' 
#' @examples
#' m <- outer(1:4, 1:6)
#' condense_matrix(m)
#' 
#' @export
condense_matrix <- function(m, fun = median){
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

