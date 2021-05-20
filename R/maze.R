#' @title Generate a random maze
#' @name maze
#' @description 
#' A function that produces a rectangular, procedurally generated random maze of
#' a given size.
#' 
#' @param nrow Number of rows.
#' @param ncol Number of columns.
#' 
#' @return An object of class \code{maze}, which is a subclass of \code{matrix}.
#' @examples
#' maze(10,10)
#' @export
maze <- function(nrow, ncol){
    m <- matrix(-1, nrow, ncol)
    m <- fill_maze(m)
    class(m) <- 'maze'
    return(m)
}


#' @title Convert matrix to maze
#' @name as.maze
#' @description 
#' A function to convert a matrix into a \code{maze} object. Binary matrices can
#' be used to make mazes in interesting, non-rectangular shapes.
#' 
#' @param x A matrix-like object to be made into a maze.
#' 
#' @details If \code{x} is a binary matrix (or otherwise contains only two
#'   unique values), the maze will be constrained to occupy only those cells
#'   containing a 1 (the higher value).
#' 
#' @examples 
#' mat <- matrix(NA, 10, 10)
#' m <- as.maze(mat)
#' 
#' # circular maze
#' mat <- matrix(1, 30, 30)
#' for(i in 1:nrow(mat)){
#'     for(j in 1:ncol(mat)){
#'         if((i-15.5)^2+(j-15.5)^2 > 220){
#'             mat[i,j] <- 0
#'         }
#'     }
#' }
# m <- as.maze(mat)
#' @export
as.maze <- function(x){
    m <- matrix(-1, nrow = nrow(x), ncol = ncol(x))
    un <- unique(as(x, typeof(x)))
    if(length(un) == 2){
        vals <- sort(un)
        m[x == vals[1]] <- -5
    }
    m <- fill_maze(m)
    class(m) <- 'maze'
    return(m)
}


