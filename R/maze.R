#' @title Random Mazes
#' @name maze
#' @aliases is.maze as.maze
#' @description 
#' Functions for producing and identifying procedurally generated random mazes.
#' 
#' @param nrow Number of rows.
#' @param ncol Number of columns.
#' 
#' @return 
#' For \code{maze} and \code{as.maze}, an object of class \code{maze}, which is
#' a subclass of \code{matrix}.
#' 
#' @return For \code{is.maze} a logical value indicating if the input is a valid
#' \code{maze}.
#' 
#' @examples
#' maze(10,10)
#' @export
maze <- function(nrow, ncol){
    m <- matrix(-1, nrow, ncol)
    m <- fill_maze(m)
    class(m) <- 'maze'
    return(m)
}

#' @rdname maze
#' 
#' @examples 
#' mat <- matrix(NA, 10, 10)
#' is.maze(mat)
#' m <- as.maze(mat)
#' is.maze(mat)
#' 
#' @importFrom methods is
#' @export
is.maze <- function(x){
    if(is.matrix(x) && all(x %in% c(-5,-1:4))){
        return(TRUE)
    }
    return(FALSE)
}

#' @rdname maze
#' 
#' @param x A matrix-like object to be made into a maze (for \code{as.maze}), or
#'   an object to be identified as either a \code{maze} or not (for
#'   \code{is.maze})
#' 
#' @details For \code{as.maze}, if \code{x} is a binary matrix (or otherwise
#'   contains only two unique values), the maze will be constrained to occupy
#'   only those cells containing a 1 (the higher value).
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
#' m <- as.maze(mat)
#' 
#' @importFrom methods as
#' @export
as.maze <- function(x){
    stopifnot(is.matrix(x))
    if(is.maze(x)){ return(x) }
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


