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
#' @seealso [print.maze()]
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
    if(is.matrix(x) && is(x,'maze') && all(x %in% c(-5,-1:4))){
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


#' @title Print Maze
#' @name print.maze
#' @description 
#' Display a \code{maze} object.
#' 
#' @param x A \code{maze} object.
#' @param walls logical value, indicating that the walls of the maze should be
#'   plotted, rather than the paths through the maze. Default is \code{FALSE}.
#' @param start,end The coordinates of the starting and ending points of a path
#'   through the maze, or descriptions of relative locations (eg.
#'   \code{"topleft"}, see \code{\link{find_maze_refpoint}}). If not provided,
#'   no path will be drawn.
#' @param compress How to compress the bitmap representation, see
#'   \code{\link[bittermelon]{print.bm_bitmap}}. For \code{maze} objects, the
#'   default is \code{"v"} for vertical.
#' @param ... Further arguments passed to
#'   \code{\link[bittermelon]{print.bm_bitmap}}, if the \code{bittermelon}
#'   package is available.
#' 
#' @details If the \code{\link[bittermelon]{bittermelon}} package is available,
#'   \code{print.maze} prints a representation of the maze az a bitmap object.
#'   Otherwise, it prints the matrix representation.
#' 
#' @examples 
#' m <- maze(10,10)
#' print(m)
#' 
#' @importFrom utils packageVersion
#' @export
print.maze <- function(x, ..., walls = FALSE, start = NULL, end = NULL, 
                       compress = "v") {
  if(requireNamespace("bittermelon", quietly = TRUE) &&
     packageVersion("bittermelon") >= "1.2.0-2"){
    bm <- bittermelon::as_bm_bitmap(x, walls = walls, start = start, end = end)
    print(bm, compress = compress, ...)
  }else{
    print.default(x)
  }
}


