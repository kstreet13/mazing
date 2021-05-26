#' @title Plot a maze object
#' @name plot.maze
#' 
#' @param x A \code{\link{maze}} object.
#' @param walls logical value, indicating that the walls of the maze should be
#'   plotted, rather than the paths through the maze. Default is \code{FALSE}.
#' @param ... Additional arguments passed to \code{\link[graphics]{lines}}
#' 
#' @details 
#' When plotting, the coordinates for locations in the maze are taken from the
#' indices of the corresponding locations in the matrix representation. This
#' means that the plot will appear to be flipped vertically relative to the
#' matrix representation of the maze.
#' 
#' @examples 
#' m <- maze(10,10)
#' plot(m, walls = TRUE)
#' lines(m, lwd = 5, col = 3)
#' @export
plot.maze <- function(x, walls = FALSE, ...){
    if(walls){
        plot(c(0,ncol(x)+.5), c(0,nrow(x)+.5), 
             col = 'white', asp = 1, axes=FALSE,
             xlab = '', ylab = '')
        lines.maze(x, walls = TRUE, ...)
    }else{ # paths
        plot(c(1,ncol(x)), c(1,nrow(x)), 
             col = 'white', asp = 1, axes=FALSE,
             xlab = '', ylab = '')
        lines.maze(x, walls = FALSE, ...)
    }
}


#' @rdname plot.maze
#' @param adjust A vector by which to adjust the overall position of the maze.
#'   Should be a numeric vector of length 2, indicating the \code{x} and
#'   \code{y} adjustments, respectively.
#' @importFrom graphics lines
#' @export
lines.maze <- function(x, walls = FALSE, adjust = c(0,0), ...){
    if(walls){
        MAZE <- maze2binary(x)
        for(i in 1:nrow(MAZE)){
            for(j in 1:ncol(MAZE)){
                if(MAZE[i,j] == 0){
                    adj <- adjacent(c(i,j), MAZE)
                    vals <- adj[!is.na(adj)]
                    if(any(vals != 0)){
                        draw <- which(adj == 0)
                        for(dir in draw){
                            trgt <- switch(dir,
                                           '1' = c(i,j) + c(-1,0), # technically, the names are 
                                           '2' = c(i,j) + c(0,-1), # unnecessary. It's picking
                                           '3' = c(i,j) + c(1,0),  # the correct case based on 
                                           '4' = c(i,j) + c(0,1))  # position, not name
                            tadj <- c(adjacent(trgt, MAZE), diag_adj_vals(trgt, MAZE))
                            tvals <- tadj[!is.na(tadj)]
                            if(any(tvals != 0)){
                                lines(c(j,trgt[2])/2 + adjust[1], 
                                      c(i,trgt[1])/2 + adjust[2],  ...)
                            }
                        }
                        MAZE[i,j] <- 0
                    }
                }
            }
        }
    }else{
        for(i in 1:nrow(x)){
            for(j in 1:ncol(x)){
                if(x[i,j] != 0){
                    dir <- x[i,j]
                    # reverse the move that was taken to get here
                    prev <- switch(dir,
                                   '1' = c(i,j) + c(1,0),
                                   '2' = c(i,j) + c(0,1),
                                   '3' = c(i,j) + c(-1,0),
                                   '4' = c(i,j) + c(0,-1))
                    lines(c(j,prev[2]) + adjust[1], 
                          c(i,prev[1]) + adjust[2], ...)
                }
            }
        }
    }
}

