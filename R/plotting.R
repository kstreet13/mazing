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
#' @return 
#' Returns \code{NULL}, invisibly.
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
#' @param openings Locations in the maze where a certain wall(s) should not be
#'   drawn (only applies when \code{walls = TRUE}). May be specified as
#'   coordinates, or as a relative description (see
#'   \code{\link{find_maze_refpoint}})
#' @param openings_direction Character vector describing which walls should not
#'   be drawn at the locations specified by \code{openings}. See Details.
#'   
#' @details The \code{openings_direction} argument specifies where to create an
#'   opening (ie. where not to draw walls), relative to a location in the maze
#'   given by \code{openings}. Possible values are: \code{"left"},
#'   \code{"right"}, \code{"top"}, \code{"bottom"}, \code{"topleft"},
#'   \code{"topright"}, \code{"bottomright"}, \code{"bottomleft"}, and
#'   \code{"all"}. If not specified, this will be set as the first value in
#'   \code{c("left","right","top","bottom")} to reference a wall that would
#'   otherwise have been drawn.
#' 
#' @importFrom graphics lines
#' @export
lines.maze <- function(x, walls = FALSE, adjust = c(0,0),
                       openings = NULL, openings_direction = NULL, ...){
    if(walls){
        MAZE <- maze2binary(x)
        if(!is.null(openings)){
            op <- 2 * matrix(find_maze_refpoint(openings, x), ncol = 2)
            if(is.null(openings_direction)){
                openings_direction <- sapply(seq_along(openings), function(i){
                    poss <- which(adjacent(op[i,2:1], MAZE) == 0)
                    if(2 %in% poss) return('left')
                    if(4 %in% poss) return('right')
                    if(3 %in% poss) return('top')
                    if(1 %in% poss) return('bottom')
                })
            }
            if(length(openings_direction) > nrow(op)){
                stop('More opening directions specified (',
                     length(openings_direction),
                     ') than openings (', nrow(op), ')')
            }
            if(length(openings_direction) < nrow(op)){
                openings_direction <- rep(openings_direction, 
                                          length.out = nrow(op))
            }
            for(i in seq_along(openings)){
                if(openings_direction[i] %in% c('left','topleft',
                                          'bottomleft','all')){
                    MAZE[op[i,2], op[i,1]-1] <- 1
                }
                if(openings_direction[i] %in% c('right','topright',
                                          'bottomright','all')){
                    MAZE[op[i,2], op[i,1]+1] <- 1
                }
                if(openings_direction[i] %in% c('top','topleft',
                                          'topright','all')){
                    MAZE[op[i,2]+1, op[i,1]] <- 1
                }
                if(openings_direction[i] %in% c('bottom','bottomright',
                                          'bottomleft','all')){
                    MAZE[op[i,2]-1, op[i,1]] <- 1
                }
            }
            
        }
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

