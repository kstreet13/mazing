#' @title Get coordinates for a point in a maze
#' @name find_maze_refpoint
#' @description 
#' A function that takes a description of a point in a maze and finds the matrix
#' indices corresponding to that point.
#' 
#' @param point A description of a relative position in the maze, such as
#'   \code{"topleft"} or \code{"bottomright"}. See Details for all possible
#'   values.
#' @param maze A \code{\link{maze}} object.
#' 
#' @details 
#' For standard values of \code{point}, this function will identify a "target"
#' (such as the top left corner of the matrix) and select the point in the maze
#' that is closest to that target, by Euclidean distance. The standard choices
#' for \code{point} are: \code{"topleft"}, \code{"top"}, \code{"topright"},
#' \code{"righttop"}, \code{"right"}, \code{"rightbottom"},
#' \code{"bottomright"}, \code{"bottom"}, \code{"bottomleft"},
#' \code{"leftbottom"}, \code{"left"}, \code{"lefttop"}, and \code{"center"}.
#' For convenience, there are several redundancies built in; for example,
#' \code{"topleft"} is identical to \code{"lefttop"}.
#' 
#' @details 
#' In addition to the standard values, there is a complementary set of "matrix"
#' or "manhattan-like" values, each of which is prepended with an \code{"m"}
#' (for example, \code{"mtopleft"}). These options select the most extreme value
#' along one dimension (ie. the highest possible row), fix that value, and then
#' select the local extreme in the other dimension (column). For example, the
#' value \code{"mtopleft"} will select the highest possible row in the maze
#' before selecting the left-most point in that row. Note that this means values
#' such as \code{"mtopleft"} and \code{"mlefttop"} are not synonymous.
#' 
#' @details For convenience, if \code{point} is a 2-column matrix or numeric
#'   vector that can be coerced into a 2-column matrix, it will be returned as
#'   such, with minor formatting changes to match the usual output. This is so
#'   that functions which rely on \code{find_maze_refpoint} can use either
#'   relative descriptions or exact coordinates.
#' 
#' @return A matrix of integers with 2 columns, giving the coordinates of the
#'   desired point(s). Note that the x-coordinate (column index) comes first, so
#'   for the corresponding index in the original matrix, these coordinates will
#'   need to be reversed.
#'   
#' @examples
#' m <- maze(15,15)
#' r <- find_maze_refpoint('topright', m)
#' 
#' plot(m, walls = TRUE)
#' points(r[1], r[2], col = 2, pch = 16)
#' 
#' @importFrom stats median
#' @export
find_maze_refpoint <- function(point, maze){
    if(is.numeric(point)){
        if(is.matrix(point)){
            stopifnot(ncol(point) == 2)
        }else{
            point <- matrix(point, ncol = 2)
        }
        colnames(point) <- c('col','row')
        return(point)
    }
    if(length(point) > 1){
        return(t(sapply(point, find_maze_refpoint, maze)))
    }
    stopifnot(point %in% c('mtopleft','mtop','mtopright',
                           'mrighttop','mright','mrightbottom',
                           'mbottomright','mbottom','mbottomleft',
                           'mleftbottom','mleft','mlefttop',
                           'mcenter',
                           'topleft','top','topright',
                           'righttop','right','rightbottom',
                           'bottomright','bottom','bottomleft',
                           'leftbottom','left','lefttop',
                           'center'))
    # distance to target
    if(point %in% c('topleft','top','topright',
                    'righttop','right','rightbottom',
                    'bottomright','bottom','bottomleft',
                    'leftbottom','left','lefttop',
                    'center')){
        target <- switch(point,
                         'topleft' = c(nrow(maze), 1),
                         'top' = c(nrow(maze), (ncol(maze)+1)/2),
                         'topright' = c(nrow(maze), ncol(maze)),
                         'righttop' = c(nrow(maze), ncol(maze)),
                         'right' = c((nrow(maze)+1)/2, ncol(maze)),
                         'rightbottom' = c(1, ncol(maze)),
                         'bottomright' = c(1, ncol(maze)),
                         'bottom' = c(1, (ncol(maze)+1)/2),
                         'bottomleft' = c(1, 1),
                         'leftbottom' = c(1, 1),
                         'left' = c((nrow(maze)+1)/2, 1),
                         'lefttop' = c(nrow(maze), 1),
                         'center' = c((nrow(maze)+1)/2, (ncol(maze)+1)/2))
        D <- outer((seq_len(nrow(maze)) - target[1])^2,
                   (seq_len(ncol(maze)) - target[2])^2, FUN = "+")
        D[maze == -5] <- Inf
        ind <- which(D == min(D), arr.ind=TRUE)[1,]
        return(ind[2:1])
    }
    # else
    if(point %in% c('mtopleft','mtop','mtopright')){
        ind <- c(NA, NA)
        valid_rows <- which(apply(maze, 1, function(x) {any(x != -5)}))
        ind[1] <- max(valid_rows)
        valid_cols <- which(maze[ind[1], ] != -5)
        ind[2] <- switch(point,
                         'mtopleft' = min(valid_cols),
                         'mtopright' = max(valid_cols),
                         'mtop' = round(median(valid_cols)))
    }
    if(point %in% c('mrighttop','mright','mrightbottom')){
        ind <- c(NA, NA)
        valid_cols <- which(apply(maze, 2, function(x) {any(x != -5)}))
        ind[2] <- max(valid_cols)
        valid_rows <- which(maze[,ind[2]] != -5)
        ind[1] <- switch(point,
                         'mrightbottom' = min(valid_rows),
                         'mrighttop' = max(valid_rows),
                         'mright' = round(median(valid_rows)))
    }
    if(point %in% c('mbottomright','mbottom','mbottomleft')){
        ind <- c(NA, NA)
        valid_rows <- which(apply(maze, 1, function(x) {any(x != -5)}))
        ind[1] <- min(valid_rows)
        valid_cols <- which(maze[ind[1], ] != -5)
        ind[2] <- switch(point,
                         'mbottomleft' = min(valid_cols),
                         'mbottomright' = max(valid_cols),
                         'mbottom' = round(median(valid_cols)))
    }
    if(point %in% c('mleftbottom','mleft','mlefttop')){
        ind <- c(NA, NA)
        valid_cols <- which(apply(maze, 2, function(x) {any(x != -5)}))
        ind[2] <- min(valid_cols)
        valid_rows <- which(maze[,ind[2]] != -5)
        ind[1] <- switch(point,
                         'mleftbottom' = min(valid_rows),
                         'mlefttop' = max(valid_rows),
                         'mleft' = round(median(valid_rows)))
    }
    if(point == 'mcenter'){
        ind <- c(NA, NA)
        valid_rows <- which(apply(maze, 1, function(x) {any(x != -5)}))
        ind[1] <- round(median(valid_rows))
        valid_cols <- which(maze[ind[1], ] != -5)
        ind[2] <- round(median(valid_cols))
    }
    return(ind[2:1])
}



#' @title Find a path through a maze
#' @name solve_maze
#' @description 
#' A function that finds the shortest path between points in a maze.
#' 
#' @param maze A \code{\link{maze}} object.
#' @param start The coordinates of the starting point, or a description of a
#'   relative location (see \code{\link{find_maze_refpoint}}). If not provided,
#'   this will be as close as possible to the bottom left corner.
#' @param end The coordinates of the end point, or a description of a relative
#'   location (see \code{\link{find_maze_refpoint}}). If not provided, this will
#'   be as close as possible to the top right corner.
#' 
#' @details For the \code{start} and \code{end} arguments (as well as the output
#'   matrix), these coordinates refer to the plotting coordinates, not the
#'   matrix indices. For plotting, the x-coordinate (column index) is listed
#'   first, whereas in matrix notation, the row (y-coordinate) is listed first.
#' 
#' @return A \code{matrix} containing the coordinates of the path through the
#'   maze. Note that the x-coordinate (column index) comes first, so for the
#'   corresponding indices in the original matrix, these coordinates will need
#'   to be reversed.
#'   
#' @examples
#' m <- maze(15,15)
#' p <- solve_maze(m)
#' 
#' plot(m, walls = TRUE)
#' lines(p, col = 2, lwd = 3)
#' 
#' @export
solve_maze <- function(maze, start='bottomleft', end='topright'){
    if(is.numeric(start)){
        stopifnot(length(start) == 2)
        start <- rev(start)
        stopifnot(maze[start[1],start[2]] != -5)
    }
    if(is.numeric(end)){
        stopifnot(length(end) == 2)
        end <- rev(end)
        stopifnot(maze[end[1],end[2]] != -5)
    }
    if(is.character(start)){ start <- find_maze_refpoint(start, maze)[2:1] }
    if(is.character(end)){ end <- find_maze_refpoint(end, maze)[2:1] }    
    
    # p1: start -> root
    p1 <- matrix(start, ncol = 2)
    parent <- previous(start, maze)
    while(!anyNA(parent)){
        p1 <- rbind(p1, parent)
        parent <- previous(parent, maze)
    }
    p1.text <- paste(p1[,1], p1[,2])
    end.text <- paste(end[1], end[2])
    if(end.text %in% p1.text){
        return(p1[1:which.max(p1.text==end.text), 2:1])
    }
    # p2: end -> root
    p2 <- matrix(end, ncol = 2)
    parent <- previous(end, maze)
    while(!anyNA(parent)){
        p2 <- rbind(p2, parent)
        parent <- previous(parent, maze)
    }
    p2.text <- paste(p2[,1], p2[,2])
    start.text <- paste(start[1], start[2])
    if(start.text %in% p2.text){
        return(p2[which.max(p2.text==start.text):1, 2:1])
    }
    if(p1.text[length(p1.text)] != p2.text[length(p2.text)]){
        stop("path between 'start' and 'end' could not be found")
    }
    while(p1.text[length(p1.text)] == p2.text[length(p2.text)]){
        last <- p1.text[length(p1.text)]
        p1.text <- p1.text[-length(p1.text)]
        p2.text <- p2.text[-length(p2.text)]
    }
    path.text <- c(p1.text, last, rev(p2.text))
    path <- t(vapply(path.text, function(x){
        as.numeric(unlist(strsplit(x, split = ' ')))
    }, c(1,1)))
    rownames(path) <- NULL
    return(path[,c(2,1)])
}
