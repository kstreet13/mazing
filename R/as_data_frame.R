#' Data frame coercion (for plotting)
#'
#' [as.data.frame()] methods to coerce [maze()] objects 
#' or their solutions into data frames suitable for plotting with 
#' `grid` or `ggplot2` graphics systems.
#'
#' @param x A [maze()] object.
#' @param walls logical value, indicating that the walls of the maze should be
#'   plotted, rather than the paths through the maze. Default is \code{FALSE}.
#' @param method Target `grid` or `ggplot2` plotting method as a string.
#'               Some `method` values may only work for either the maze object or its solution.
#' @param start,end The coordinates of the starting and ending points of a path
#'        through the maze, or descriptions of relative locations (eg.
#'        `"topleft"`, see [find_maze_refpoint()]. If not provided, no path will be drawn.
#' @param ... Currently ignored.
#' @examples
#' x <- maze(12L, 12L)
#' dfw <- as.data.frame(x, walls = TRUE)
#' dfs <- as.data.frame(x, start = "top", end = "bottom")
#' if (require("ggplot2", quietly = TRUE)) {
#'    ggplot(dfw, aes(x, y)) + geom_tile() + coord_equal() + theme_void() + 
#'    geom_path(aes(x=x, y=y), colour = "red", linewidth = 2, data = dfs)
#' }
#' @return A data frame.
#' @seealso [plot.maze()], [grid::pointsGrob()]
#' @rdname as.data.frame.maze
#' @export
as.data.frame.maze <- function(x, ...,
                               walls = FALSE,
                               method = c("geom_point", "geom_path", "geom_tile"),
                               start = NULL, end = NULL) {
    method <- match.arg(method)
    if (!is.null(start) && !is.null(end)) {
        s <- solve_maze(x, start, end)
        switch(method,
               geom_path = as_df_solution_geom_point(s),             
               geom_point = as_df_solution_geom_point(s),             
               stop(paste("method =", shQuote(method), "not supported for maze solutions"))
        )
    } else {
        switch(method,
               geom_point = as_df_maze_geom_point(x, walls = walls),
               geom_tile = as_df_maze_geom_point(x, walls = walls),
               stop(paste("method =", shQuote(method), "not supported for maze objects"))
        )
    }
}

as_df_solution_geom_point <- function(x) {
    data.frame(x = x[, 1], y = x[, 2])
}

as_df_maze_geom_point <- function(x, walls = FALSE) {
    m <- maze2binary(x)
    if (walls) {
        m[, ] <- !m
    } 
    xs <- seq_len(ncol(m)) / 2
    ys <- seq_len(nrow(m)) / 2
    l <- lapply(seq_len(nrow(m)), function(i)  {
        if (any(as.logical(m[i, ] == 1L)))
            data.frame(x = xs[which(m[i, ] == 1L)], y = ys[i])
        else
            data.frame(x = numeric(0L), y = numeric(0L))
    })
    do.call(rbind, l)
}
