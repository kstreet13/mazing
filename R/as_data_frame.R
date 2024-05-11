#' Data frame coercion (for plotting)
#'
#' [as.data.frame()] methods to coerce [maze()] objects 
#' or their solutions into data frames suitable for plotting with 
#' the `ggplot2` graphics systems.
#'
#' @param x A [maze()] object.
#' @param walls logical value, indicating that the walls of the maze should be
#'   plotted, rather than the paths through the maze. Default is \code{FALSE}.
#' @param method Target `ggplot2` plotting method as a string.
#'               Some `method` values may only work for either the maze object or its solution
#'               i.e. `method = "geom_path"` only works for the solution.
#' @param start,end The coordinates of the starting and ending points of a path
#'        through the maze, or descriptions of relative locations (eg.
#'        `"topleft"`, see [find_maze_refpoint()]. If not provided, no path will be drawn.
#' @param ... Currently ignored.
#' @examples
#' x <- maze(12L, 12L)
#' dfw <- as.data.frame(x, walls = TRUE, method = "geom_tile")
#' dfs <- as.data.frame(x, start = "top", end = "bottom", method = "geom_path")
#' if (require("ggplot2", quietly = TRUE)) {
#'    ggplot(dfw, aes(x, y)) + geom_tile() + coord_equal() + theme_void() + 
#'    geom_path(aes(x=x, y=y), colour = "red", linewidth = 2, data = dfs)
#' }
#' @return A data frame with data for either plotting the maze's path, mazes's wall (if `wall = TRUE`),
#'         or the maze's solution (if `start` and `end` are not `NULL`) with the `ggplot2`
#'         method indicated by `method`.
#' @seealso [plot.maze()], [ggplot2::geom_point()], [ggplot2::geom_path()], 
#'          [ggplot2::geom_text()], [ggplot2::geom_tile()]
#' @rdname as.data.frame.maze
#' @export
as.data.frame.maze <- function(x, ...,
                               walls = FALSE,
                               method = c("geom_point", "geom_path", "geom_segment", "geom_text", "geom_tile"),
                               start = NULL, end = NULL) {
    method <- match.arg(method)
    if (!is.null(start) && !is.null(end)) {
        switch(method,
               geom_path = as_df_solution_geom_path(x, start, end),
               geom_point = as_df_solution_geom_point(x, start, end),
               geom_segment = as_df_solution_geom_segment(x, start, end),
               geom_text = as_df_solution_geom_point(x, start, end),
               geom_tile = as_df_solution_geom_point(x, start, end),
               stop(paste("method =", shQuote(method), "not supported for maze solutions"))
        )
    } else {
        switch(method,
               geom_point = as_df_maze_geom_point(x, walls = walls),
               geom_segment = as_df_maze_geom_segment(x, walls = walls),
               geom_text = as_df_maze_geom_point(x, walls = walls),
               geom_tile = as_df_maze_geom_point(x, walls = walls),
               stop(paste("method =", shQuote(method), "not supported for maze objects"))
        )
    }
}

as_df_solution_geom_path <- function(x, start, end) {
    s <- solve_maze(x, start, end, by = Inf)
    data.frame(x = s[, 1L], y = s[, 2L])
}

as_df_solution_geom_point <- function(x, start, end) {
    s <- solve_maze(x, start, end, by = 0.5)
    data.frame(x = s[, 1L], y = s[, 2L])
}

as_df_solution_geom_segment <- function(x, start, end) {
    s <- solve_maze(x, start, end, by = Inf)
    stopifnot(nrow(s) > 1L)
    n <- nrow(s)
    data.frame(x = s[seq.int(n-1L), 1L], y = s[seq.int(n-1L), 2L],
               xend = s[seq.int(2L, n), 1L], yend = s[seq.int(2L, n), 2L])
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

as_df_maze_geom_segment <- function(x, walls = FALSE) {
    m <- maze2binary(x)
    if (walls) {
        m[, ] <- !m
    } 
    xs <- seq_len(ncol(m)) / 2
    ys <- seq_len(nrow(m)) / 2
    # Vertical line segments
    lv <- lapply(seq_len(ncol(m)), function(i)  {
        y1 <- which(m[, i] == 1L)
        dy1 <- diff(y1)
        if (any(dy1 == 1)) {
            ystart <- numeric()
            yend <- numeric()
            for (k in seq_along(dy1)) {
                if (dy1[k] == 1 && (k == 1 || dy1[k-1] != 1))
                    ystart <- append(ystart, ys[y1[k]])
                if (dy1[k] == 1 && (k == length(dy1) || dy1[k+1] != 1))
                    yend <- append(yend, ys[y1[k+1]])
            }
            data.frame(x = xs[i], y = ystart, xend = xs[i], yend = yend)
        } else {
            data.frame(x = numeric(0L), y = numeric(0L), xend = numeric(0L), yend = numeric(0L))
        }
    })
    dfv <- do.call(rbind, lv)

    # Horizontal line segments
    lh <- lapply(seq_len(nrow(m)), function(i)  {
        x1 <- which(m[i, ] == 1L)
        dx1 <- diff(x1)
        if (any(dx1 == 1)) {
            xstart <- numeric()
            xend <- numeric()
            for (k in seq_along(dx1)) {
                if (dx1[k] == 1 && (k == 1 || dx1[k-1] != 1))
                    xstart <- append(xstart, xs[x1[k]])
                if (dx1[k] == 1 && (k == length(dx1) || dx1[k+1] != 1))
                    xend <- append(xend, xs[x1[k+1]])
            }
            data.frame(x = xstart, y = ys[i], xend = xend, yend = ys[i])
        } else {
            data.frame(x = numeric(0L), y = numeric(0L), xend = numeric(0L), yend = numeric(0L))
        }
    })
    dfh <- do.call(rbind, lh)
    rbind(dfv, dfh)
}
