set.seed(1)

test_that("basic maze functionality works", {
    m <- maze(10,10)
    expect_true(is.matrix(m))
    expect_true(is.maze(m))
    
    m <- matrix(1, nrow = 10, ncol = 10)
    expect_true(is.matrix(m))
    expect_false(is.maze(m))
    m <- as.maze(m)
    expect_true(is.maze(m))
    m2 <- as.maze(m)
    expect_identical(m, m2)
    
    # test return.coords (not currently used, but might want it at some point)
    adj <- mazing:::adjacent(c(2,2), m, return.coords = TRUE)
    expect_true(all(adj[,1] == c(3,1,2,2)))
    expect_true(all(adj[,2] == c(2,2,1,3)))
})

test_that("pathfinding works as expected", {
    m <- maze(10,10)
    p <- solve_maze(m)
    expect_true(is.matrix(p))
    
    pnames <- c('mtopleft','mtop','mtopright',
                'mrighttop','mright','mrightbottom',
                'mbottomright','mbottom','mbottomleft',
                'mleftbottom','mleft','mlefttop',
                'mcenter',
                'topleft','top','topright',
                'righttop','right','rightbottom',
                'bottomright','bottom','bottomleft',
                'leftbottom','left','lefttop',
                'center')
    pts <- find_maze_refpoint(pnames, m)
    expect_true(all(dim(pts) == c(length(pnames),2)))
    
    pts <- find_maze_refpoint(c(1,2), m)
    expect_true(is.matrix(pts))
    expect_equal(ncol(pts), 2)
    expect_true(all(pts[,] == 1:2))
    
    pts <- find_maze_refpoint(matrix(1:4, 2), m)
    expect_true(is.matrix(pts))
    expect_equal(ncol(pts), 2)
    expect_true(all(pts[,] == 1:4))
    
    p2 <- solve_maze(m, start = c(1,1), end = c(10,10))
    expect_true(all(p == p2))
    
    p3 <- solve_maze(m, start = 'mbottomleft', end = 'mrighttop')
    expect_true(all(p == p2))
    
    p4 <- solve_maze(m, start = 'topright', end = 'bottomleft')
    expect_true(all(p == p4[nrow(p4):1, ]))
    
    m <- matrix(1, nrow = 10, ncol = 10)
    m <- cbind(m, 0,0,0, m)
    m <- as.maze(m)
    expect_error(solve_maze(m),
                 "path between 'start' and 'end' could not be found")
    
    # with reasonably high probability, this will hit the while() statement
    # at the end of solve_maze
    m <- matrix(1, nrow = 10, ncol = 10)
    m <- rbind(0,0,m)
    m[1,4:6] <- 1
    m[2, 5] <- 1
    m <- as.maze(m)
    p <- solve_maze(m, start = 'mbottomleft', end = 'mbottomright')
    expect_true(all(p[,1] == 4:6))
    expect_true(all(p[,2] == 1))
})

test_that("plotting and printing functions do not give errors", {
    m <- maze(10, 10)
    
    expect_invisible(print(m))
    # can't find a way to test this and the matrix print method

    expect_invisible(plot(m))
    expect_invisible(plot(m, walls = TRUE))
    expect_invisible(plot(m, adjust = c(.5,.5)))
    expect_invisible(plot(m, walls = TRUE, adjust = c(.5,.5)))
    expect_invisible(plot(m, walls = TRUE, openings = c('left','right')))
    expect_invisible(plot(m, walls = TRUE, openings = c('left','right'),
                          openings_direction = c('topleft','bottomright')))
    expect_invisible(plot(m, walls = TRUE, openings = c('left','right'),
                          openings_direction = 'all'))
    expect_error(plot(m, walls = TRUE, openings = c('left','right'),
                      openings_direction = c('top','top','top')), 
                 'More opening directions specified')
    # contrived mazes to hit the openings_direction = 'top' / 'bottom' 
    # cases without specifying
    mat <- matrix(1, nrow = 2, ncol = 3)
    mat[1,1] <- mat[1,3] <- 0
    m <- as.maze(mat)
    expect_invisible(plot(m, walls = TRUE, openings = c(2,2)))
    m <- as.maze(mat[2:1,])
    expect_invisible(plot(m, walls = TRUE, openings = c(2,1)))
})

test_that("advanced maze/matrix manipulation works", {
    m <- maze(10, 10)
    m <- maze2binary(m)
    expect_true(is.matrix(m))
    expect_true(all(m %in% 0:1))
    
    m2 <- expand_matrix(m)
    expect_true(all(dim(m2) == 2*dim(m)))
    eps <- ifelse(capabilities("long.double"),
                  sqrt(.Machine$double.eps), 0.1)
    expect_equal(mean(m), mean(m2), tolerance = eps)
    
    m3 <- widen_paths(m2)
    expect_true(all(dim(m3) == dim(m2)))
    expect_true(mean(m3) > mean(m2))
    
    m4 <- widen_paths(m2, square_corners = TRUE)
    expect_true(all(dim(m4) == dim(m2)))
    expect_true(mean(m4) > mean(m3))
    
    m3 <- condense_matrix(m2)
    expect_true(all(dim(m3) == dim(m2)/2))
    
    m2[,5] <- -5
    m3 <- widen_paths(m2, blocked = -5)
    expect_true(all(dim(m3) == dim(m2)))
    expect_equal(sum(m2 == -5), sum(m3 == -5))
    expect_true(mean(m3) > mean(m2))
    
})
