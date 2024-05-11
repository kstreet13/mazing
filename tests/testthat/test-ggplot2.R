test_that("{ggplot2} helpers work", {
    skip_if_not_installed("ggplot2", "3.5.1")
    skip_if_not_installed("vdiffr", "1.0.7")
    skip_if_not_installed("withr")

    library("ggplot2")

    withr::with_seed(42, x <- maze(13, 13))
    vdiffr::expect_doppelganger("geom_path", {
        dfw <- as.data.frame(x, walls = TRUE, method = "geom_tile")
        dfs <- as.data.frame(x, start = "top", end = "bottom", method = "geom_path")
        ggplot()  + coord_equal() + theme_void() +
          geom_tile(aes(x, y), fill = "black", data = dfw) +
          geom_path(aes(x, y), linewidth = 3, colour = "red", data = dfs)
    })

    vdiffr::expect_doppelganger("geom_point", {
        dfw <- as.data.frame(x, walls = TRUE, method = "geom_point")
        dfp <- as.data.frame(x, walls = FALSE, method = "geom_point")
        dfs <- as.data.frame(x, start = "top", end = "bottom", method = "geom_point")
        ggplot()  + coord_equal() + theme_void() +
          geom_point(aes(x, y), size = 3, colour = "black", data = dfw) +
          geom_point(aes(x, y), size = 3, colour = "grey80", data = dfp) +
          geom_point(aes(x, y), size = 3, colour = "red", data = dfs)
    })

    vdiffr::expect_doppelganger("geom_segment", {
        dfw <- as.data.frame(x, walls = TRUE, method = "geom_segment")
        dfp <- as.data.frame(x, walls = FALSE, method = "geom_segment")
        dfs <- as.data.frame(x, start = "top", end = "bottom", method = "geom_segment")
        ggplot()  + coord_equal() + theme_void() +
          geom_segment(aes(x, y, xend=xend, yend=yend), lineend = "round", 
                       linewidth = 3, colour = "black", data = dfw) +
          geom_segment(aes(x, y, xend=xend, yend=yend), lineend = "round", 
                       linewidth = 3, colour = "grey80", data = dfp) +
          geom_segment(aes(x, y, xend=xend, yend=yend), lineend = "round",
                       linewidth = 3, colour = "red", data = dfs)
    })

    vdiffr::expect_doppelganger("geom_text", {
        dfw <- as.data.frame(x, walls = TRUE, method = "geom_text")
        dfp <- as.data.frame(x, walls = FALSE, method = "geom_text")
        dfs <- as.data.frame(x, start = "top", end = "bottom", method = "geom_text")
        ggplot()  + coord_equal() + theme_void() +
          geom_text(aes(x, y), label = "#", size = 8, colour = "black", data = dfw) +
          geom_text(aes(x, y), label = "o", size = 8, colour = "grey80", data = dfp) +
          geom_text(aes(x, y), label = "x", size = 8, colour = "red", data = dfs)
    })

    vdiffr::expect_doppelganger("geom_tile", {
        dfw <- as.data.frame(x, walls = TRUE, method = "geom_tile")
        dfp <- as.data.frame(x, walls = FALSE, method = "geom_tile")
        dfs <- as.data.frame(x, start = "top", end = "bottom", method = "geom_tile")
        ggplot()  + coord_equal() + theme_void() +
          geom_tile(aes(x, y), fill = "black", data = dfw) +
          geom_tile(aes(x, y), fill = "grey80", data = dfp) +
          geom_tile(aes(x, y), fill = "red", data = dfs)
    })
})
