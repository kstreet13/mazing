#################
### maze building
#################
# key:
# 0 = origin, 1 = down, 2 = left, 3 = up, 4 = right
# -1 = to be filled in
# -5 = not to be filled in
adjacent <- function(coords, maze, return.coords = FALSE){
    out.coords <- NULL
    val <- rep(NA, 4)
    up <- coords + c(1,0)
    if(up[1] <= nrow(maze)){
        out.coords <- rbind(out.coords, up)
        val[3] <- maze[up[1],up[2]]
    }
    dn <- coords + c(-1,0)
    if(dn[1] > 0){
        out.coords <- rbind(out.coords, dn)        
        val[1] <- maze[dn[1],dn[2]]
    }
    lf <- coords + c(0,-1)
    if(lf[2] > 0){
        out.coords <- rbind(out.coords, lf)
        val[2] <- maze[lf[1],lf[2]]
    }
    rt <- coords + c(0,1)
    if(rt[2] <= ncol(maze)){
        out.coords <- rbind(out.coords, rt)
        val[4] <- maze[rt[1],rt[2]]
    }
    if(return.coords){
        return(out.coords)
    }
    return(val)
}
# only need this for plotting walls
diag_adj_vals <- function(coords, maze){
    val <- rep(NA, 4)
    ul <- coords + c(1,-1)
    if(ul[1] <= nrow(maze) && ul[2] > 0){
        val[1] <- maze[ul[1],ul[2]]
    }
    ur <- coords + c(1,1)
    if(ur[1] <= nrow(maze) && ur[2] <= ncol(maze)){
        val[2] <- maze[ur[1],ur[2]]
    }
    dl <- coords + c(-1,-1)
    if(dl[1] > 0 && dl[2] > 0){
        val[3] <- maze[dl[1],dl[2]]
    }
    dr <- coords + c(-1,1)
    if(dr[1] > 0 && dr[2] <= ncol(maze)){
        val[4] <- maze[dr[1],dr[2]]
    }
    return(val)
}
previous <- function(coords, maze){
    dir <- maze[coords[1],coords[2]]
    if(dir == 0){
        return(c(NA, NA))
    }
    # reverse the move that was taken to get here
    prev <- switch(dir,
                   '1' = coords + c(1,0),
                   '2' = coords + c(0,1),
                   '3' = coords + c(-1,0),
                   '4' = coords + c(0,-1))
    return(prev)
}
fill_maze <- function(maze, start = NULL){
    while(any(maze == -1)){
        if(is.null(start)){
            # pick a not-exactly-random start
            s1 <- as.numeric(sample(as.character( # more coding around "sample"
                seq_len(nrow(maze))[rowSums(maze==-1) > 0]), 1))
            s2 <- as.numeric(sample(as.character(which(maze[s1,]==-1)), 1))
            start <- c(s1,s2)
        }
        maze[start[1],start[2]] <- 0
        last <- curr <- start
        adj <- adjacent(last, maze)
        poss <- which(adj == -1)
        while(! (length(poss) == 0 & all(curr == start))){
            # if no valid options, back up one step and try again
            if(length(poss) == 0){
                curr <- previous(last, maze)
            }else{
                # pick next step
                dir <- poss[sample(length(poss), 1)] # coding around sample's "convenience" feature
                curr <- switch(dir,
                               '1' = last + c(-1,0), # technically, the names are 
                               '2' = last + c(0,-1), # unnecessary. It's picking
                               '3' = last + c(1,0),  # the correct case based on 
                               '4' = last + c(0,1))  # position, not name
                maze[curr[1],curr[2]] <- dir
            }
            last <- curr
            # identify possible next steps from neighbors of 'last'
            adj <- adjacent(last, maze)
            poss <- which(adj == -1)
        }
        start <- NULL
    }
    return(maze)
}

#################
### convert to binary representation for plotting walls
#################
to_binary <- function(m){
    m2 <- matrix(NA, 2*nrow(m)+1, 2*ncol(m)+1)
    m2[,1] <- m2[,ncol(m2)] <- -5
    m2[1,] <- m2[nrow(m2),] <- -5
    for(i in 1:nrow(m)){
        for(j in 1:ncol(m)){
            dir <- m[i,j]
            m2[2*i, 2*j] <- dir
            if(dir != 0){
                # reverse the move that was taken to get here
                prev <- switch(dir,
                               '1' = 2*c(i,j) + c(1,0),
                               '2' = 2*c(i,j) + c(0,1),
                               '3' = 2*c(i,j) + c(-1,0),
                               '4' = 2*c(i,j) + c(0,-1))
                m2[prev[1], prev[2]] <- dir
            }
        }
    }
    m2[is.na(m2)] <- -5
    m2[m2 >= 0] <- 1
    return(m2)
}


#################
### advanced maze manipulation
#################
# if you've made it this far:
# These operate on mazes in the thick representation and I used these to make a
# maze-within-a-maze.
# expand: double the size of a maze (each wall and path becomes two rows/columns
# wide).
expand <- function(m){
    m2 <- matrix(NA, nrow = 2*nrow(m), ncol = 2*ncol(m))
    for(i in 1:ncol(m)){
        m2[,2*i-1] <- m2[,2*i] <- rep(m[,i], each = 2)
    }
    return(m2)
}
# seep: let the paths "seep" into the walls. For making a maze-within-a-maze,
# the walls are not interesting, but the paths are *very* interesting, so
# there's no reason not to make the paths wider than the walls. Expand twice,
# seep once.
seep <- function(m, what = 1){
    m2 <- m
    for(i in 2:nrow(m)){
        m2[i, ][m[i-1, ]==what] <- what
    }
    for(i in 1:(nrow(m)-1)){
        m2[i, ][m[i+1, ]==what] <- what
    }
    for(j in 2:ncol(m)){
        m2[,j][m[,j-1]==what] <- what
    }
    for(j in 1:(ncol(m)-1)){
        m2[,j][m[,j+1]==what] <- what
    }
    return(m2)
}
# condense: this one's for making mazes from images (ie. PNG files via readPNG).
# They might be too big, so you can condense them by a factor of 2 and take the
# average within each cell (previously 4 cells)
condense <- function(m, fun = median){
    odd.col <- (ncol(m) %% 2) == 1
    odd.row <- (nrow(m) %% 2) == 1
    m2 <- matrix(NA, nrow = ceiling(nrow(m)/2), ncol = ceiling(ncol(m)/2))
    for(i in 1:(nrow(m2))){
        for(j in 1:(ncol(m2))){
            i.2 <- (2*i-1):(2*i)
            j.2 <- (2*j-1):(2*j)
            if(i == nrow(m2)){
                i.2 <- (nrow(m)-1):nrow(m)
            }
            if(j == ncol(m2)){
                j.2 <- (ncol(m)-1):ncol(m)
            }
            m2[i,j] <- fun(m[i.2 , j.2])
        }
    }
    return(m2)
}



