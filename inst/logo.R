
### logo for the mazing hex sticker
library(mazing)

# define text region
mat <- matrix(c(
    1,0,1,1,1,0,0,1,1,0,0,0,1,1,0,0,0,0,0,0,1,0,0,1,0,0,1,1,0,1,1,1,0,0,0,1,1,
    1,0,0,1,0,0,0,1,0,0,0,0,0,1,1,1,1,0,0,0,1,0,0,1,0,0,1,1,0,1,1,0,0,1,1,0,1,
    1,0,0,0,0,0,0,1,0,1,1,0,0,1,1,1,0,0,0,1,1,0,0,1,0,0,0,1,0,1,0,0,1,1,1,1,1,
    1,0,1,0,1,0,0,1,0,1,1,0,0,1,1,0,0,0,1,1,1,0,0,1,0,0,0,0,0,1,0,0,1,0,0,0,1,
    1,0,1,1,1,0,0,1,0,0,0,0,0,1,0,0,0,1,1,1,1,0,0,1,0,1,0,0,0,1,0,0,1,1,0,0,1,
    1,0,1,1,1,0,0,1,0,1,1,0,0,1,0,0,1,1,1,1,1,0,0,1,0,1,1,0,0,1,1,0,0,1,1,0,1,
    1,0,1,1,1,0,0,1,0,1,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,1,1,1,0,1,1,1,0,0,0,0,1),
byrow = TRUE, nrow=7)[7:1,]

# adjust padding
mat <- rbind(1,1,mat,1,1)
mat <- cbind(1,mat,1)

# make it a hex
# width = current width, center the text
w <- ncol(mat)
h <- ceiling(5.08 * w / 4.39)
while(nrow(mat) < h - 1){
    mat <- rbind(1,mat,1)
}
if(nrow(mat) == h-1){
    mat <- rbind(mat,1)
}
for(ii in 1:(nrow(mat)/2)){
    for(jj in 1:(ncol(mat)/2)){
        if(sqrt(3)*ii+jj < (ncol(mat)+1)/2){
            mat[ii,jj] <- 0
            mat[ii,ncol(mat)+1-jj] <- 0
            mat[nrow(mat)+1-ii,jj] <- 0
            mat[nrow(mat)+1-ii,ncol(mat)+1-jj] <- 0
        }
    }
}
# image(t(mat))

# make it a maze
m <- as.maze(mat)
mat[abs((1:nrow(mat))-(nrow(mat)/2)) > nrow(mat)/4, ] <- 1
m2 <- as.maze(1-mat)
# solve it (top to bottom)
p <- solve_maze(m, start = 'bottom', end = 'top')



##########
### plotting
##########

# simple
pdf(file = '~/Desktop/mazing_logo.pdf', width = 6, height = 6)
red <- brewer.pal(9,'Set1')[1]
plot(m)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col =  "black")
#rect(1, 12, 38, 34, col = 'black')
lines(m, col = 'white', lwd=6, lend=2)
lines(m, walls=TRUE, lwd=2, col='black', lend =2)
lines(p, col = red, lwd = 1, lty = 3)
points(rep(median(1:ncol(mat)),2), c(nrow(mat),1), col = red, cex=.6, pch = 16)
dev.off()



# mondrian
plot(m, walls = TRUE)
rect(1, 12, 38, 34, col = 'red')
lines(m, lwd=7, col = 'white', lend=2)
lines(p, col = 'yellow2', lwd = 6, lend=2)
points(rep(median(1:ncol(mat)),2), c(nrow(mat),1), col = 'blue', pch=15)
lines(m, walls = TRUE, lwd = 1.5, lend = 2)


# hedge maze
darkgreen <- brewer.pal(9,'BuGn')[9]
lightgreen <- brewer.pal(9,'Greens')[6]
plot(m)
rect(1, 12, 38, 34, col = 'goldenrod')
lines(m, col = darkgreen, lwd=6, lend=2)
lines(m, walls=TRUE, lwd=3, col='black',adjust = c(-.15,-.15))
lines(m, walls=TRUE, lwd=3, col=lightgreen)
lines(p, col = 'grey90', lwd = .6, lty = 3)
points(rep(median(1:ncol(mat)),2), c(nrow(mat),1), col = 'grey90', cex=.6)




# 90's cup
plot(m)
#rect(1, 12, 38, 34, col = 'turquoise')
lines(m, col = 'black', lwd=6, lend=2)
lines(p+.1, col = 'grey90', lwd = .6, lty = 3)
points(rep(median(1:ncol(mat)),2)+.1, c(nrow(mat),1)+.1, col = 'grey90', cex=.4)
lines(m, walls=TRUE, lwd=3, col='pink', adjust = c(.2,.2))
lines(m, walls=TRUE, lwd=3, col='turquoise')
lines(m2, lwd=9, lend=2, col = 'turquoise')
points(37,25, pch=15, col='turquoise', cex=1.1)





