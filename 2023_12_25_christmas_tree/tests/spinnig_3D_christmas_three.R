library(rgl)

# from: https://www.r-bloggers.com/2021/12/x-mas-tree-with-10-lines-of-r-code/

t <- seq(0, 100, by = 0.7)^0.6
x <- t * c(sin(t), sin(t + pi))
y <- t * c(cos(t), cos(t + pi))
z <- -2 * c(t, t)
color <- rep(c("darkgreen", "gold"), each = length(t))
open3d(windowRect = c(100, 100, 600, 600), zoom = 0.9)
bg3d("black")
spheres3d(x, y, z, radius = 0.3, color = color)
# On screen (skip if export)
play3d(spin3d(axis = c(0, 0, 1), rpm = 4))

close3d()
