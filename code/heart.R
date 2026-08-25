x <- seq(0, 2*pi, by = 0.01)
xhrt <- 16 * sin(x) ^ 3
yhrt <- 13 * cos(x) - 5 * cos(2*x) - 2 * cos(3*x) - cos(4*x)
par(mar = c(0, 0, 0, 0))
plot(xhrt, yhrt, type = "l", axes = FALSE, xlab = "", ylab = "")
polygon(xhrt, yhrt, col = "red", border = NA)
points(c(10,-10, -15, 15), c(-10, -10, 10, 10), pch = 169, font = 5)
text(0, 0, "Happy Valentine's Day!", font = 2, cex = 2, col = "pink")