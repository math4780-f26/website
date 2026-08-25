ell <- function(cx, cy, a, b, n = 400) {
    t <- seq(0, 2*pi, length.out = n)
    cbind(x = cx + a*cos(t), y = cy + b*sin(t))
}
tri <- function(x0, y0, w, h) rbind(c(x0-w, y0), c(x0+w, y0), c(x0, y0+h))
band_from_curve <- function(x, y, thickness = 0.9, col = "#FFD34E") {
    polygon(c(x, rev(x)), c(y, y - thickness), col = col, border = NA)
}
draw_pumpkin <- function() {
    par(bg = "black", mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
    plot.new(); plot.window(xlim = c(-10, 10), ylim = c(-10, 10))
    
    pumpkin_orange <<- "#F28C28"
    pumpkin_border <<- "#CC6E1E"
    groove_col <<- "#B95E15"
    stem_fill <<- "#3E6D3E"
    stem_border <<- "#2F5232"
    glow <<- "#FFD34E"
    
    body <- ell(0, -1, 7.5, 6)
    polygon(body, col = pumpkin_orange, border = pumpkin_border, lwd = 6)
    for (dx in seq(-5, 5, by = 2))
        lines(ell(dx*0.15, -1, 7.5 - abs(dx)*0.5, 6), col = groove_col, lwd = 2)
    
    polygon(rbind(c(-1, 5.5), c(1, 5.5), c(1.5, 8), c(-1.5, 8)),
            col = stem_fill, border = stem_border, lwd = 5)
    
    for (i in seq(0, 1, length.out = 6)) {
        a <- 7.8 + i*1.2
        b <- 6.2 + i*1.0
        alpha <- 0.10 * (1 - i)
        halo <- ell(0, -1, a, b)
        polygon(halo, col = rgb(1, 0.83, 0.3, alpha), border = NA)
    }
}
stats_pumpkin <- function(lambda_left = 5, lambda_right = 7,
                          n_sample = 20, conf_level = 0.95, set_seed = 123) {
    draw_pumpkin()

    kL <- 0:12; kR <- 0:14
    pL <- dpois(kL, lambda_left);  pL <- 1.8 * pL / max(pL)
    # pR <- dpois(kR, lambda_right); pR <- 1.8 * pR / max(pR)
    pR <- dchisq(kR, df = 7); pR <- 1.8 * pR / max(pR)
    points(-2.5 + (kL - mean(kL))*0.35, 2.0 + pL, pch = 16, cex = 1.5, col = glow)
    points(3 + (kR - mean(kR))*0.35, 2.0 + pR, pch = 16, cex = 1.5, col = glow,
           type = "b")

    polygon(tri(-2.6, 0.5, 1.2, 2.0), col = glow, border = NA)
    polygon(tri( 2.6, 0.5, 1.2, 2.0), col = glow, border = NA)

    set.seed(set_seed)
    x <- rnorm(n_sample, mean = 0, sd = 1)
    xbar <- mean(x); s <- sd(x); se <- s / sqrt(n_sample)
    alpha <- 1 - conf_level
    tcrit <- qt(1 - alpha/2, df = n_sample - 1)
    L <- xbar - tcrit*se
    U <- xbar + tcrit*se
    
    nose_center_y <- -1.6
    nose_scale <- 2.0
    ci_half <- (U - L)/2
    ci_vis_half <- max(0.6, nose_scale * ci_half)
    y_low <- nose_center_y - ci_vis_half
    y_high <- nose_center_y + ci_vis_half
    
    segments(0, y_low, 0, y_high, col = glow, lwd = 7, lend = "round")
    segments(-0.6, y_low, 0.6, y_low, col = glow, lwd = 7)
    segments(-0.6, y_high, 0.6, y_high, col = glow, lwd = 7)
    points(0, nose_center_y, pch = 21, bg = pumpkin_orange, col = NA, cex = 2.2)

    x <- seq(-6, 6, length.out = 300)
    y <- dnorm(x, mean = 0, sd = 2)
    y <- (y / max(y)) * 2.2
    y_smile <- -y - 3.5
    band_from_curve(x, y_smile, thickness = 0.5, col = glow)
    
    teeth_x <- seq(-4.5, 4.5, length.out = 6)
    for (tx in teeth_x) {
        tooth <- rbind(c(tx - 0.28, y_smile[which.min(abs(x - tx))] - 0.05),
                       c(tx + 0.28, y_smile[which.min(abs(x - tx))] - 0.05),
                       c(tx,        y_smile[which.min(abs(x - tx))] + 0.65))
        polygon(tooth, col = "white", border = NA)
    }
    
    text(0, 9, "~~~ Statistical Pumpkin ~~~", col = glow, cex = 2.5, font = 2)
    text(0, -8.5, "Happy Halloween from Dr. Yu!", 
         col = glow, cex = 2, font = 3)
}

stats_pumpkin()
