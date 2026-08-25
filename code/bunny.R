ell <- function(cx, cy, a, b, n = 400) {
    t <- seq(0, 2*pi, length.out = n)
    cbind(x = cx + a*cos(t), y = cy + b*sin(t))
}

egg_shape <- function(cx, cy, a = 1.1, b = 1.5, n = 400) {
    t <- seq(0, 2*pi, length.out = n)
    x <- cx + a * 0.78 * sin(t)
    y <- cy + b * (0.95 * cos(t) - 0.18 * cos(2*t))
    cbind(x = x, y = y)
}

band_from_curve <- function(x, y, thickness = 0.4, col = "#FFD34E") {
    polygon(c(x, rev(x)), c(y, y - thickness), col = col, border = NA)
}

rescale_to <- function(z, to) {
    rng <- range(z)
    if (diff(rng) == 0) return(rep(mean(to), length(z)))
    (z - rng[1]) / diff(rng) * diff(to) + to[1]
}

ci_bar_h <- function(est, lo, hi, y, col = "#7A3E9D", lwd = 4, cex_pt = 1.6) {
    segments(lo, y, hi, y, col = col, lwd = lwd, lend = "round")
    segments(lo, y - 0.10, lo, y + 0.10, col = col, lwd = lwd)
    segments(hi, y - 0.10, hi, y + 0.10, col = col, lwd = lwd)
    points(est, y, pch = 21, bg = "white", col = col, cex = cex_pt, lwd = 2)
}

ci_bar_v <- function(est, lo, hi, x, col = "#E97A7A", lwd = 6, cex_pt = 2.0) {
    segments(x, lo, x, hi, col = col, lwd = lwd, lend = "round")
    segments(x - 0.18, lo, x + 0.18, lo, col = col, lwd = lwd)
    segments(x - 0.18, hi, x + 0.18, hi, col = col, lwd = lwd)
    points(x, est, pch = 21, bg = "white", col = col, cex = cex_pt, lwd = 2)
}

draw_inferential_easter <- function(
        n = 22,
        conf_level = 0.95,
        alpha_test = 0.05,
        seed = 123
) {
    set.seed(seed)

    sky <- "#EAF7FF"
    ground <- "#CFEFB1"
    bunny_fill <- "white"
    bunny_border <- "#8A8A8A"
    ear_inner <- "#FFD8EC"
    glow <- "#FFD34E"
    pink <- "#F49AC2"
    purple <- "#8E63C7"
    green <- "#5A9E55"
    blue <- "#5DADE2"
    orange <- "#F5B041"
    red <- "#D95F5F"
    teal <- "#1F618D"

    par(bg = sky, mar = c(0, 0, 1.2, 0), xaxs = "i", yaxs = "i")
    plot.new()
    plot.window(xlim = c(-10, 10), ylim = c(-9, 10))
    
    rect(-10, -9, 10, 10, col = sky, border = NA)
    rect(-10, -9, 10, -4.3, col = ground, border = NA)

    body <- ell(0, -0.8, 3.1, 3.6)
    head <- ell(0, 3.0, 2.25, 2.1)
    
    earL_outer <- ell(-2.2, 6.5, 0.95, 2.65)
    earR_outer <- ell( 2.2, 6.5, 0.95, 2.65)
    earL_inner <- ell(-2.2, 6.55, 0.45, 2.0)
    earR_inner <- ell( 2.2, 6.55, 0.45, 2.0)
    
    polygon(body, col = bunny_fill, border = bunny_border, lwd = 3)
    polygon(head, col = bunny_fill, border = bunny_border, lwd = 3)
    polygon(earL_outer, col = bunny_fill, border = bunny_border, lwd = 3)
    polygon(earR_outer, col = bunny_fill, border = bunny_border, lwd = 3)
    polygon(earL_inner, col = ear_inner, border = NA)
    polygon(earR_inner, col = ear_inner, border = NA)

    polygon(ell(-1.2, -3.7, 0.9, 0.6), col = bunny_fill, border = bunny_border, lwd = 2)
    polygon(ell( 1.2, -3.7, 0.9, 0.6), col = bunny_fill, border = bunny_border, lwd = 2)
    polygon(ell(2.85, -1.4, 0.65, 0.65), col = "white", border = bunny_border, lwd = 2)

    x <- rnorm(n, mean = 0.35, sd = 1.15)
    xbar <- mean(x)
    s <- sd(x)
    se <- s / sqrt(n)
    tcrit <- qt(1 - (1 - conf_level)/2, df = n - 1)
    ci_mean <- c(xbar - tcrit * se, xbar + tcrit * se)

    x_t <- seq(-4, 4, length.out = 500)
    y_t <- dt(x_t, df = n - 1)
    crit <- qt(1 - alpha_test / 2, df = n - 1)
    
    xL <- rescale_to(x_t, c(-2.95, -1.45))
    yL <- rescale_to(y_t, c(5.3, 8.55))
    y_base_L <- 5.3
    
    idx_left <- x_t <= -crit
    idx_right <- x_t >= crit
    
    polygon(
        c(xL[idx_left], rev(xL[idx_left])),
        c(yL[idx_left], rep(y_base_L, sum(idx_left))),
        col = adjustcolor(glow, alpha.f = 0.70),
        border = NA
    )
    polygon(
        c(xL[idx_right], rev(xL[idx_right])),
        c(yL[idx_right], rep(y_base_L, sum(idx_right))),
        col = adjustcolor(glow, alpha.f = 0.70),
        border = NA
    )
    lines(xL, yL, col = purple, lwd = 3)
    
    xv1 <- rescale_to(-crit, c(-2.95, -1.45))
    xv2 <- rescale_to( crit, c(-2.95, -1.45))
    segments(xv1, y_base_L, xv1, rescale_to(dt(-crit, df = n - 1), c(5.3, 8.55)),
             col = purple, lwd = 2, lty = 2)
    segments(xv2, y_base_L, xv2, rescale_to(dt( crit, df = n - 1), c(5.3, 8.55)),
             col = purple, lwd = 2, lty = 2)

    x_n <- seq(-3.8, 3.8, length.out = 500)
    y_n <- dnorm(x_n)
    zcrit <- qnorm(1 - (1 - conf_level)/2)
    
    xR <- rescale_to(x_n, c(1.45, 2.95))
    yR <- rescale_to(y_n, c(5.3, 8.55))
    y_base_R <- 5.3
    
    idx_mid <- abs(x_n) <= zcrit
    polygon(
        c(xR[idx_mid], rev(xR[idx_mid])),
        c(yR[idx_mid], rep(y_base_R, sum(idx_mid))),
        col = adjustcolor(blue, alpha.f = 0.55),
        border = NA
    )
    lines(xR, yR, col = blue, lwd = 3)
    
    xci1 <- rescale_to(-zcrit, c(1.45, 2.95))
    xci2 <- rescale_to( zcrit, c(1.45, 2.95))
    xmu  <- rescale_to(0,      c(1.45, 2.95))
    
    segments(xci1, y_base_R, xci1, rescale_to(dnorm(-zcrit), c(5.3, 8.55)),
             col = blue, lwd = 2, lty = 2)
    segments(xci2, y_base_R, xci2, rescale_to(dnorm( zcrit), c(5.3, 8.55)),
             col = blue, lwd = 2, lty = 2)
    segments(xmu, y_base_R, xmu, 8.45, col = teal, lwd = 2)

    points(-0.65, 3.45, pch = 16, cex = 1.4, col = "#333333")
    points( 0.65, 3.45, pch = 16, cex = 1.4, col = "#333333")

    ci_half_vis <- max(0.5, 1.9 * (ci_mean[2] - ci_mean[1]) / 2)
    ci_bar_v(est = 2.45, lo = 2.45 - ci_half_vis, hi = 2.45 + ci_half_vis,
             x = 0, col = red, lwd = 6, cex_pt = 2.2)

    y_whisk <- c(2.9, 2.45, 2.0)
    
    ci_bar_h(est = -1.90, lo = -2.65, hi = -1.15, y = y_whisk[1], col = purple, lwd = 4)
    ci_bar_h(est = -2.00, lo = -2.95, hi = -1.05, y = y_whisk[2], col = purple, lwd = 4)
    ci_bar_h(est = -1.75, lo = -2.45, hi = -1.05, y = y_whisk[3], col = purple, lwd = 4)
    
    ci_bar_h(est =  1.90, lo =  1.15, hi =  2.65, y = y_whisk[1], col = purple, lwd = 4)
    ci_bar_h(est =  2.00, lo =  1.05, hi =  2.95, y = y_whisk[2], col = purple, lwd = 4)
    ci_bar_h(est =  1.75, lo =  1.05, hi =  2.45, y = y_whisk[3], col = purple, lwd = 4)

    z_obs <- 1.55
    xz <- seq(-3.5, 3.5, length.out = 500)
    yz <- dnorm(xz)
    yz <- (yz / max(yz)) * 1.55
    y_smile <- -yz + 0.55
    
    band_from_curve(xz, y_smile, thickness = 0.18, col = "#F7C948")

    lines(xz, y_smile, col = "#C97B00", lwd = 3)

    egg1 <- egg_shape(-5.7, -5.6, a = 1.05, b = 1.55)
    egg2 <- egg_shape( 0.0, -5.7, a = 1.15, b = 1.65)
    egg3 <- egg_shape( 5.8, -5.6, a = 1.05, b = 1.55)
    
    polygon(egg1, col = "#FDE2E4", border = pink, lwd = 3)
    polygon(egg2, col = "#E8DAEF", border = purple, lwd = 3)
    polygon(egg3, col = "#FCF3CF", border = orange, lwd = 3)

    xe1 <- seq(0, 12, length.out = 300)
    ye1 <- dchisq(xe1, df = 5)
    x1m <- rescale_to(xe1, c(-6.45, -4.95))
    y1m <- rescale_to(ye1, c(-6.35, -5.10))
    lines(x1m, y1m, col = pink, lwd = 2.5)
    
    idx1R <- xe1 >= 7
    polygon(
        c(x1m[idx1R], rev(x1m[idx1R])),
        c(y1m[idx1R], rep(-6.35, sum(idx1R))),
        col = adjustcolor(pink, alpha.f = 0.45),
        border = NA
    )

    ci_bar_h(est = -0.35, lo = -0.90, hi = 0.20, y = -5.2, col = purple, lwd = 3, cex_pt = 1.0)
    ci_bar_h(est =  0.10, lo = -0.55, hi = 0.80, y = -5.7, col = purple, lwd = 3, cex_pt = 1.0)
    ci_bar_h(est =  0.40, lo = -0.05, hi = 0.95, y = -6.2, col = purple, lwd = 3, cex_pt = 1.0)

    k <- 0:10
    pk <- dpois(k, lambda = 4)
    x3m <- rescale_to(k, c(4.95, 6.65))
    y3m <- rescale_to(pk, c(-6.35, -5.15))
    y_base_3 <- -6.35
    
    segments(x3m, y_base_3, x3m, y3m, col = orange, lwd = 2)
    points(x3m, y3m, pch = 21, bg = orange, col = "#AF601A", cex = 0.9, lwd = 1.2)
    lines(x3m, y3m, col = "#AF601A", lwd = 1.5)

    flower_xy <- rbind(c(-8.3, -4.2), c(-7.0, -4.0), c(7.0, -4.15), c(8.4, -4.0))
    for (i in 1:nrow(flower_xy)) {
        cx <- flower_xy[i, 1]
        cy <- flower_xy[i, 2]
        segments(cx, -4.9, cx, cy - 0.08, col = green, lwd = 2)
        for (ang in seq(0, 2*pi, length.out = 5)[-5]) {
            polygon(ell(cx + 0.13*cos(ang), cy + 0.13*sin(ang), 0.12, 0.18),
                    col = "#F7C6D9", border = "#D16BA5", lwd = 1)
        }
        polygon(ell(cx, cy, 0.08, 0.08), col = glow, border = "#B9770E", lwd = 1)
    }
    text(0, 9.35, "~~~Happy Easter: Statistics Bunny~~~", cex = 1.9,
         col = "#3B5BA9", font = 2)
}

draw_inferential_easter()