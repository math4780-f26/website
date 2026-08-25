# ============================================================
# Polynomial regression surfaces:
#   (a) Full quadratic model with interaction
#   (b) Linear terms with interaction
#   (c) Quadratic terms without interaction
#
# Output:
#   surface_a_full_quadratic_interaction.png
#   surface_b_linear_interaction.png
#   surface_c_quadratic_no_interaction.png
#
# No external packages are required.
# ============================================================

# rm(list = ls())


# ============================================================
# 1. Generate predictor values
# ============================================================

x1_train <- seq(0, 1, length.out = 25)
x2_train <- seq(0, 1, length.out = 25)

dat <- expand.grid(
    X1 = x1_train,
    X2 = x2_train
)


# ============================================================
# 2. Define the three polynomial mean surfaces
# ============================================================

# ------------------------------------------------------------
# Panel (a)
#
# E(Y) = beta0 + beta1 X1 + beta2 X2
#        + beta3 X1^2 + beta4 X2^2
#        + beta5 X1 X2
# ------------------------------------------------------------

dat$Y_a <- with(
    dat,
    0.60 -
        3 * X1 -
        1 * X2 +
        5 * X1^2 +
        8 * X2^2 -
        6 * X1 * X2
)


# ------------------------------------------------------------
# Panel (b)
#
# E(Y) = beta0 + beta1 X1 + beta2 X2
#        + beta3 X1 X2
# ------------------------------------------------------------

dat$Y_b <- with(
    dat,
    0.20 +
        1 * X1 +
        2 * X2 +
        -3 * X1 * X2
)


# ------------------------------------------------------------
# Panel (c)
#
# E(Y) = beta0 + beta1 X1 + beta2 X2
#        + beta3 X1^2 + beta4 X2^2
# ------------------------------------------------------------

dat$Y_c <- with(
    dat,
    0.60 -
        2 * X1 -
        1 * X2 +
        3 * X1^2 +
        1.5 * X2^2
)


# ============================================================
# 3. Fit polynomial regression models
# ============================================================

fit_a <- lm(
    Y_a ~ X1 + X2 + I(X1^2) + I(X2^2) + X1:X2,
    data = dat
)

fit_b <- lm(
    Y_b ~ X1 + X2 + X1:X2,
    data = dat
)

fit_c <- lm(
    Y_c ~ X1 + X2 + I(X1^2) + I(X2^2),
    data = dat
)


# Optional: inspect the fitted coefficients

print(coef(fit_a))
print(coef(fit_b))
print(coef(fit_c))


# ============================================================
# 4. Create a fine prediction grid
# ============================================================

x1_grid <- seq(0, 1, length.out = 35)
x2_grid <- seq(0, 1, length.out = 35)

prediction_grid <- expand.grid(
    X1 = x1_grid,
    X2 = x2_grid
)


# Convert model predictions into a matrix suitable for persp()

make_surface <- function(model) {
    
    predicted_mean <- predict(
        model,
        newdata = prediction_grid
    )
    
    matrix(
        predicted_mean,
        nrow = length(x1_grid),
        ncol = length(x2_grid)
    )
}


z_a <- make_surface(fit_a)
z_b <- make_surface(fit_b)
z_c <- make_surface(fit_c)


# ============================================================
# 5. Add manually positioned axis labels
#
# persp() frequently positions xlab, ylab, and zlab poorly.
# Therefore, the built-in labels are suppressed and labels are
# added afterward using trans3d().
# ============================================================

add_persp_labels <- function(
        pmat,
        xlim,
        ylim,
        zlim,
        cex = 1.30
) {
    
    x_range <- diff(xlim)
    y_range <- diff(ylim)
    
    # ----------------------------------------------------------
    # X1-axis label
    # ----------------------------------------------------------
    
    label_x1 <- trans3d(
        x = mean(xlim),
        y = ylim[1] - 0.16 * y_range,
        z = zlim[1],
        pmat = pmat
    )
    
    text(
        x = label_x1$x,
        y = label_x1$y,
        labels = expression(X[1]),
        cex = cex,
        xpd = NA
    )
    
    
    # ----------------------------------------------------------
    # X2-axis label
    # ----------------------------------------------------------
    
    label_x2 <- trans3d(
        x = xlim[1]- 0.15 * x_range,
        # x = xlim[2] + 0.11 * x_range,
        y = mean(ylim),
        z = zlim[1],
        pmat = pmat
    )
    
    text(
        x = label_x2$x,
        y = label_x2$y,
        labels = expression(X[2]),
        cex = cex,
        xpd = NA
    )
    
    
    # ----------------------------------------------------------
    # Vertical response-axis label
    # ----------------------------------------------------------
    
    label_z <- trans3d(
        x = xlim[1] - 0.15 * x_range,
        y = ylim[2] + 0.03 * y_range,
        z = mean(zlim),
        pmat = pmat
    )
    
    text(
        x = label_z$x,
        y = label_z$y,
        labels = expression(E(Y ~ "|"~ X[1], X[2])),
        cex = cex,
        srt = 90,
        xpd = NA
    )
}


# ============================================================
# 6. Add filled vertical curtains below the surface
# ============================================================

add_surface_curtains <- function(
        z,
        pmat,
        z_bottom,
        curtain_color = "gray88",
        curtain_border = "gray55"
) {
    
    number_x1 <- nrow(z)
    number_x2 <- ncol(z)
    
    
    # ----------------------------------------------------------
    # Front curtain: X2 = minimum X2
    # ----------------------------------------------------------
    
    for (i in seq_len(number_x1 - 1)) {
        
        projected_polygon <- trans3d(
            x = c(
                x1_grid[i],
                x1_grid[i + 1],
                x1_grid[i + 1],
                x1_grid[i]
            ),
            y = rep(x2_grid[1], 4),
            z = c(
                z[i, 1],
                z[i + 1, 1],
                z_bottom,
                z_bottom
            ),
            pmat = pmat
        )
        
        polygon(
            projected_polygon$x,
            projected_polygon$y,
            col = adjustcolor(
                curtain_color,
                alpha.f = 0.90
            ),
            border = curtain_border,
            lwd = 0.45
        )
    }
    
    
    # ----------------------------------------------------------
    # Side curtain: X1 = maximum X1
    # ----------------------------------------------------------
    
    for (j in seq_len(number_x2 - 1)) {
        
        projected_polygon <- trans3d(
            x = rep(x1_grid[number_x1], 4),
            y = c(
                x2_grid[j],
                x2_grid[j + 1],
                x2_grid[j + 1],
                x2_grid[j]
            ),
            z = c(
                z[number_x1, j],
                z[number_x1, j + 1],
                z_bottom,
                z_bottom
            ),
            pmat = pmat
        )
        
        polygon(
            projected_polygon$x,
            projected_polygon$y,
            col = adjustcolor(
                curtain_color,
                alpha.f = 0.84
            ),
            border = curtain_border,
            lwd = 0.45
        )
    }
}


# ============================================================
# 7. Highlight conditional relationships
#
# Solid curves:
#   E(Y | X1, X2 fixed)
#
# Dashed curves:
#   E(Y | X1 fixed, X2)
# ============================================================

add_conditional_curves <- function(
        z,
        pmat,
        number_of_curves = 6
) {
    
    number_x1 <- nrow(z)
    number_x2 <- ncol(z)
    
    
    # ----------------------------------------------------------
    # Vary X1 while holding X2 fixed
    # ----------------------------------------------------------
    
    selected_x2 <- unique(
        round(
            seq(
                from = 1,
                to = number_x2,
                length.out = number_of_curves
            )
        )
    )
    
    for (j in selected_x2) {
        
        conditional_curve <- trans3d(
            x = x1_grid,
            y = rep(x2_grid[j], number_x1),
            z = z[, j],
            pmat = pmat
        )
        
        lines(
            conditional_curve$x,
            conditional_curve$y,
            col = "gray15",
            lwd = 1.30,
            lty = 1
        )
    }
    
    
    # ----------------------------------------------------------
    # Vary X2 while holding X1 fixed
    # ----------------------------------------------------------
    
    selected_x1 <- unique(
        round(
            seq(
                from = 1,
                to = number_x1,
                length.out = number_of_curves
            )
        )
    )
    
    for (i in selected_x1) {
        
        conditional_curve <- trans3d(
            x = rep(x1_grid[i], number_x2),
            y = x2_grid,
            z = z[i, ],
            pmat = pmat
        )
        
        lines(
            conditional_curve$x,
            conditional_curve$y,
            col = "gray30",
            lwd = 1.05,
            lty = 2
        )
    }
}


# ============================================================
# 8. Function for drawing one complete surface
# ============================================================

draw_polynomial_surface <- function(
        z,
        main,
        theta = -42,
        phi = 24,
        expand = 0.72
) {
    
    z_range <- range(z)
    z_span <- diff(z_range)
    
    if (z_span == 0) {
        z_span <- 1
    }
    
    z_bottom <- z_range[1] - 0.18 * z_span
    z_top    <- z_range[2] + 0.05 * z_span
    
    
    # ----------------------------------------------------------
    # Main fitted surface
    #
    # The border argument creates the fine grid over the surface.
    # Built-in axis labels are suppressed.
    # ----------------------------------------------------------
    
    pmat <- persp(
        x = x1_grid,
        y = x2_grid,
        z = z,
        
        theta = theta,
        phi = phi,
        expand = expand,
        
        col = "gray93",
        border = "gray55",
        shade = 0.06,
        ltheta = 110,
        
        xlim = range(x1_grid),
        ylim = range(x2_grid),
        zlim = c(z_bottom, z_top),
        
        axes = TRUE,
        box = TRUE,
        ticktype = "detailed",
        nticks = 5,
        
        # Add labels manually later
        xlab = "",
        ylab = "",
        zlab = "",
        
        main = main,
        
        cex.axis = 0.85,
        cex.main = 2
    )
    
    
    # ----------------------------------------------------------
    # Add filled curtains
    # ----------------------------------------------------------
    
    add_surface_curtains(
        z = z,
        pmat = pmat,
        z_bottom = z_bottom
    )
    
    
    # ----------------------------------------------------------
    # Add conditional curves
    # ----------------------------------------------------------
    
    add_conditional_curves(
        z = z,
        pmat = pmat,
        number_of_curves = 6
    )
    
    
    # ----------------------------------------------------------
    # Redraw the visible front boundary
    # ----------------------------------------------------------
    
    front_edge <- trans3d(
        x = x1_grid,
        y = rep(
            x2_grid[1],
            length(x1_grid)
        ),
        z = z[, 1],
        pmat = pmat
    )
    
    lines(
        front_edge$x,
        front_edge$y,
        col = "gray10",
        lwd = 1.40
    )
    
    
    # ----------------------------------------------------------
    # Redraw the visible side boundary
    # ----------------------------------------------------------
    
    side_edge <- trans3d(
        x = rep(
            x1_grid[length(x1_grid)],
            length(x2_grid)
        ),
        y = x2_grid,
        z = z[length(x1_grid), ],
        pmat = pmat
    )
    
    lines(
        side_edge$x,
        side_edge$y,
        col = "gray10",
        lwd = 1.40
    )
    
    
    # ----------------------------------------------------------
    # Add axis labels last so that nothing covers them
    # ----------------------------------------------------------
    
    add_persp_labels(
        pmat = pmat,
        xlim = range(x1_grid),
        ylim = range(x2_grid),
        zlim = c(z_bottom, z_top),
        cex = 1.30
    )
    
    
    invisible(pmat)
}


# ============================================================
# 9. Function for saving one surface
# ============================================================

save_surface_plot <- function(
        filename,
        z,
        main
) {
    
    png(
        filename = filename,
        width = 1500,
        height = 1200,
        res = 180,
        bg = "white"
    )
    
    par(
        mar = 0*c(4.8, 4.8, 3.2, 4.2),
        xpd = NA,
        family = "serif"
    )
    
    draw_polynomial_surface(
        z = z,
        main = main
    )
    
    dev.off()
}


# ============================================================
# 10. Create three separate image files
# ============================================================

# save_surface_plot(
#     filename = "surface_a_full_quadratic_interaction.png",
#     z = z_a,
#     main = "(a) Full quadratic model with interaction"
# )
# 
# draw_polynomial_surface(
#     z = z_a,
#     main = expression(
#         E(Y ~ "|" ~ X[1], X[2]) ==
#             beta[0] + beta[1] * X[1] + beta[2] * X[2] +
#             beta[3] * X[1]^2 + beta[4] * X[2]^2 +
#             beta[5] * X[1] * X[2]
#     )
# )
# 
# 
# save_surface_plot(
#     filename = "surface_b_linear_interaction.png",
#     z = z_b,
#     main = "(b) Linear terms with interaction"
# )
# 
# 
# save_surface_plot(
#     filename = "surface_c_quadratic_no_interaction.png",
#     z = z_c,
#     main = "(c) Quadratic terms without interaction"
# )


# ============================================================
# 11. Confirmation
# ============================================================

# message(
#     "Three plots have been saved in:\n",
#     normalizePath(getwd()), "\n\n",
#     "1. surface_a_full_quadratic_interaction.png\n",
#     "2. surface_b_linear_interaction.png\n",
#     "3. surface_c_quadratic_no_interaction.png"
# )