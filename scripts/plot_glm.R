# ============================================================
# Conditional response distributions for regression models
# Revised version:
# - Beta and Gamma panels have more distinct distributions
# - Beta and Gamma mean curves are visibly nonlinear on response scale
# Base R only
# ============================================================

# ------------------------------------------------------------
# 1. Global settings
# ------------------------------------------------------------

bg_col       <- "white"
axis_col     <- "#3465A4"
curve_col    <- "black"
mean_col     <- "#F28E2B"
marker_col   <- "#B22222"
title_col    <- "#7030A0"
logistic_col <- "#B00000"

# Use more separated x-values so the conditional distributions
# differ more clearly across panels
x_values <- c(0.10, 0.50, 0.90)

# ------------------------------------------------------------
# 2. Perspective projection
# ------------------------------------------------------------
# Model coordinates:
# x = predictor direction
# y = response direction on the plane
# z = density / probability height

origin <- c(0.22, 0.10)
x_vec  <- c(0.63, 0.31)
y_vec  <- c(-0.22, 0.20)
z_vec  <- c(0.00, 0.45)

project_point <- function(x, y, z = 0) {
    origin + x * x_vec + y * y_vec + z * z_vec
}

normalize_y <- function(y, y_lim) {
    (y - y_lim[1]) / diff(y_lim)
}

# ------------------------------------------------------------
# 3. Draw the perspective axes
# ------------------------------------------------------------

draw_plane_axes <- function() {
    p0 <- project_point(0, 0, 0)
    px <- project_point(1, 0, 0)
    py <- project_point(0, 1, 0)
    
    segments(p0[1], p0[2], px[1], px[2], col = axis_col, lwd = 1.2)
    segments(p0[1], p0[2], py[1], py[2], col = axis_col, lwd = 1.2)
}

# ------------------------------------------------------------
# 4. Draw the conditional-mean curve
# ------------------------------------------------------------

draw_mean_curve <- function(mean_fun, y_lim,
                            col = mean_col,
                            lwd = 2.5) {
    x_grid <- seq(0.05, 0.95, length.out = 300)
    means  <- mean_fun(x_grid)
    y_norm <- normalize_y(means, y_lim)
    
    pts <- t(vapply(
        seq_along(x_grid),
        function(i) project_point(x_grid[i], y_norm[i], 0),
        numeric(2)
    ))
    
    lines(pts[, 1], pts[, 2], col = col, lwd = lwd)
}

# ------------------------------------------------------------
# 5. Draw continuous conditional distributions
# ------------------------------------------------------------

draw_continuous_distributions <- function(
        x_values,
        density_fun,
        mean_fun,
        y_lim,
        density_scale = 0.70,
        draw_mean_markers = TRUE) {
    
    y_grid <- seq(y_lim[1], y_lim[2], length.out = 500)
    
    for (x0 in x_values) {
        
        density_values <- density_fun(y_grid, x0)
        density_values[!is.finite(density_values)] <- 0
        
        if (max(density_values) > 0) {
            density_height <- density_values / max(density_values) * density_scale
        } else {
            density_height <- density_values
        }
        
        y_norm <- normalize_y(y_grid, y_lim)
        
        curve_pts <- t(vapply(
            seq_along(y_grid),
            function(i) {
                project_point(
                    x = x0,
                    y = y_norm[i],
                    z = density_height[i]
                )
            },
            numeric(2)
        ))
        
        # idx <- which(curve_pts[, 2] > 0.1)
        lines(curve_pts[, 1], curve_pts[, 2], col = curve_col, lwd = 3)
        
        if (draw_mean_markers) {
            mu0 <- mean_fun(x0)
            density_at_mean <- density_fun(mu0, x0)
            
            if (!is.finite(density_at_mean)) {
                density_at_mean <- 0
            }
            
            max_density <- max(density_values)
            
            marker_height <- if (max_density > 0) {
                density_at_mean / max_density * density_scale
            } else {
                0
            }
            
            mean_base <- project_point(x0, normalize_y(mu0, y_lim), 0)
            mean_top  <- project_point(x0, normalize_y(mu0, y_lim), marker_height)
            
            segments(
                mean_base[1], mean_base[2],
                mean_top[1], mean_top[2],
                col = marker_col,
                lwd = 1.4,
                lty = 3
            )
        }
    }
}

# ------------------------------------------------------------
# 6. Draw discrete conditional distributions
# ------------------------------------------------------------

draw_discrete_distributions <- function(
        x_values,
        support,
        pmf_fun,
        y_lim,
        probability_scale = 0.70,
        lwd = 3) {
    
    for (x0 in x_values) {
        
        probabilities <- pmf_fun(support, x0)
        probabilities[!is.finite(probabilities)] <- 0
        
        if (max(probabilities) > 0) {
            heights <- probabilities / max(probabilities) * probability_scale
        } else {
            heights <- probabilities
        }
        
        for (j in seq_along(support)) {
            y0 <- normalize_y(support[j], y_lim)
            
            bar_base <- project_point(x0, y0, 0)
            bar_top  <- project_point(x0, y0, heights[j])
            
            segments(
                bar_base[1], bar_base[2],
                bar_top[1], bar_top[2],
                col = curve_col,
                lwd = lwd,
                lend = "round"
            )
        }
    }
}

# ------------------------------------------------------------
# 7. Optional labels at the conditional means
# ------------------------------------------------------------

draw_mean_labels <- function(mean_fun, y_lim, use_link = FALSE) {
    for (i in seq_along(x_values)) {
        
        x0  <- x_values[i]
        mu0 <- mean_fun(x0)
        
        location <- project_point(
            x0,
            normalize_y(mu0, y_lim),
            0
        )
        
        label_text <- if (use_link) {
            paste0("g(E(Y|X=x", i, "))")
        } else {
            paste0("E(Y|X=x", i, ")")
        }
        
        text(
            x = location[1] + 0.025,
            y = location[2] - 0.025,
            labels = label_text,
            pos = 4,
            cex = 0.62,
            xpd = NA
        )
    }
}

# ------------------------------------------------------------
# 8. Generic panel setup
# ------------------------------------------------------------

start_panel <- function(title,
                        title_color = title_col,
                        title_line2 = NULL) {
    plot.new()
    
    plot.window(
        xlim = c(0, 1),
        ylim = c(0, 1),
        xaxs = "i",
        yaxs = "i"
    )
    
    draw_plane_axes()
    
    if (is.null(title_line2)) {
        text(
            0.03, 0.94,
            labels = title,
            adj = c(0, 1),
            font = 2,
            cex = 1.15,
            col = title_color
        )
    } else {
        text(
            0.03, 0.94,
            labels = title,
            adj = c(0, 1),
            font = 2,
            cex = 1.15,
            col = title_color
        )
        
        text(
            0.03, 0.87,
            labels = title_line2,
            adj = c(0, 1),
            font = 2,
            cex = 1.15,
            col = title_color
        )
    }
}

# ============================================================
# 9. Model definitions
# ============================================================

# ------------------------------------------------------------
# Gaussian regression
# ------------------------------------------------------------

gaussian_mean <- function(x) {
    0.25 + 0.50 * x
}

gaussian_sd <- 0.105

gaussian_density <- function(y, x) {
    dnorm(y, mean = gaussian_mean(x), sd = gaussian_sd)
}

# ------------------------------------------------------------
# Beta regression
# IMPORTANT:
# logit(mu(x)) = beta0 + beta1*x is linear on the link scale,
# but mu(x) itself is nonlinear in x.
#
# Revised to make:
# - the orange mean curve visibly curved
# - the three conditional distributions more different
# ------------------------------------------------------------

beta_mean <- function(x) {
    plogis(-2 + 4.5 * x)
}

# smaller precision -> broader distributions and more visible differences
beta_precision <- 8

beta_density <- function(y, x) {
    mu <- beta_mean(x)
    alpha <- mu * beta_precision
    beta  <- (1 - mu) * beta_precision
    dbeta(y, shape1 = alpha, shape2 = beta)
}

# ------------------------------------------------------------
# Gamma regression
# IMPORTANT:
# log(mu(x)) = beta0 + beta1*x is linear on the link scale,
# but mu(x) itself is nonlinear in x.
#
# Revised to make:
# - the orange mean curve visibly curved
# - the three conditional distributions more different
# ------------------------------------------------------------

gamma_mean <- function(x) {
    exp(-1.25 + 2.10 * x)
}

# smaller shape -> more skewness and more visible differences
gamma_shape <- 2.6

gamma_density <- function(y, x) {
    mu <- gamma_mean(x)
    dgamma(y, shape = gamma_shape, scale = mu / gamma_shape)
}

# ------------------------------------------------------------
# Poisson regression
# ------------------------------------------------------------

poisson_mean <- function(x) {
    exp(0.15 + 1.55 * x)
}

poisson_pmf <- function(y, x) {
    dpois(y, lambda = poisson_mean(x))
}

# ------------------------------------------------------------
# Negative-binomial regression
# ------------------------------------------------------------

nb_mean <- function(x) {
    exp(0.15 + 1.55 * x)
}

nb_size <- 2.2

nb_pmf <- function(y, x) {
    dnbinom(y, mu = nb_mean(x), size = nb_size)
}

# ------------------------------------------------------------
# Logistic regression
# ------------------------------------------------------------

bernoulli_mean <- function(x) {
    plogis(-2.0 + 4.0 * x)
}

bernoulli_pmf <- function(y, x) {
    p <- bernoulli_mean(x)
    ifelse(y == 1, p, 1 - p)
}

# ============================================================
# 10. Draw all six panels
# ============================================================

# png(
#     filename = "regression_conditional_distributions_revised.png",
#     width = 1800,
#     height = 1150,
#     res = 180,
#     bg = bg_col
# )

par(
    mfrow = c(2, 3),
    mar = c(0.2, 0.2, 0.2, 0.2),
    oma = c(0.5, 0.5, 0.5, 0.5),
    xpd = NA,
    bg = bg_col
)

# ------------------------------------------------------------
# Panel 1: Gaussian regression
# ------------------------------------------------------------

start_panel(
    title = "Linear regression",
    title_line2 = "(Gaussian)"
)

gaussian_ylim <- c(0, 1)

draw_mean_curve(
    mean_fun = gaussian_mean,
    y_lim = gaussian_ylim
)

draw_continuous_distributions(
    x_values = x_values,
    density_fun = gaussian_density,
    mean_fun = gaussian_mean,
    y_lim = gaussian_ylim,
    density_scale = 0.63
)

# draw_mean_labels(
#     mean_fun = gaussian_mean,
#     y_lim = gaussian_ylim,
#     use_link = FALSE
# )

# ------------------------------------------------------------
# Panel 2: Beta regression
# ------------------------------------------------------------

start_panel(
    title = "Beta regression"
)

beta_ylim <- c(0.001, 0.999)

draw_mean_curve(
    mean_fun = beta_mean,
    y_lim = beta_ylim
)

draw_continuous_distributions(
    x_values = x_values,
    density_fun = beta_density,
    mean_fun = beta_mean,
    y_lim = beta_ylim,
    density_scale = 0.63
)

# draw_mean_labels(
#     mean_fun = beta_mean,
#     y_lim = beta_ylim,
#     use_link = TRUE
# )

# ------------------------------------------------------------
# Panel 3: Gamma regression
# ------------------------------------------------------------

start_panel(
    title = "Gamma regression"
)

# expanded y-range to accommodate more distinct gamma means
gamma_ylim <- c(0, 3.2)

draw_mean_curve(
    mean_fun = gamma_mean,
    y_lim = gamma_ylim
)

draw_continuous_distributions(
    x_values = x_values,
    density_fun = gamma_density,
    mean_fun = gamma_mean,
    y_lim = gamma_ylim,
    density_scale = 0.63
)

# draw_mean_labels(
#     mean_fun = gamma_mean,
#     y_lim = gamma_ylim,
#     use_link = TRUE
# )

# ------------------------------------------------------------
# Panel 4: Poisson regression
# ------------------------------------------------------------

start_panel(
    title = "Poisson regression"
)

poisson_support <- 0:10
poisson_ylim    <- range(poisson_support)

draw_mean_curve(
    mean_fun = poisson_mean,
    y_lim = poisson_ylim
)

draw_discrete_distributions(
    x_values = x_values,
    support = poisson_support,
    pmf_fun = poisson_pmf,
    y_lim = poisson_ylim,
    probability_scale = 0.63,
    lwd = 3
)

# ------------------------------------------------------------
# Panel 5: Negative-binomial regression
# ------------------------------------------------------------

start_panel(
    title = "Negative-binomial",
    title_line2 = "regression"
)

nb_support <- 0:14
nb_ylim    <- range(nb_support)

draw_mean_curve(
    mean_fun = nb_mean,
    y_lim = nb_ylim
)

draw_discrete_distributions(
    x_values = x_values,
    support = nb_support,
    pmf_fun = nb_pmf,
    y_lim = nb_ylim,
    probability_scale = 0.63,
    lwd = 3
)

# ------------------------------------------------------------
# Panel 6: Logistic regression
# ------------------------------------------------------------

start_panel(
    title = "Logistic regression",
    title_line2 = "(Bernoulli)",
    title_color = logistic_col
)

bernoulli_support <- c(0, 1)
bernoulli_ylim    <- c(0, 1)

draw_mean_curve(
    mean_fun = bernoulli_mean,
    y_lim = bernoulli_ylim,
    col = mean_col
)

draw_discrete_distributions(
    x_values = x_values,
    support = bernoulli_support,
    pmf_fun = bernoulli_pmf,
    y_lim = bernoulli_ylim,
    probability_scale = 0.65,
    lwd = 5
)

# dev.off()