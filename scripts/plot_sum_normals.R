library(ggplot2)
library(dplyr)
library(colorspace)
# ============================================================
# Parameters
# ============================================================

mu_X  <- 1
var_X <- 2
sd_X  <- sqrt(var_X)

mu_Y  <- 2
var_Y <- 1
sd_Y  <- sqrt(var_Y)

a <- 2
b <- -3

# Theoretical results
mu_aX  <- a * mu_X
var_aX <- a^2 * var_X
sd_aX  <- sqrt(var_aX)

mu_bY  <- b * mu_Y
var_bY <- b^2 * var_Y
sd_bY  <- sqrt(var_bY)

mu_S   <- mu_aX + mu_bY
var_S  <- var_aX + var_bY
sd_S   <- sqrt(var_S)

# ============================================================
# Build theoretical density curves
# ============================================================

x_min <- min(mu_aX - 4*sd_aX,
             mu_bY - 4*sd_bY,
             mu_S  - 4*sd_S)

x_max <- max(mu_aX + 4*sd_aX,
             mu_bY + 4*sd_bY,
             mu_S  + 4*sd_S)

x_grid <- seq(x_min, x_max, length.out = 1000)

plot_df <- bind_rows(
    data.frame(
        x = x_grid,
        density = dnorm(x_grid, mean = mu_X, sd = sd_X),
        dist = "X"
    ),
    data.frame(
        x = x_grid,
        density = dnorm(x_grid, mean = mu_Y, sd = sd_Y),
        dist = "Y"
    ),
    data.frame(
        x = x_grid,
        density = dnorm(x_grid, mean = mu_aX, sd = sd_aX),
        dist = "2X"
    ),
    data.frame(
        x = x_grid,
        density = dnorm(x_grid, mean = mu_bY, sd = sd_bY),
        dist = "-3Y"
    ),
    data.frame(
        x = x_grid,
        density = dnorm(x_grid, mean = mu_S, sd = sd_S),
        dist = "2X - 3Y"
    )
)

mean_df <- data.frame(
    mean = c(mu_X, mu_Y, mu_aX, mu_bY, mu_S),
    dist = c("X", "Y", "2X", "-3Y", "2X - 3Y")
)

# ============================================================
# One combined plot
# ============================================================

fig <- ggplot(plot_df, aes(x = x, y = density, color = dist)) +
    geom_line(linewidth = 1.3) +
    # geom_vline(data = mean_df,
    #            aes(xintercept = mean, color = dist),
    #            linetype = 2,
    #            linewidth = 1) +
    scale_color_manual(
        values = c("X" = lighten("#4C78A8", 0.8),
                   "Y" = lighten("#F58518", 0.8),
                   "2X" = "#4C78A8",
                   "-3Y" = "#F58518",
                   "2X - 3Y" = "#54A24B")
    ) +
    labs(
        title = "Sum of Normal Distributions",
        x = "Value",
        y = "Density",
        color = NULL
    ) +
    # annotate("text", x = mu_aX, y = 0.14,
    #          label = "mean = 2", color = "#4C78A8", hjust = -0.1) +
    # annotate("text", x = mu_bY, y = 0.08,
    #          label = "mean = -6", color = "#F58518", hjust = 1.1) +
    # annotate("text", x = mu_S, y = 0.06,
    #          label = "mean = -4", color = "#54A24B", hjust = -0.1) +
    theme_classic(base_size = 15) +
    theme(legend.position = "top")

print(fig)
