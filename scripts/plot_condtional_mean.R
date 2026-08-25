library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# ============================================================
# Model
# ============================================================

# Y | X ~ N(mu(X), 1)
# mu(X) = 3X

mu <- function(x) 3 * x
sigma <- 1

x_values <- c(0, 1, 2, 3)

# ============================================================
# Use the SAME colors in both figures
# ============================================================

x_colors <- c(
    "0" = "#4C78A8",
    "1" = "#F58518",
    "2" = "#54A24B",
    "3" = "#B279A2"
)

# ============================================================
# Figure 1: Conditional mean mu(X) = 3X
# ============================================================

mean_df <- data.frame(
    x = seq(-0.2, 3.2, length.out = 200)
) |>
    mutate(mu = 3 * x)

points_df <- data.frame(
    x = x_values,
    mu = mu(x_values),
    X_group = factor(x_values)
)

p1 <- ggplot(mean_df, aes(x = x, y = mu)) +
    
    # regression / conditional mean function
    geom_line(
        linewidth = 1.2,
        color = "gray40"
    ) +
    
    # points use SAME colors as density curves below
    geom_point(
        data = points_df,
        aes(x = x, y = mu, color = X_group),
        size = 4
    ) +
    
    scale_color_manual(
        values = x_colors
    ) +
    
    scale_x_continuous(
        breaks = 0:3
    ) +
    
    scale_y_continuous(
        breaks = seq(0, 9, 3)
    ) +
    
    labs(
        title = expression(mu(X) == 3*X),
        subtitle = "X determines the center of the conditional distribution of Y",
        x = "X",
        y = expression(mu(X)),
        color = "X"
    ) +
    
    theme_classic(base_size = 15) +
    
    theme(
        legend.position = "right"
    )


# ============================================================
# Figure 2: Conditional distributions for different X values
# ============================================================

y_grid <- seq(-4, 14, length.out = 1000)

density_df <- expand_grid(
    y = y_grid,
    X = x_values
) |>
    mutate(
        mean = 3 * X,
        density = dnorm(
            y,
            mean = mean,
            sd = sigma
        ),
        X_group = factor(X)
    )

p2 <- ggplot(
    density_df,
    aes(
        x = y,
        y = density,
        color = X_group
    )
) +
    
    geom_line(
        linewidth = 1.3
    ) +
    
    # mark the corresponding conditional means
    geom_vline(
        xintercept = c(0, 3, 6, 9),
        linetype = 3,
        color = "gray60"
    ) +
    
    scale_color_manual(
        values = x_colors,
        labels = c(
            "Y | X = 0 ~ N(0, 1)",
            "Y | X = 1 ~ N(3, 1)",
            "Y | X = 2 ~ N(6, 1)",
            "Y | X = 3 ~ N(9, 1)"
        )
    ) +
    
    scale_x_continuous(
        breaks = seq(-3, 12, 3)
    ) +
    
    labs(
        title = expression(
            Y*" | "*X == x %~% N(3*x, 1)
        ),
        subtitle =
            "Changing X shifts the center of the conditional distribution",
        x = "Y",
        y = "Conditional density",
        color = NULL
    ) +
    
    theme_classic(base_size = 15) +
    
    theme(
        legend.position = "right",
        legend.text = element_text(size = 11)
    )


# ============================================================
# Combine the two figures
# ============================================================

fig <- p1 / p2 
# +
#     plot_layout(
#         heights = c(1, 1.3)
#     ) +
#     plot_annotation(
#         title = "The Distribution of Y Depends on X",
#         subtitle = expression(
#             Y*" | "*X %~% N(mu(X), 1)~
#                 ","~~mu(X) == 3*X
#         )
#     )

# Needed if running through source()
print(fig)
