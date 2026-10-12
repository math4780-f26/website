# ============================================================
# Error distributions and what they suggest in regression
# ============================================================

set.seed(4780)

library(ggplot2)
library(patchwork)

theme_set(
    theme_classic(base_size = 14) +
        theme(
            plot.title = element_text(face = "bold"),
            panel.grid.minor = element_blank(),
            legend.position = "bottom"
        )
)

# ============================================================
# 1. HEAVIER-TAILED ERRORS
#    Compare N(0,1) with raw t(3)
# ============================================================

x_ht <- seq(-6, 6, length.out = 2000)

df_ht <- rbind(
    data.frame(
        error = x_ht,
        density = dnorm(x_ht),
        Distribution = "N(0,1)"
    ),
    data.frame(
        error = x_ht,
        density = dt(x_ht, df = 3),
        Distribution = "t(3)"
    )
)

p1 <- ggplot(
    df_ht,
    aes(x = error, y = density, color = Distribution)
) +
    geom_line(linewidth = 1.2) +
    labs(
        title = "Heavier-tailed errors",
        x = "Error",
        y = "Density",
        color = NULL
    ) +
    coord_cartesian(xlim = c(-6, 6))


# ------------------------------------------------------------
# Regression example with t(3) errors
# ------------------------------------------------------------

n1 <- 120

x1 <- runif(n1, 0, 10)

error1 <- 2 * rt(n1, df = 3)

y1 <- 2 + 1.5 * x1 + error1

fit1 <- lm(y1 ~ x1)

res1 <- resid(fit1)

cut1 <- quantile(abs(res1), 0.95)

df1 <- data.frame(
    x = x1,
    y = y1,
    extreme = abs(res1) > cut1
)

p4 <- ggplot(df1, aes(x = x, y = y)) +
    geom_point(
        data = subset(df1, !extreme),
        aes(shape = "Most data"),
        size = 2.4,
        alpha = 0.75
    ) +
    geom_point(
        data = subset(df1, extreme),
        aes(shape = "Extreme residuals"),
        size = 3.2,
        color = 2
    ) +
    geom_smooth(
        aes(linetype = "LS line"),
        method = "lm",
        se = FALSE,
        linewidth = 1.1
    ) +
    scale_shape_manual(
        values = c(
            "Most data" = 16,
            "Extreme residuals" = 18
        )
    ) +
    scale_linetype_manual(
        values = c("LS line" = "solid")
    ) +
    labs(
        title = "Heavier tails create more outliers",
        x = "x",
        y = "y",
        shape = NULL,
        linetype = NULL
    )


# ============================================================
# 2. SKEWED ERRORS
#    Right-skewed errors with mean 0
# ============================================================

# Gamma(shape = 2, scale = 1) has mean 2
# Subtract 2 so the error distribution has mean 0

x_sk <- seq(-2, 10, length.out = 2000)

density_sk <- ifelse(
    x_sk >= -2,
    dgamma(x_sk + 2, shape = 2, scale = 1),
    0
)

# Exact mean after shifting
mean_sk <- 0

# Median of shifted Gamma(2,1)
median_sk <- qgamma(0.5, shape = 2, scale = 1) - 2

df_sk <- data.frame(
    error = x_sk,
    density = density_sk
)

p2 <- ggplot(df_sk, aes(x = error, y = density)) +
    geom_line(linewidth = 1.2) +
    geom_vline(
        xintercept = median_sk,
        linetype = "dashed",
        linewidth = 1,
        color = 3
    ) +
    geom_vline(
        xintercept = mean_sk,
        linetype = "dashed",
        linewidth = 1,
        color = 4
    ) +
    annotate(
        "text",
        x = median_sk - 0.5,
        y = 0.18,
        label = "median",
        angle = 90,
        vjust = 1,
        color = 3
    ) +
    annotate(
        "text",
        x = mean_sk + 0.5,
        y = 0.18,
        label = "mean",
        angle = 90,
        vjust = 0,
        color = 4
    ) +
    annotate(
        "text",
        x = 4.3,
        y = 0.14,
        label = "Long right tail\npulls the mean\nrightward.",
        hjust = 0
    ) +
    annotate(
        "segment",
        x = 5.8,
        y = 0.11,
        xend = 7.4,
        yend = 0.02,
        arrow = arrow(length = unit(0.18, "cm"))
    ) +
    labs(
        title = "Skewed errors",
        x = "Error",
        y = "Density"
    )


# ------------------------------------------------------------
# Regression example with right-skewed errors
# ------------------------------------------------------------

n2 <- 120

x2 <- runif(n2, 0, 10)

error2 <- rgamma(
    n2,
    shape = 2,
    scale = 1.2
) - 2.4

# mean(error2) = 0 theoretically

y2 <- 2 + 1.5 * x2 + error2

cut2 <- quantile(error2, 0.95)

df2 <- data.frame(
    x = x2,
    y = y2,
    extreme = error2 > cut2
)

p5 <- ggplot(df2, aes(x = x, y = y)) +
    geom_point(
        data = subset(df2, !extreme),
        aes(shape = "Most data"),
        size = 2.4,
        alpha = 0.75
    ) +
    geom_point(
        data = subset(df2, extreme),
        aes(shape = "Large positive errors"),
        size = 3.2,
        color = 2
    ) +
    geom_smooth(
        aes(linetype = "LS line"),
        method = "lm",
        se = FALSE,
        linewidth = 1.1
    ) +
    scale_shape_manual(
        values = c(
            "Most data" = 16,
            "Large positive errors" = 17
        )
    ) +
    scale_linetype_manual(
        values = c("LS line" = "solid")
    ) +
    labs(
        title = "Right-skewed errors: outliers in the skew direction",
        x = "x",
        y = "y",
        shape = NULL,
        linetype = NULL
    )


# ============================================================
# 3. MULTIMODAL RESIDUALS
#    Omitted categorical regressor
# ============================================================

n3 <- 160

x3 <- runif(n3, 0, 10)

group3 <- sample(
    c("Group A", "Group B"),
    size = n3,
    replace = TRUE
)

# True model contains a group effect
y3 <- 3 +
    1.2 * x3 +
    ifelse(group3 == "Group A", -2.5, 2.5) +
    rnorm(n3, mean = 0, sd = 0.7)

# Deliberately omit group
fit3 <- lm(y3 ~ x3)

res3 <- resid(fit3)

df_res3 <- data.frame(
    residual = res3
)

p3 <- ggplot(df_res3, aes(x = residual)) +
    geom_density(linewidth = 1.2) +
    annotate(
        "text",
        x = min(res3) + 0.5,
        y = 0.21,
        label = "Suggests unmodeled nsubgroups.",
        hjust = 0
    ) +
    labs(
        title = "Multimodal residuals",
        x = "Residual",
        y = "Density"
    )


# ------------------------------------------------------------
# Show omitted categorical variable explicitly
# ------------------------------------------------------------

df3 <- data.frame(
    x = x3,
    y = y3,
    group = group3
)

coef3 <- coef(fit3)

p6 <- ggplot(
    df3,
    aes(x = x, y = y, color = group, shape = group)
) +
    geom_point(
        size = 2.4,
        alpha = 0.75
    ) +
    geom_abline(
        intercept = coef3[1],
        slope = coef3[2],
        linewidth = 1.1,
        color = "blue"
    ) +
    labs(
        title = "Omitted categorical regressor",
        x = "x",
        y = "y",
        color = NULL,
        shape = NULL
    ) +
    annotate(
        "text",
        x = 5.7,
        y = min(y3) + 1,
        label = "One line (group omitted)",
        hjust = 0
    )


# ============================================================
# Combine all 6 plots
# ============================================================
# 
# final_plot <-
#     (p1 | p2 | p3) /
#     (p4 | p5 | p6) +
#     plot_annotation(
#         title = "Error distributions and what they suggest in regression",
#         theme = theme(
#             plot.title = element_text(
#                 size = 18,
#                 face = "bold",
#                 hjust = 0.5
#             )
#         )
#     )
# 
# final_plot


# ============================================================
# Save
# ============================================================
# 
# ggsave(
#     "error_distribution_regression_diagnostics.png",
#     final_plot,
#     width = 16,
#     height = 10,
#     dpi = 300
# )