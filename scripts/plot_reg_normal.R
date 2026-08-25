# ============================================================
# Regression illustration with conditional Normal distributions
# Final refined framing version
#
# Fixes:
# (1) reduce excessive blank space at the top
# (2) avoid cutting off the lower blue region and x-axis
# (3) preserve the improved distribution geometry
# Base R only
# ============================================================

# ----------------------------
# 1. Global styling
# ----------------------------
bg_col        <- "white"
plane_col     <- "#cfe0f3"
blue_col      <- "#2450d3"
blue_pt_col   <- "navy"
red_col       <- "#ef3b2c"
yellow_col    <- "#f6ef99"
green_col     <- "#2ca25f"
orange_col    <- "#f26b38"
dash_col      <- "#ef7d6a"
axis_col      <- "gray20"
text_col      <- "gray25"

# ----------------------------
# 2. Main geometric objects
# ----------------------------
A <- c(0.10, 0.86)   # front-left
B <- c(2.82, 3.56)   # back-left
C <- c(10.92, 2.52)  # back-right
D <- A + (C - B)     # front-right

plane <- rbind(A, B, C, D)

# Regression line
line_p1 <- c(0.78, 1.18)
line_p2 <- c(9.92, 1.77)

# Point on regression line, indexed by t in [0, 1]
line_pt <- function(t) {
    line_p1 + t * (line_p2 - line_p1)
}

# Convenience helper for points
pt <- function(t, dx = 0, dy = 0) {
    unname(line_pt(t) + c(dx, dy))
}

# Centers of the 3 conditional distributions
centers <- lapply(c(0.22, 0.44, 0.72), line_pt)

# ----------------------------
# 3. Geometry helpers
# ----------------------------
y_axis_vec <- plane[2, ] - plane[1, ]
u_y <- y_axis_vec / sqrt(sum(y_axis_vec^2))

# ----------------------------
# 4. Drawing helpers
# ----------------------------

draw_plane <- function() {
    
    polygon(
        x = plane[, 1],
        y = plane[, 2],
        col = plane_col,
        border = NA
    )
    
    # axes
    segments(
        plane[1, 1], plane[1, 2],
        plane[2, 1], plane[2, 2],
        lwd = 2,
        col = axis_col
    )
    
    segments(
        plane[1, 1], plane[1, 2],
        plane[4, 1], plane[4, 2],
        lwd = 2,
        col = axis_col
    )
    
    # axis labels
    text(
        x = 4.78,
        y = 0.16,
        labels = "x",
        cex = 2.10,
        col = "black"
    )
    
    text(
        x = 1.33,
        y = 2.55,
        labels = "y",
        cex = 2.10,
        col = "black"
    )
    
    # equation label
    text(
        x = 8.20,
        y = 1.53,
        labels = expression(mu[y * "|" * x] == beta[0] + beta[1] * x),
        pos = 4,
        cex = 2.45,
        col = text_col
    )
}

draw_regression_line <- function(col = blue_col, lwd = 5.3) {
    segments(
        line_p1[1], line_p1[2],
        line_p2[1], line_p2[2],
        col = col,
        lwd = lwd
    )
}

draw_parallel_bands <- function(shift = 0.55, col = dash_col) {
    
    upper1 <- line_p1 + c(-0.24,  shift)
    upper2 <- line_p2 + c( 0.24,  shift)
    
    lower1 <- line_p1 + c(-0.24, -shift)
    lower2 <- line_p2 + c( 0.24, -shift)
    
    segments(
        upper1[1], upper1[2],
        upper2[1], upper2[2],
        col = col,
        lwd = 2.8,
        lty = 2
    )
    
    segments(
        lower1[1], lower1[2],
        lower2[1], lower2[2],
        col = col,
        lwd = 2.8,
        lty = 2
    )
}

# ------------------------------------------------------------
# Normal-distribution drawing
# - baseline parallel to y-axis
# - density rises vertically upward
# - peak projects onto the regression line
# - green line is vertical
# ------------------------------------------------------------
draw_density <- function(center,
                         axis_scale = 0.31,
                         height_scale = 1.95,
                         fill = yellow_col,
                         border = blue_col,
                         mean_col = green_col,
                         lwd = 4.5) {
    
    u <- seq(-2.8, 2.8, length.out = 350)
    dens <- dnorm(u)
    
    # Baseline parallel to y-axis
    base_x <- center[1] + axis_scale * u * u_y[1]
    base_y <- center[2] + axis_scale * u * u_y[2]
    
    # Density rises vertically upward
    curve_x <- base_x
    curve_y <- base_y + height_scale * dens
    
    # Filled polygon between baseline and density curve
    polygon(
        x = c(curve_x, rev(base_x)),
        y = c(curve_y, rev(base_y)),
        col = fill,
        border = NA
    )
    
    # Density outline
    lines(
        curve_x,
        curve_y,
        col = border,
        lwd = lwd
    )
    
    # Peak at u = 0
    i0 <- which.min(abs(u))
    
    # Vertical green mean line
    segments(
        x0 = center[1],
        y0 = center[2],
        x1 = center[1],
        y1 = curve_y[i0],
        col = mean_col,
        lwd = 4
    )
}

draw_densities <- function() {
    for (cc in centers) draw_density(cc)
}

draw_big_points_set1 <- function() {
    
    blue_pts <- rbind(
        pt(0.06, -0.12,  0.18),
        pt(0.19,  0.08,  0.25),
        pt(0.48,  0.05,  0.28),
        pt(0.63,  0.35,  0.23),
        pt(0.79, -0.10,  0.13),
        pt(0.88,  0.02, -0.02)
    )
    
    red_pts <- rbind(
        pt(0.10,  0.02,  0.65),
        pt(0.17,  0.22, -0.28),
        pt(0.39,  0.20,  0.02),
        pt(0.58, -0.02, -0.35),
        pt(0.83,  0.15,  0.42),
        pt(0.91,  0.12,  0.08)
    )
    
    points(
        blue_pts[, 1], blue_pts[, 2],
        pch = 16, cex = 2.45, col = blue_pt_col
    )
    
    points(
        red_pts[, 1], red_pts[, 2],
        pch = 16, cex = 2.45, col = red_col
    )
}

draw_big_points_set2 <- function() {
    
    red_pts <- rbind(
        pt(0.10,  0.02,  0.65),
        pt(0.17,  0.22, -0.28),
        pt(0.39,  0.20,  0.02),
        pt(0.58, -0.02, -0.35),
        pt(0.83,  0.15,  0.42),
        pt(0.91,  0.12,  0.08)
    )
    
    points(
        red_pts[, 1], red_pts[, 2],
        pch = 16, cex = 2.45, col = red_col
    )
}

draw_small_orange_points <- function() {
    
    # sm <- rbind(
    #     pt(0.15, -0.05, -0.22),
    #     pt(0.19,  0.00,  0.10),
    #     pt(0.24,  0.05,  0.20),
    #     pt(0.31,  0.00,  0.02),
    #     pt(0.39,  0.03,  0.14),
    #     pt(0.46,  0.02, -0.05),
    #     pt(0.55,  0.04,  0.19),
    #     pt(0.63, -0.03, -0.20),
    #     pt(0.70,  0.02, -0.05),
    #     pt(0.79, -0.02,  0.12)
    # )
    
        sm <- rbind(
            pt(0.205, -0.05, -0.22),
            pt(0.234,  0.00,  0.10),
            pt(0.24,  0.05,  0.20),
            pt(0.38,  0.02, -0.5),
            pt(0.44,  0.00,  0.02),
            pt(0.48,  0.03,  0.4),
            pt(0.705, -0.03, -0.20),
            pt(0.715,  0.02, -0.05),
            pt(0.775,  0.04,  0.49)
            # pt(0.77, -0.02,  0.62)
        )
    
    points(
        sm[, 1], sm[, 2],
        pch = 16, cex = 2.45, col = orange_col
    )
}

# ----------------------------
# 5. Draw one panel
# ----------------------------
draw_panel <- function(line_col = blue_col,
                       show_big_set1 = FALSE,
                       show_big_set2 = FALSE,
                       show_small = FALSE,
                       show_bands = FALSE,
                       lwd_reg = 5) {
    
    par(
        mar = c(0, 0, 0, 0),
        oma = c(0, 0, 0, 0),
        bg = bg_col,
        xaxs = "i",
        yaxs = "i"
    )
    
    plot.new()
    
    # Revised framing:
    # - lower ylim extends downward so x-axis and blue region are not cut
    # - upper ylim reduced to remove excess blank space
    plot.window(
        xlim = c(0.00, 11.05),
        ylim = c(-0.28, 3.82)
    )
    
    draw_plane()
    
    if (show_bands) draw_parallel_bands()
    
    draw_regression_line(
        col = line_col,
        lwd = lwd_reg
    )
    
    draw_densities()
    
    if (show_big_set1) draw_big_points_set1()
    if (show_big_set2) draw_big_points_set2()
    if (show_small)    draw_small_orange_points()
}

# draw_panel(line_col = blue_col, show_big_set1 = TRUE)
# draw_panel(line_col = blue_col, show_big_set2 = TRUE)
# draw_panel(line_col = red_col)
# draw_panel(line_col = red_col, show_small = TRUE, show_bands = TRUE)
# draw_panel(line_col = blue_col)
# ----------------------------
# 6. Save figures
# ----------------------------

# # Figure 1
# png("regression_figure_1_final_framing.png",
#     width = 1350, height = 700, res = 160, bg = bg_col)
# draw_panel(line_col = blue_col, show_big_set1 = TRUE)
# dev.off()
# 
# # Figure 2
# png("regression_figure_2_final_framing.png",
#     width = 1350, height = 700, res = 160, bg = bg_col)
# draw_panel(line_col = blue_col, show_big_set2 = TRUE)
# dev.off()
# 
# # Figure 3
# png("regression_figure_3_final_framing.png",
#     width = 1350, height = 700, res = 160, bg = bg_col)
# draw_panel(line_col = red_col)
# dev.off()
# 
# # Figure 4
# png("regression_figure_4_final_framing.png",
#     width = 1350, height = 700, res = 160, bg = bg_col)
# draw_panel(line_col = red_col, show_small = TRUE, show_bands = TRUE)
# dev.off()
# 
# # Figure 5
# png("regression_figure_5_final_framing.png",
#     width = 1350, height = 700, res = 160, bg = bg_col)
# draw_panel(line_col = blue_col, show_small = TRUE, show_bands = TRUE)
# dev.off()
# 
# # Figure 6
# png("regression_figure_6_final_framing.png",
#     width = 1350, height = 700, res = 160, bg = bg_col)
# draw_panel(line_col = blue_col)
# dev.off()


# # ============================================================
# # Regression illustration with conditional Normal distributions
# # Base R only
# # ============================================================
# 
# # ----------------------------
# # 1. Global styling
# # ----------------------------
# bg_col        <- "white"
# plane_col     <- "#cfe0f3"
# blue_col      <- 4
# blue_pt_col   <- "navy"
# red_col       <- "#ef3b2c"
# yellow_col    <- "#ffffe0"
# green_col     <- "#2ca25f"
# orange_col    <- "#f26b38"
# dash_col      <- "#ef7d6a"
# axis_col      <- "gray20"
# text_col      <- "gray25"
# 
# # ----------------------------
# # 2. Main geometric objects
# # ----------------------------
# 
# # Exact parallelogram in screen coordinates.
# #
# # The opposite sides are parallel:
# # plane[2, ] - plane[1, ] = plane[3, ] - plane[4, ]
# # plane[3, ] - plane[2, ] = plane[4, ] - plane[1, ]
# #
# # The plane is also larger than in the original version.
# 
# plane <- rbind(
#     c(0.15,  0.55),   # front-left
#     c(2.4,  3.55),   # back-left
#     c(11, 2.85),   # back-right
#     c(9.25, -0.15)    # front-right
# )
# 
# # Regression line endpoints
# line_p1 <- c(0.75, 1.05)
# line_p2 <- c(10.65, 1.82)
# 
# # Point on regression line, indexed by t in [0,1]
# line_pt <- function(t) {
#     line_p1 + t * (line_p2 - line_p1)
# }
# 
# # Convenient helper for offsetting a point on the line
# pt <- function(t, dx = 0, dy = 0) {
#     unname(line_pt(t) + c(dx, dy))
# }
# 
# # Centers for the three Normal curves
# centers <- lapply(c(0.23, 0.43, 0.73), line_pt)
# 
# # ----------------------------
# # 3. Drawing helpers
# # ----------------------------
# 
# draw_plane <- function() {
#     
#     # Light-blue parallelogram
#     polygon(
#         x = plane[, 1],
#         y = plane[, 2],
#         col = yellow_col,
#         border = NA
#     )
#     
#     # Axes
#     segments(
#         plane[1, 1], plane[1, 2],
#         plane[2, 1], plane[2, 2],
#         lwd = 2,
#         col = axis_col
#     )
#     
#     segments(
#         plane[1, 1], plane[1, 2],
#         plane[4, 1], plane[4, 2],
#         lwd = 2,
#         col = axis_col
#     )
#     
#     # Axis labels
#     text(
#         x = 4.85,
#         y = 0.10,
#         labels = "x",
#         cex = 1.5
#     )
#     
#     text(
#         x = 1.2,
#         y = 2.35,
#         labels = "y",
#         cex = 1.5
#     )
#     
#     # Equation label
#     text(
#         x = 0.4,
#         y = 0.82,
#         labels = expression(mu[y] == beta[0] + beta[1] * x),
#         pos = 4,
#         cex = 2,
#         col = text_col
#     )
# }
# 
# draw_regression_line <- function(col = blue_col, lwd = 9) {
#     segments(
#         line_p1[1], line_p1[2],
#         line_p2[1], line_p2[2],
#         col = col,
#         lwd = lwd
#     )
# }
# 
# draw_parallel_bands <- function(shift = 0.55, col = dash_col) {
#     
#     # Vertical offsets representing approximately plus/minus sigma
#     upper1 <- line_p1 + c(-0.25,  shift)
#     upper2 <- line_p2 + c( 0.25,  shift)
#     
#     lower1 <- line_p1 + c(-0.25, -shift)
#     lower2 <- line_p2 + c( 0.25, -shift)
#     
#     segments(
#         upper1[1], upper1[2],
#         upper2[1], upper2[2],
#         col = col,
#         lwd = 1.8,
#         lty = 2
#     )
#     
#     segments(
#         lower1[1], lower1[2],
#         lower2[1], lower2[2],
#         col = col,
#         lwd = 2,
#         lty = 2
#     )
# }
# 
# # The Normal-distribution construction is unchanged
# draw_density <- function(center,
#                          width_vec = c(0.18, 0.32),
#                          height_scale = 2.7,
#                          fill = yellow_col,
#                          border = blue_col,
#                          mean_col = green_col,
#                          lwd = 5) {
#     
#     u <- seq(-2.8, 2.8, length.out = 250)
#     dens <- dnorm(u)
#     
#     # Base line direction
#     base_x <- center[1] + u * width_vec[1]
#     base_y <- center[2] + u * width_vec[2]
#     
#     # Bell curve lifted upward
#     curve_x <- base_x
#     curve_y <- base_y + dens * height_scale
#     
#     # Filled polygon under the curve
#     polygon(
#         x = c(curve_x, rev(base_x)),
#         y = c(curve_y, rev(base_y)),
#         col = rgb(1, 1, 1, alpha = 0.5),
#         border = NA
#     )
#     
#     # Bell outline
#     lines(
#         curve_x,
#         curve_y,
#         col = border,
#         lwd = lwd
#     )
#     
#     # Mean line at u = 0
#     i0 <- which.min(abs(u))
#     
#     segments(
#         base_x[i0], base_y[i0],
#         curve_x[i0], curve_y[i0],
#         col = mean_col,
#         lwd = 2.5
#     )
# }
# 
# draw_big_points_set1 <- function() {
#     
#     blue_pts <- rbind(
#         pt(0.06, -0.12,  0.18),
#         pt(0.19,  0.08,  0.25),
#         pt(0.48,  0.05,  0.28),
#         pt(0.63,  0.35,  0.23),
#         pt(0.79, -0.10,  0.13),
#         pt(0.88,  0.02, -0.02)
#     )
#     
#     red_pts <- rbind(
#         pt(0.10,  0.02,  0.65),
#         pt(0.17,  0.22, -0.28),
#         pt(0.39,  0.20,  0.02),
#         pt(0.58, -0.02, -0.35),
#         pt(0.83,  0.15,  0.42),
#         pt(0.91,  0.12,  0.08)
#     )
#     
#     # Increased from cex = 1.0
#     points(
#         blue_pts[, 1],
#         blue_pts[, 2],
#         pch = 16,
#         cex = 2,
#         col = blue_pt_col
#     )
#     
#     points(
#         red_pts[, 1],
#         red_pts[, 2],
#         pch = 16,
#         cex = 2,
#         col = red_col
#     )
# }
# 
# draw_big_points_set2 <- function() {
#     
#     red_pts <- rbind(
#         pt(0.10,  0.02,  0.65),
#         pt(0.17,  0.22, -0.28),
#         pt(0.39,  0.20,  0.02),
#         pt(0.58, -0.02, -0.35),
#         pt(0.83,  0.15,  0.42),
#         pt(0.91,  0.12,  0.08)
#     )
#     
#     # Increased from cex = 1.0
#     points(
#         red_pts[, 1],
#         red_pts[, 2],
#         pch = 16,
#         cex = 2,
#         col = red_col
#     )
# }
# 
# draw_small_orange_points <- function() {
#     
#     sm <- rbind(
#         pt(0.225, -0.05, -0.22),
#         pt(0.24,  0.00,  0.10),
#         pt(0.24,  0.05,  0.20),
#         pt(0.435,  0.00,  0.02),
#         pt(0.45,  0.03,  0.4),
#         pt(0.4,  0.02, -0.5),
#         pt(0.758,  0.04,  0.49),
#         pt(0.723, -0.03, -0.20),
#         pt(0.728,  0.02, -0.05),
#         pt(0.77, -0.02,  0.62)
#     )
#     
#     # Increased from cex = 0.55
#     points(
#         sm[, 1],
#         sm[, 2],
#         pch = 16,
#         cex = 2,
#         col = orange_col
#     )
# }
# 
# draw_densities <- function() {
#     for (cc in centers) {
#         draw_density(cc)
#     }
# }
# 
# # ----------------------------
# # 4. Draw one panel
# # ----------------------------
# 
# draw_panel <- function(line_col = blue_col,
#                        show_big_set1 = FALSE,
#                        show_big_set2 = FALSE,
#                        show_small = FALSE,
#                        show_bands = FALSE,
#                        lwd_reg = 5) {
#     
#     # Zero outer plot margins
#     par(
#         mar = c(0, 0, 0, 0),
#         oma = c(0, 0, 0, 0),
#         bg = bg_col,
#         xaxs = "i",
#         yaxs = "i"
#     )
#     
#     plot.new()
#     
#     # Tighter plotting limits:
#     # less unused space and a larger apparent plane
#     plot.window(
#         xlim = c(0.00, 11.95),
#         ylim = c(-0.22, 3.95)
#     )
#     
#     draw_plane()
#     
#     if (show_bands) {
#         draw_parallel_bands()
#     }
#     
#     draw_regression_line(
#         col = line_col,
#         lwd = lwd_reg
#     )
#     
#     draw_densities()
#     
#     if (show_big_set1) {
#         draw_big_points_set1()
#     }
#     
#     if (show_big_set2) {
#         draw_big_points_set2()
#     }
#     
#     if (show_small) {
#         draw_small_orange_points()
#     }
#     
#     # No callout box, explanatory text, or arrow
# }
# # 
# # 
# # 
# # 
# # # ============================================================
# # # Regression illustration with conditional Normal distributions
# # # Base R only
# # # ============================================================
# # 
# # # ----------------------------
# # # 1. Global styling
# # # ----------------------------
# # bg_col        <- "#efefef"
# # plane_col     <- "#cfe0f3"
# # blue_col      <- 4
# # blue_pt_col   <- "navy"
# # red_col       <- "#ef3b2c"
# # yellow_col    <- "#f6ef99"
# # green_col     <- "#2ca25f"
# # orange_col    <- "#f26b38"
# # dash_col      <- "#ef7d6a"
# # axis_col      <- "gray20"
# # text_col      <- "gray25"
# # 
# # # ----------------------------
# # # 2. Main geometric objects
# # # ----------------------------
# # 
# # # Exact parallelogram in screen coordinates.
# # #
# # # The opposite sides are parallel:
# # # plane[2, ] - plane[1, ] = plane[3, ] - plane[4, ]
# # # plane[3, ] - plane[2, ] = plane[4, ] - plane[1, ]
# # #
# # # The plane is also larger than in the original version.
# # 
# # plane <- rbind(
# #     c(0.15,  0.55),   # front-left
# #     c(2.70,  3.55),   # back-left
# #     c(11.80, 2.85),   # back-right
# #     c(9.25, -0.15)    # front-right
# # )
# # 
# # # Regression line endpoints
# # line_p1 <- c(0.75, 1.05)
# # line_p2 <- c(10.65, 1.82)
# # 
# # # Point on regression line, indexed by t in [0,1]
# # line_pt <- function(t) {
# #     line_p1 + t * (line_p2 - line_p1)
# # }
# # 
# # # Convenient helper for offsetting a point on the line
# # pt <- function(t, dx = 0, dy = 0) {
# #     unname(line_pt(t) + c(dx, dy))
# # }
# # 
# # # Centers for the three Normal curves
# # centers <- lapply(c(0.23, 0.43, 0.73), line_pt)
# # 
# # # ----------------------------
# # # 3. Drawing helpers
# # # ----------------------------
# # 
# # draw_plane <- function() {
# #     
# #     # Light-blue parallelogram
# #     polygon(
# #         x = plane[, 1],
# #         y = plane[, 2],
# #         col = plane_col,
# #         border = NA
# #     )
# #     
# #     # Axes
# #     segments(
# #         plane[1, 1], plane[1, 2],
# #         plane[2, 1], plane[2, 2],
# #         lwd = 2,
# #         col = axis_col
# #     )
# #     
# #     segments(
# #         plane[1, 1], plane[1, 2],
# #         plane[4, 1], plane[4, 2],
# #         lwd = 2,
# #         col = axis_col
# #     )
# #     
# #     # Axis labels
# #     text(
# #         x = 4.85,
# #         y = 0.10,
# #         labels = "x",
# #         cex = 1.5
# #     )
# #     
# #     text(
# #         x = 1.5,
# #         y = 2.35,
# #         labels = "y",
# #         cex = 1.5
# #     )
# #     
# #     # Equation label
# #     text(
# #         x = 0.5,
# #         y = 0.82,
# #         labels = expression(mu[y] == beta[0] + beta[1] * x),
# #         pos = 4,
# #         cex = 2,
# #         col = text_col
# #     )
# # }
# # 
# # draw_regression_line <- function(col = blue_col, lwd = 9) {
# #     segments(
# #         line_p1[1], line_p1[2],
# #         line_p2[1], line_p2[2],
# #         col = col,
# #         lwd = lwd
# #     )
# # }
# # 
# # draw_parallel_bands <- function(shift = 0.55, col = dash_col) {
# #     
# #     # Vertical offsets representing approximately plus/minus sigma
# #     upper1 <- line_p1 + c(-0.25,  shift)
# #     upper2 <- line_p2 + c( 0.25,  shift)
# #     
# #     lower1 <- line_p1 + c(-0.25, -shift)
# #     lower2 <- line_p2 + c( 0.25, -shift)
# #     
# #     segments(
# #         upper1[1], upper1[2],
# #         upper2[1], upper2[2],
# #         col = col,
# #         lwd = 1.8,
# #         lty = 2
# #     )
# #     
# #     segments(
# #         lower1[1], lower1[2],
# #         lower2[1], lower2[2],
# #         col = col,
# #         lwd = 2,
# #         lty = 2
# #     )
# # }
# # 
# # # The Normal-distribution construction is unchanged
# # draw_density <- function(center,
# #                          width_vec = c(0.18, 0.32),
# #                          height_scale = 2.7,
# #                          fill = yellow_col,
# #                          border = blue_col,
# #                          mean_col = green_col,
# #                          lwd = 5) {
# #     
# #     u <- seq(-2.8, 2.8, length.out = 250)
# #     dens <- dnorm(u)
# #     
# #     # Base line direction
# #     base_x <- center[1] + u * width_vec[1]
# #     base_y <- center[2] + u * width_vec[2]
# #     
# #     # Bell curve lifted upward
# #     curve_x <- base_x
# #     curve_y <- base_y + dens * height_scale
# #     
# #     # Filled polygon under the curve
# #     polygon(
# #         x = c(curve_x, rev(base_x)),
# #         y = c(curve_y, rev(base_y)),
# #         col = rgb(1, 1, 1, alpha = 0.5),
# #         border = NA
# #     )
# #     
# #     # Bell outline
# #     lines(
# #         curve_x,
# #         curve_y,
# #         col = border,
# #         lwd = lwd
# #     )
# #     
# #     # Mean line at u = 0
# #     i0 <- which.min(abs(u))
# #     
# #     segments(
# #         base_x[i0], base_y[i0],
# #         curve_x[i0], curve_y[i0],
# #         col = mean_col,
# #         lwd = 2.5
# #     )
# # }
# # 
# # draw_big_points_set1 <- function() {
# #     
# #     blue_pts <- rbind(
# #         pt(0.06, -0.12,  0.18),
# #         pt(0.19,  0.08,  0.25),
# #         pt(0.48,  0.05,  0.28),
# #         pt(0.63,  0.35,  0.23),
# #         pt(0.79, -0.10,  0.13),
# #         pt(0.88,  0.02, -0.02)
# #     )
# #     
# #     red_pts <- rbind(
# #         pt(0.10,  0.02,  0.65),
# #         pt(0.17,  0.22, -0.28),
# #         pt(0.39,  0.20,  0.02),
# #         pt(0.58, -0.02, -0.35),
# #         pt(0.83,  0.15,  0.42),
# #         pt(0.91,  0.12,  0.08)
# #     )
# #     
# #     # Increased from cex = 1.0
# #     points(
# #         blue_pts[, 1],
# #         blue_pts[, 2],
# #         pch = 16,
# #         cex = 2,
# #         col = blue_pt_col
# #     )
# #     
# #     points(
# #         red_pts[, 1],
# #         red_pts[, 2],
# #         pch = 16,
# #         cex = 2,
# #         col = red_col
# #     )
# # }
# # 
# # draw_big_points_set2 <- function() {
# #     
# #     red_pts <- rbind(
# #         pt(0.10,  0.02,  0.65),
# #         pt(0.17,  0.22, -0.28),
# #         pt(0.39,  0.20,  0.02),
# #         pt(0.58, -0.02, -0.35),
# #         pt(0.83,  0.15,  0.42),
# #         pt(0.91,  0.12,  0.08)
# #     )
# #     
# #     # Increased from cex = 1.0
# #     points(
# #         red_pts[, 1],
# #         red_pts[, 2],
# #         pch = 16,
# #         cex = 2,
# #         col = red_col
# #     )
# # }
# # 
# # draw_small_orange_points <- function() {
# #     
# #     sm <- rbind(
# #         pt(0.225, -0.05, -0.22),
# #         pt(0.24,  0.00,  0.10),
# #         pt(0.24,  0.05,  0.20),
# #         pt(0.435,  0.00,  0.02),
# #         pt(0.45,  0.03,  0.4),
# #         pt(0.4,  0.02, -0.5),
# #         pt(0.758,  0.04,  0.49),
# #         pt(0.723, -0.03, -0.20),
# #         pt(0.728,  0.02, -0.05),
# #         pt(0.77, -0.02,  0.62)
# #     )
# #     
# #     # Increased from cex = 0.55
# #     points(
# #         sm[, 1],
# #         sm[, 2],
# #         pch = 16,
# #         cex = 2,
# #         col = orange_col
# #     )
# # }
# # 
# # draw_densities <- function() {
# #     for (cc in centers) {
# #         draw_density(cc)
# #     }
# # }
# # 
# # # ----------------------------
# # # 4. Draw one panel
# # # ----------------------------
# # 
# # draw_panel <- function(line_col = blue_col,
# #                        show_big_set1 = FALSE,
# #                        show_big_set2 = FALSE,
# #                        show_small = FALSE,
# #                        show_bands = FALSE,
# #                        lwd_reg = 5) {
# #     
# #     # Zero outer plot margins
# #     par(
# #         mar = c(0, 0, 0, 0),
# #         oma = c(0, 0, 0, 0),
# #         bg = bg_col,
# #         xaxs = "i",
# #         yaxs = "i"
# #     )
# #     
# #     plot.new()
# #     
# #     # Tighter plotting limits:
# #     # less unused space and a larger apparent plane
# #     plot.window(
# #         xlim = c(0.00, 11.95),
# #         ylim = c(-0.22, 3.95)
# #     )
# #     
# #     draw_plane()
# #     
# #     if (show_bands) {
# #         draw_parallel_bands()
# #     }
# #     
# #     draw_regression_line(
# #         col = line_col,
# #         lwd = lwd_reg
# #     )
# #     
# #     draw_densities()
# #     
# #     if (show_big_set1) {
# #         draw_big_points_set1()
# #     }
# #     
# #     if (show_big_set2) {
# #         draw_big_points_set2()
# #     }
# #     
# #     if (show_small) {
# #         draw_small_orange_points()
# #     }
# #     
# #     # No callout box, explanatory text, or arrow
# # }
# 
# 
# # draw_panel(
# #     line_col = blue_col,
# #     show_big_set1 = TRUE
# # )
# # 
# # draw_panel(
# #     line_col = blue_col,
# #     show_big_set2 = TRUE
# # )
# # 
# # draw_panel(
# #     line_col = red_col
# # )
# # 
# # draw_panel(
# #     line_col = red_col,
# #     show_small = TRUE,
# #     show_bands = TRUE
# # )
# # 
# # draw_panel(
# #     line_col = blue_col,
# #     show_small = TRUE,
# #     show_bands = TRUE
# # )
# # 
# # draw_panel(
# #     line_col = blue_col
# # )
