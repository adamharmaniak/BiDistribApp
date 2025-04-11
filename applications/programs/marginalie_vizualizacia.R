library(ggplot2)
library(ggExtra)
library(grid)
library(patchwork)
library(cowplot)

data <- read.csv("C:/Users/Adam/STUBA/Rocnik_3/Bakalárska_práca/data/penguins_size_cleaned.csv")

# 1. graf marg_proj_hist
p <- ggplot(data, aes(x = culmen_length_mm, y = culmen_depth_mm)) +
  geom_point(color = "orange", alpha = 0.7) +
  theme_minimal() +
  labs(x = expression(x[1]), y = expression(x[2])) +
  theme(
    axis.title = element_text(size = 16),
    plot.margin = margin(10, 10, 10, 20)
  )

p_with_margins <- ggMarginal(p,
                             type = "histogram",
                             margins = "y",
                             fill = "blue",
                             color = "black")

print(p_with_margins)

grid.lines(x = unit(c(0.65, 0.85), "npc"), y = unit(c(0.75, 0.75), "npc"),
           gp = gpar(col = "red", lty = "dashed", lwd = 2), arrow = arrow(length = unit(0.15, "cm")))
grid.lines(x = unit(c(0.65, 0.85), "npc"), y = unit(c(0.25, 0.25), "npc"),
           gp = gpar(col = "red", lty = "dashed", lwd = 2), arrow = arrow(length = unit(0.15, "cm")))
grid.text("Projection", x = unit(0.65, "npc"), y = unit(0.5, "npc"),
          gp = gpar(col = "red", fontsize = 12, fontface = "bold"))


# 2. graf marg_strip_sum1
# dx2_center <- 17
# dx2_width <- 1
# ymin <- dx2_center - dx2_width / 2
# ymax <- dx2_center + dx2_width / 2
# 
# p <- ggplot(data, aes(x = culmen_length_mm, y = culmen_depth_mm)) +
#   geom_point(color = "orange", alpha = 0.6) +
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax,
#            fill = "blue", alpha = 0.6) +
#   annotate("segment", x = 59, xend = 59, y = ymin, yend = ymax,
#            arrow = arrow(type = "closed", length = unit(0.2, "cm"), ends = "both"),
#            colour = "black") +
#   annotate("text", x = 60, y = mean(c(ymin, ymax)), label = expression(dx[2]),
#            parse = TRUE, angle = 90, vjust = 0.5, hjust = -0.2, size = 5) +
#   labs(x = expression(x[1]), y = expression(x[2])) +
#   theme_minimal() +
#   theme(
#     axis.title = element_text(size = 16),
#     plot.margin = margin(10, 10, 10, 10)
#   )
# 
# print(p)

# 3.graf marg_strip_sum2
# # Parametre pasma
# dx2_center <- 17
# dx2_width <- 1
# ymin <- dx2_center - dx2_width / 2
# ymax <- dx2_center + dx2_width / 2
# 
# # Parametre vyrezov (dx1)
# dx1 <- 3
# left_start <- 30
# x_centers_left <- left_start + (0:2) * dx1
# x_center_right <- 57
# x_centers <- c(x_centers_left, x_center_right)
# x_labels <- 1:4
# 
# p <- ggplot(data, aes(x = culmen_length_mm, y = culmen_depth_mm)) +
#   geom_point(color = "orange", alpha = 0.6) +
# 
#   # Horizontalne pasmo dx2
#   annotate("rect", xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax,
#            fill = "blue", alpha = 0.6) +
# 
#   annotate("segment", x = 60.5, xend = 60.5, y = ymin, yend = ymax,
#            arrow = arrow(type = "closed", length = unit(0.2, "cm"), ends = "both"),
#            colour = "black") +
#   annotate("text", x = 61.7, y = mean(c(ymin, ymax)), label = expression(dx[2]),
#            parse = TRUE, angle = 90, vjust = 0.5, hjust = -0.2, size = 5, colour = "black")
# 
# for (i in seq_along(x_centers)) {
#   xc <- x_centers[i]
#   label <- x_labels[i]
# 
#   p <- p + annotate("rect",
#                     xmin = xc - dx1/2, xmax = xc + dx1/2,
#                     ymin = ymin, ymax = ymax,
#                     colour = "red", fill = NA, size = 1.5)
# 
#   p <- p + annotate("text", x = xc, y = ymax + 0.25, label = label,
#                     size = 5, colour = "red", fontface = "bold")
# 
#   if (i == 1) {
#     p <- p + annotate("segment", x = xc - dx1/2, xend = xc + dx1/2,
#                       y = ymin - 0.25, yend = ymin - 0.25,
#                       arrow = arrow(type = "closed", length = unit(0.15, "cm"), ends = "both"),
#                       colour = "black") +
#       annotate("text", x = xc, y = ymin - 0.5, label = expression(dx[1]),
#                parse = TRUE, size = 5)
#   }
# }
# 
# x_start <- x_centers_left[3] + dx1 / 2
# x_end <- x_center_right - dx1 / 2
# y_mid <- mean(c(ymin, ymax))
# 
# p <- p + annotate("segment", x = x_start, xend = x_end,
#                   y = y_mid, yend = y_mid,
#                   arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
#                   colour = "red", linetype = "dotted", size = 1)
# 
# p <- p + labs(x = expression(x[1]), y = expression(x[2])) +
#   theme_minimal() +
#   theme(
#     axis.title = element_text(size = 16),
#     plot.margin = margin(10, 10, 10, 10)
#   )
# 
# print(p)

# 4. graf marg_condition_strip

# # Parametre
# x <- 45
# eps <- 1.5
# xmin <- x
# xmax <- x + eps
# 
# # Oznacenie bodov
# data$in_band <- with(data, culmen_length_mm >= xmin & culmen_length_mm <= xmax)
# data_subset <- subset(data, in_band)
# 
# # Histogram z pasma
# hist_plot <- ggplot(data_subset, aes(x = culmen_depth_mm)) +
#   geom_histogram(fill = "blue", color = "black", bins = 30) +
#   coord_flip() +
#   theme_minimal() +
#   theme(axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         plot.margin = margin(10, 0, 10, 0)) +
#   labs(y = expression(f(x[2] ~ "|" ~ x[1] %in% bgroup("[", paste(x, ",", x + epsilon), "]"))))
# 
# scatter_plot <- ggplot() +
#   annotate("rect", xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
#            fill = "blue", alpha = 0.1) +
# 
#   geom_point(data = subset(data, !in_band),
#              aes(x = culmen_length_mm, y = culmen_depth_mm),
#              color = "orange", alpha = 0.6) +
# 
#   geom_point(data = data_subset,
#              aes(x = culmen_length_mm, y = culmen_depth_mm),
#              color = "red", alpha = 0.6) +
# 
#   annotate("segment", x = xmin, xend = xmin, y = -Inf, yend = Inf,
#            linetype = "dashed", color = "blue", size = 1) +
#   annotate("segment", x = xmax, xend = xmax, y = -Inf, yend = Inf,
#            linetype = "dashed", color = "blue", size = 1) +
# 
#     annotate("segment", x = xmax - 0.4, xend = xmax + 14.5, y = 20.0, yend = 20.0,
#              colour = "red", linetype = "dashed", size = 0.8) +
#     annotate("segment", x = xmax + 14.5, xend = xmax + 15.4, y = 20.0, yend = 20.0,
#              colour = "red", linetype = "solid", size = 0.8,
#              arrow = arrow(length = unit(0.4, "cm"))) +
# 
#     annotate("segment", x = xmax - 0.4, xend = xmax + 14.5, y = 17.0, yend = 17.0,
#              colour = "red", linetype = "dashed", size = 0.8) +
#     annotate("segment", x = xmax + 14.5, xend = xmax + 15.4, y = 17.0, yend = 17.0,
#              colour = "red", linetype = "solid", size = 0.8,
#              arrow = arrow(length = unit(0.4, "cm"))) +
# 
#     annotate("segment", x = xmax - 0.4, xend = xmax + 14.5, y = 14.0, yend = 14.0,
#              colour = "red", linetype = "dashed", size = 0.8) +
#     annotate("segment", x = xmax + 14.5, xend = xmax + 15.4, y = 14.0, yend = 14.0,
#              colour = "red", linetype = "solid", size = 0.8,
#              arrow = arrow(length = unit(0.4, "cm"))) +
# 
#     # Texty
#     annotate("text", x = xmax + 7, y = 21, label = "Projection", color = "red", size = 5) +
#     annotate("text", x = xmin - 1.5, y = 13.2, label = "x", size = 5) +
#     annotate("text", x = xmax + 3, y = 13.2, label = expression(paste("x + ", epsilon)), size = 5) +
# 
#   labs(x = expression(x[1]), y = expression(x[2])) +
#   theme_minimal() +
#   theme(axis.title = element_text(size = 16),
#         plot.margin = margin(10, 5, 10, 10))
# 
# combined <- scatter_plot + hist_plot + plot_layout(widths = c(3, 1))
# 
# print(combined)

# 5. graf dist_transform_pred

# # Scatter plot
# scatter <- ggplot(data, aes(x = culmen_length_mm, y = culmen_depth_mm)) +
#   geom_point(color = "orange", alpha = 0.6, size = 1.5) +
#   theme_minimal() +
#   labs(x = "x", y = "y") +
#   theme(
#     plot.margin = margin(t = 2, r = 2, b = 20, l = 2),
#     axis.title.x = element_text(margin = margin(t = 10))
#   )
# 
# # Histogram pre x (dole, zrkadlovo)
# hist_x <- ggplot(data, aes(x = culmen_length_mm)) +
#   geom_histogram(bins = 30, fill = "blue", color = "black") +
#   scale_y_reverse() +
#   theme_void() +
#   theme(
#     plot.margin = margin(t = -15, r = 10, b = 0, l = 15)
#   )
# 
# # Histogram pre y (vľavo, zrkadlovo)
# max_count <- max(hist(data$culmen_depth_mm, plot = FALSE, breaks = 30)$counts)
# hist_y <- ggplot(data, aes(x = culmen_depth_mm)) +
#   geom_histogram(binwidth = 0.25, fill = "blue", color = "black") +
#   coord_flip() +
#   scale_y_reverse(limits = c(0.8 * max_count, 0)) +
#   theme_void() +
#   theme(
#     plot.margin = margin(t = 10, r = 0, b = 10, l = -10)
#   )
# 
# # Kombinácia: scatter + x-histogram
# scatter_x <- plot_grid(scatter, hist_x, ncol = 1, align = "v", rel_heights = c(1, 0.2))
# 
# # Celý graf: y-histogram + (scatter + hist_x)
# final_plot <- plot_grid(hist_y, scatter_x, ncol = 2, align = "h", rel_widths = c(0.2, 1))
# 
# # Zobrazenie
# print(final_plot)


# 6. graf dist_transform_po

# u <- ecdf(data$culmen_length_mm)(data$culmen_length_mm)
# v <- ecdf(data$culmen_depth_mm)(data$culmen_depth_mm)
# 
# copula_data <- data.frame(u = u, v = v)
# 
# scatter <- ggplot(copula_data, aes(x = u, y = v)) +
#   geom_point(size = 1.2, alpha = 0.6, color = "orange") +
#   theme_minimal() +
#   labs(x = "u", y = "v") +
#   theme(
#      plot.margin = margin(t = 2, r = 2, b = 20, l = 2),
#      axis.title.x = element_text(margin = margin(t = 10))
#   )
# 
# hist_u <- ggplot(copula_data, aes(x = u)) +
#   geom_histogram(bins = 10, fill = "blue", color = "black") +
#   scale_y_reverse() +
#   theme_void() +
#   theme(plot.margin = margin(t = -15, r = 10, b = 0, l = 10))
# 
# hist_v <- ggplot(copula_data, aes(x = v)) +
#   geom_histogram(bins = 10, fill = "blue", color = "black") +
#   coord_flip() +
#   scale_x_reverse() +
#   scale_y_reverse() +
#   theme_void() +
#   theme(plot.margin = margin(t = 30, r = 0, b = 40, l = -5))
# 
# scatter_u <- plot_grid(scatter, hist_u, ncol = 1, align = "v", rel_heights = c(1, 0.25))
# final_plot <- plot_grid(hist_v, scatter_u, ncol = 2, align = "h", rel_widths = c(0.10, 0.3))
# 
# print(final_plot)
