# ============================================================
# Title: Polar histogram of angular distribution
#
# Description:
# This script creates a polar histogram (rose diagram) of angle
# distributions and computes the circular mean direction.
# Angles are grouped into bins and expressed as percentages.
#
# Input:
# - CSV file containing angular measurements (in degrees)
#
# Output:
# - Polar histogram with:
#   * percentage per angular bin
#   * circular mean direction (red dashed line)
#
# Dependencies:
# - ggplot2
# - dplyr
# - circular
# ============================================================

# ---- 0. Load packages ----
library(ggplot2)
library(dplyr)
library(circular)
library(readr)

# ---- 1. Import data ----
# Use relative path for GitHub compatibility
data <- read_csv("data/riviere_angles.csv")

# ---- 2. Check required columns ----
required_cols <- c("angle")
stopifnot(all(required_cols %in% names(data)))

# ---- 3. Define bins ----
bin_width <- 10
breaks <- seq(0, 360, by = bin_width)

# Ensure all bins exist (including empty ones)
all_bins <- data.frame(
  angle_bin = cut(breaks[-length(breaks)], breaks = breaks, right = FALSE),
  bin_start = breaks[-length(breaks)]
)

# ---- 4. Assign observations to bins ----
data$angle_bin <- cut(data$angle, breaks = breaks, right = FALSE)

# Count observations per bin
bin_counts <- data %>%
  group_by(angle_bin) %>%
  summarize(count = n(), .groups = "drop") %>%
  right_join(all_bins, by = "angle_bin") %>%
  mutate(
    count = ifelse(is.na(count), 0, count),
    Angle = bin_start + bin_width / 2,
    Percentage = 100 * count / sum(count)
  )

# ---- 5. Circular statistics ----
angles_circ <- circular(data$angle, units = "degrees", modulo = "2pi")

mean_angle <- as.numeric(mean(angles_circ))

# Position for annotation
max_y <- max(bin_counts$Percentage) * 1.1

# ---- 6. Plot polar histogram ----
ggplot(bin_counts, aes(x = Angle, y = Percentage, fill = Percentage)) +
  geom_bar(stat = "identity", color = "black", width = bin_width) +
  coord_polar(start = -pi / 2) +
  
  scale_y_continuous(
    limits = c(0, max(bin_counts$Percentage) * 1.1),
    labels = function(x) paste0(x, "%")
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 360, by = 45),
    limits = c(0, 360),
    labels = paste0(seq(0, 360, by = 45), "°")
  ) +
  
  # Mean direction
  geom_vline(
    xintercept = mean_angle,
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  
  labs(
    title = "Polar histogram of angle distribution",
    x = "Angle (degrees)",
    y = "Percentage"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold")
  ) +
  
  # Annotation for mean angle
  annotate(
    "text",
    x = mean_angle,
    y = max_y,
    label = paste0("Mean\n", round(mean_angle, 1), "°"),
    color = "red",
    fontface = "bold",
    size = 5,
    hjust = -0.1,
    vjust = 0.15
  )# Compute circular mean
mean_angle <- mean(angles_circ)

# Compute max percentage for y placement
max_y <- max(bin_counts$Percentage) * 1.1

# Plot
ggplot(bin_counts, aes(x = Angle, y = Percentage, fill = Percentage)) +
  geom_bar(stat = "identity", color = "black", width = 10) +
  coord_polar(start = -pi/2) +
  scale_y_continuous(limits = c(0, max(bin_counts$Percentage)*1.1),
                     labels = function(x) paste0(x, "%")) +
  
  scale_x_continuous(
    breaks = seq(0, 360, by = 45),
    limits = c(0, 360),
    labels = paste0(seq(0, 360, by = 45), "°")
  ) +
  geom_vline(xintercept = as.numeric(mean_angle), color="red", linetype="dashed", size=1
  ) +
  labs(
    title = "Polar histogram of IWP-05 interior angles",
    x = "Angle",
    y = "Percentage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold")
  ) +
  annotate("text",
           x = mean_angle,
           y = max_y , # slightly below top
           label = paste0("MEAN\n", round(mean_angle, 1), "°"),
           color = "red",
           fontface = "bold",
           size = 5,
           hjust = -0.1,  # aligns left edge of text to x
           vjust = 0.15)  # moves text slightly down



