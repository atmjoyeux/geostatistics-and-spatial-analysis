library(ggplot2)
library(dplyr)
library(circular)

# Load data
inerstect <- read.csv("/Users/adelejoyeux/Downloads/CB24/article 1/IWP/11aaaaaa_data/riviere_angles.csv")
# my CSV has comma delimited and dot for decimal


# Define bin width
bin_width <- 10  # adjust as you like
breaks <- seq(0, 360, by=bin_width)

# Force all bins to exist (even empty ones)
all_bins <- data.frame(
  angle_bin = cut(breaks[-length(breaks)], breaks=breaks, right=FALSE),
  bin_start = breaks[-length(breaks)]
)

# Assign data to bins
inerstect$angle_bin <- cut(inerstect$angle, breaks=breaks, right=FALSE)

# Count observations per bin
bin_counts <- inerstect %>%
  group_by(angle_bin) %>%
  summarize(count = n(), .groups="drop") %>%
  right_join(all_bins, by="angle_bin") %>%
  mutate(
    count = ifelse(is.na(count), 0, count),
    Angle = bin_start + bin_width/2,
    Percentage = 100 * count / sum(count)
  )

# Your angles as a circular object
angles_circ <- circular(inerstect$angle, units="degrees", modulo="2pi")

# Compute circular mean
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



