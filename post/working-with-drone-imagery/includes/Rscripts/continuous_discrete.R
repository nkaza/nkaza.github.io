# ==========================
# Continuous Surface to Raster Animation
# ==========================

# Install and load required packages
pkgs <- c("ggplot2", "cowplot", "magick", "dplyr", "viridis")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(ggplot2)
library(cowplot)
library(magick)
library(dplyr)
library(viridis)

# Function to generate continuous surface data
generate_continuous_surface <- function(width = 100, height = 100, t = 0) {
  # Create coordinate grids
  x_seq <- seq(0, 4, length.out = width)
  y_seq <- seq(0, 4, length.out = height)
  
  # Generate surface using mathematical functions
  surface_data <- expand.grid(x = x_seq, y = y_seq)
  surface_data$x_idx <- rep(1:width, height)
  surface_data$y_idx <- rep(1:height, each = width)
  
  # Create interesting terrain using sine waves
  surface_data$value <- with(surface_data, {
    val <- sin(x * 2 + t) * cos(y * 1.5 + t * 0.5)
    val <- val + sin(x * 3 - y * 2 + t * 0.8) * 0.5
    val <- val + sin((x + y) * 1.5 + t * 0.3) * 0.3
    # Normalize to 0-1 range
    (val + 2) / 4
  })
  
  # Ensure values are between 0 and 1
  surface_data$value <- pmax(0, pmin(1, surface_data$value))
  
  return(surface_data)
}

# Function to create raster (sampled) data
create_raster_data <- function(continuous_data, grid_size = 10) {
  width <- max(continuous_data$x_idx)
  height <- max(continuous_data$y_idx)
  
  # Calculate raster grid dimensions
  cols <- floor(width / grid_size)
  rows <- floor(height / grid_size)
  
  raster_data <- data.frame()
  
  for (row in 1:rows) {
    for (col in 1:cols) {
      # Calculate pixel center coordinates
      center_x_idx <- col * grid_size - grid_size/2
      center_y_idx <- row * grid_size - grid_size/2
      
      # Find closest point in continuous data
      closest_idx <- which.min(abs(continuous_data$x_idx - center_x_idx) + 
                                 abs(continuous_data$y_idx - center_y_idx))
      
      # Sample value at this point
      sampled_value <- continuous_data$value[closest_idx]
      actual_x <- continuous_data$x[closest_idx]
      actual_y <- continuous_data$y[closest_idx]
      
      # Create raster cell
      pixel_data <- data.frame(
        x_min = actual_x - (continuous_data$x[2] - continuous_data$x[1]) * grid_size/2,
        x_max = actual_x + (continuous_data$x[2] - continuous_data$x[1]) * grid_size/2,
        y_min = actual_y - (continuous_data$y[width+1] - continuous_data$y[1]) * grid_size/2,
        y_max = actual_y + (continuous_data$y[width+1] - continuous_data$y[1]) * grid_size/2,
        center_x = actual_x,
        center_y = actual_y,
        value = sampled_value,
        pixel_id = paste(row, col, sep = "_")
      )
      
      raster_data <- rbind(raster_data, pixel_data)
    }
  }
  
  return(raster_data)
}

# Function to create one frame of animation
create_frame <- function(t, frame_num) {
  # Generate continuous surface
  continuous_data <- generate_continuous_surface(width = 80, height = 80, t = t)
  
  # Create raster version
  raster_data <- create_raster_data(continuous_data, grid_size = 8)
  
  # Plot continuous surface
  p_continuous <- ggplot(continuous_data, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", 
                         midpoint = 0.5, name = "Value") +
    coord_equal() +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    labs(title = "Continuous Surface", x = NULL, y = NULL)
  
  # Plot raster (sampled) version
  p_raster <- ggplot(raster_data) +
    # Draw raster cells
    geom_rect(aes(xmin = x_min, xmax = x_max, 
                  ymin = y_min, ymax = y_max, 
                  fill = value), 
              color = "black", size = 0.3) +
    # Add center points
    geom_point(aes(x = center_x, y = center_y), 
               color = "black", size = 1.5) +
    scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", 
                         midpoint = 0.5, name = "Value") +
    coord_equal() +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      plot.title = element_text(size = 14, hjust = 0.5),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    labs(title = "Raster (Sampled at Pixels)", x = NULL, y = NULL)
  
  # Create arrow annotation
  arrow_plot <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "→", size = 20, hjust = 0.5) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  # Combine plots
  combined_plot <- plot_grid(
    p_continuous, 
    arrow_plot, 
    p_raster, 
    nrow = 1, 
    rel_widths = c(1, 0.2, 1),
    align = "h"
  )
  
  # Add main title and explanation
  title_plot <- ggplot() + 
    annotate("text", x = 0.5, y = 0.8, 
             label = "Raster Data: Continuous Surface → Discrete Sampling", 
             size = 6, hjust = 0.5, fontface = "bold") +
    theme_void() +
    theme(plot.margin = margin(5, 5, 5, 5))
  
  # Final layout
  final_plot <- plot_grid(
    title_plot,
    combined_plot,
    nrow = 2,
    rel_heights = c(0.15, 1)
  )
  
  return(final_plot)
}

# Generate animation frames
cat("Generating animation frames...\n")
n_frames <- 30
time_sequence <- seq(0, 2*pi, length.out = n_frames)

frames <- list()
for (i in 1:n_frames) {
  cat("Creating frame", i, "of", n_frames, "\n")
  frames[[i]] <- create_frame(time_sequence[i], i)
}

# Save frames as temporary PNG files
cat("Saving frames as PNG files...\n")
temp_dir <- tempdir()
frame_files <- character(n_frames)

for (i in 1:n_frames) {
  filename <- file.path(temp_dir, paste0("frame_", sprintf("%03d", i), ".png"))
  ggsave(filename, frames[[i]], width = 12, height = 8, dpi = 100, bg = "white")
  frame_files[i] <- filename
}

# Create GIF using magick
cat("Creating GIF animation...\n")
# Read all frames
images <- image_read(frame_files)

# Create animation
animation <- images %>%
  image_animate(fps = 4) %>%  # 4 frames per second
  image_write("raster_continuous_surface.gif")

cat("✅ GIF saved as 'raster_continuous_surface.gif'\n")

# Clean up temporary files
file.remove(frame_files)
cat("🧹 Temporary files cleaned up\n")

# Display some information about the created GIF
cat("\n📊 Animation Details:\n")
cat("- Frames:", n_frames, "\n")
cat("- Frame rate: 4 FPS\n")
cat("- Duration:", n_frames/4, "seconds\n")
cat("- Shows continuous surface being sampled at discrete pixel locations\n")