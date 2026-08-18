# ==========================
# Animated raster + table GIF - 2x2 Grid Layout
# ==========================
# Install needed packages if missing
pkgs <- c("terra", "ggplot2", "cowplot", "magick", "dplyr", "gridExtra")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(terra)
library(ggplot2)
library(cowplot)
library(magick)
library(dplyr)
library(gridExtra)
set.seed(123)

# 1. Create a 3x3 raster with 4 bands
r <- rast(nrows = 3, ncols = 3, nlyrs = 4)
values(r) <- matrix(floor(runif(ncell(r) * 4, 1, 100)), ncol = 4)

# 2. Convert to data frame for plotting
df <- as.data.frame(r, xy = TRUE, cells = TRUE)
df <- df %>%
  rename(b1 = lyr.1, b2 = lyr.2, b3 = lyr.3, b4 = lyr.4)

# Add row/col info
rc <- rowColFromCell(r, df$cell)
df$row <- rc[,1]
df$col <- rc[,2]

# 3. Function to make one animation frame
make_frame <- function(highlight_cell) {
  df$highlight <- df$cell == highlight_cell
  
  # --- Band plots with values + highlighted outline ---
  p_list <- lapply(1:4, function(i) {
    band <- paste0("b", i)
    ggplot(df, aes(x = x, y = y, fill = .data[[band]])) +
      geom_tile(color = "grey30") +
      # numeric values
      geom_text(aes(label = round(.data[[band]], 1)), size = 3, color = "black") +
      # highlight outline (red border, no fill)
      geom_tile(data = subset(df, highlight),
                aes(x = x, y = y), fill = NA, color = "red", size = 10) +
      scale_fill_gradient(low = "white", high = "white") +
      coord_equal() +
      theme_minimal() +
      theme(axis.text = element_blank(),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            legend.position = "none",
            plot.title = element_text(size = 10)) +
      ggtitle(paste("Band", i))
  })
  
  # --- Composite raster (outline highlight only) ---
  rgb_vals <- with(df, rgb(b1/100, b2/100, b3/100))
  p_comp <- ggplot(df, aes(x=x, y=y, fill=rgb_vals)) +
    geom_tile(color="grey30") +
    geom_text(aes(label = paste0("(", row, ",", col, ")")), size = 3, color = "black") +
    # outline for highlighted cell
    geom_tile(data = subset(df, highlight),
              aes(x = x, y = y), fill = NA, color = "red", size = 10) +
    scale_fill_identity() +
    coord_equal() +
    theme_minimal() +
    theme(axis.text=element_blank(),
          panel.grid = element_blank(),
          axis.title=element_blank(),
          legend.position="none",
          plot.title=element_text(size=10)) +
    ggtitle("Composite")
  
  # --- Table with highlighted row in red ---
  tab <- df %>%
    select(row, col, b1, b2, b3, b4, highlight) 
  
  tg <- tableGrob(tab[,1:6],
                  rows = NULL,
                  theme = ttheme_default(
                    core = list(fg_params = list(cex=1,
                                                 col = ifelse(tab$highlight, "red", "black"),
                                                 fontface = ifelse(tab$highlight, "bold", "plain")),
                                bg_params = list(fill = ifelse(tab$highlight,
                                                               "mistyrose", NA))),
                    colhead = list(fg_params=list(cex=1, fontface="bold"))
                  ))
  
  # --- Assemble final frame in 2x2 grid layout ---
  # Create 2x2 grid with Band 1, Band 2, Band 3, Band 4
  raster_grid <- plot_grid(p_list[[1]], p_list[[2]], 
                           p_list[[3]], p_list[[4]], 
                           nrow = 2, ncol = 2)
  
  # Add composite plot below the 2x2 grid
  left_panel <- plot_grid(raster_grid, p_comp, 
                          ncol = 1, rel_heights = c(2, 1))
  
  # Create right panel with table taking 80% height
  empty_top <- ggplot() + theme_void()
  table_plot <- ggdraw() + draw_grob(tg)
  empty_bottom <- ggplot() + theme_void()
  
  # Stack vertically with 10% empty space top, 80% table, 10% empty space bottom
  table_panel <- plot_grid(empty_top, table_plot, empty_bottom, 
                           ncol = 1, rel_heights = c(0.1, 0.8, 0.1))
  
  # Combine left panel (rasters) with right panel (table)
  final <- plot_grid(left_panel, table_panel,
                     ncol = 2, rel_widths = c(2, 1))
  
  return(final)
}

# 4. Generate frames
frames <- lapply(df$cell, make_frame)

# 5. Save frames as images
dir.create("frames", showWarnings = FALSE)
frame_files <- character(length(frames))
for (i in seq_along(frames)) {
  f <- paste0("frames/frame_", sprintf("%02d", i), ".png")
  ggsave(f, frames[[i]], width=12, height=8, dpi=120)  # Adjusted dimensions
  frame_files[i] <- f
}

# 6. Assemble into GIF
imgs <- image_read(frame_files)
anim <- image_animate(imgs, fps=1, loop=0) # 1 frame per sec
image_write(anim, "raster_animation.gif")
cat("✅ GIF saved as raster_animation.gif\n")
