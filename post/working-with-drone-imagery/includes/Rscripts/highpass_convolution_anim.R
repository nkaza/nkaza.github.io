# High-pass Filter (3x3) with Progressive Animation
# -------------------------------------------------

library(terra)
library(magick)
library(grid)
library(gridExtra)

# ---- Parameters
n <- 15
kernel_size <- 3
pad <- floor(kernel_size / 2)

# ---- Create binary raster with visible structure
set.seed(123)
mat <- matrix(0, n, n)
mat[5:11, 5:11] <- 1              # central block
noise_idx <- sample(1:(n*n), 25)  # sprinkle noise
mat[noise_idx] <- 1
r <- rast(mat)

# ---- Define 3x3 High-pass kernel
highpass_kernel <- matrix(c(-1,-1,-1,
                            -1, 8,-1,
                            -1,-1,-1), nrow=3, byrow=TRUE)

# ---- Apply high-pass with terra::focal
r_hp <- focal(r, w = highpass_kernel,
              fun = sum, na.rm = TRUE)
mat_hp <- as.matrix(r_hp, wide = TRUE)

# Normalize for display (0–1)
mat_hp_norm <- (mat_hp - min(mat_hp, na.rm=TRUE)) / 
  (max(mat_hp, na.rm=TRUE) - min(mat_hp, na.rm=TRUE))

# ---- Helpers
plot_raster <- function(mat, kernel_pos = NULL, highlight_pos = NULL, na_white = TRUE) {
  n <- nrow(mat)
  display <- mat
  if (na_white) display[is.na(display)] <- 0
  display <- display / max(display, na.rm=TRUE) # normalize grayscale
  base <- rasterGrob(1 - display, interpolate = FALSE, width = 1, height = 1)
  grobs <- list(base)
  if (!is.null(kernel_pos)) {
    i <- kernel_pos[1]; j <- kernel_pos[2]
    rect <- rectGrob(
      x = (j - 0.5) / n, y = 1 - (i - 0.5) / n,
      width = kernel_size / n, height = kernel_size / n,
      gp = gpar(fill = NA, col = "red", lwd = 2)
    )
    grobs <- c(grobs, list(rect))
  }
  if (!is.null(highlight_pos)) {
    i <- highlight_pos[1]; j <- highlight_pos[2]
    rect <- rectGrob(
      x = (j - 0.5) / n, y = 1 - (i - 0.5) / n,
      width = 1 / n, height = 1 / n,
      gp = gpar(fill = NA, col = "blue", lwd = 2)
    )
    grobs <- c(grobs, list(rect))
  }
  gTree(children = do.call(gList, grobs))
}

plot_kernel_info <- function(mat, out_val, i, j, ksize = 3) {
  pad <- floor(ksize/2)
  block <- mat[(i - pad):(i + pad), (j - pad):(j + pad)]
  
  tbl1 <- tableGrob(block, rows = NULL, cols = NULL,
                    theme = ttheme_minimal(base_size = 10))
  
  tbl2 <- tableGrob(
    data.frame("HP Value" = round(out_val, 3)),
    rows = NULL,
    theme = ttheme_minimal(base_size = 12)
  )
  
  arrangeGrob(tbl1, tbl2, ncol = 2, widths = c(2, 1))
}

# ---- Build frames with progressive output
frames <- list()
mat_partial <- matrix(NA, n, n)

for (i in (1 + pad):(n - pad)) {
  for (j in (1 + pad):(n - pad)) {
    # Compute convolution at current location
    block <- mat[(i-pad):(i+pad), (j-pad):(j+pad)]
    hp_val <- sum(block * highpass_kernel)
    
    # Normalize to 0–1 range
    hp_val_norm <- (hp_val - min(mat_hp, na.rm=TRUE)) /
      (max(mat_hp, na.rm=TRUE) - min(mat_hp, na.rm=TRUE))
    
    mat_partial[i, j] <- hp_val_norm
    
    # Top row
    left <- plot_raster(mat, kernel_pos = c(i, j))
    right <- plot_raster(mat_partial, highlight_pos = c(i, j))
    top_row <- arrangeGrob(left, right, ncol = 2)
    
    # Bottom row
    bottom <- plot_kernel_info(mat, hp_val_norm, i, j, kernel_size)
    
    frame_grob <- arrangeGrob(
      top_row, bottom,
      ncol = 1,
      heights = c(2, 1),
      top = textGrob("High-pass Filter (3×3 Kernel)",
                     gp = gpar(fontsize = 16))
    )
    
    img <- image_graph(width = 700, height = 500, res = 96)
    grid.draw(frame_grob)
    dev.off()
    frames <- c(frames, list(img))
  }
}

# ---- Save GIF
animation <- image_animate(image_join(frames), fps = 5, loop = 0)
image_write(animation, "highpass_filter.gif")
