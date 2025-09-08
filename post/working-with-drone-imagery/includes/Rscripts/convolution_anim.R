# Morphological Opening with terra::focal + Animated GIF
# ------------------------------------------------------
# Top left: Input raster with moving kernel
# Top right: Output raster, progressively filled as convolution proceeds
# Bottom: Current 3×3 kernel neighborhood + output pixel value

library(terra)
library(magick)
library(grid)
library(gridExtra)

# ---- Parameters
n <- 15
kernel_size <- 3
kernel <- matrix(1, nrow = kernel_size, ncol = kernel_size)
pad <- floor(kernel_size / 2)

# ---- Create binary raster with visible structure
set.seed(123)
mat <- matrix(0, n, n)
mat[5:11, 5:11] <- 1               # solid block in center
noise_idx <- sample(1:(n*n), 20)   # sprinkle noise
mat[noise_idx] <- 1
r <- rast(mat)

# ---- Morphology using terra::focal
r_eroded <- focal(r, w = kernel, fun = function(x, ...) as.integer(min(x, na.rm = TRUE)), na.rm = TRUE)
r_opened <- focal(r_eroded, w = kernel, fun = function(x, ...) as.integer(max(x, na.rm = TRUE)), na.rm = TRUE)
mat_o <- as.matrix(r_opened, wide = TRUE)

# ---- Helpers
plot_raster <- function(mat, kernel_pos = NULL, highlight_pos = NULL, na_white = TRUE) {
  n <- nrow(mat)
  display <- mat
  if (na_white) display[is.na(display)] <- 0
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
                    theme = ttheme_minimal(base_size = 12))
  
  tbl2 <- tableGrob(
    data.frame("Output" = out_val),
    rows = NULL,
    theme = ttheme_minimal(base_size = 14)
  )
  
  arrangeGrob(tbl1, tbl2, ncol = 2, widths = c(2, 1))
}

# ---- Build frames with progressive output
frames <- list()
mat_partial <- matrix(NA, n, n)  # start with empty output

for (i in (1 + pad):(n - pad)) {
  for (j in (1 + pad):(n - pad)) {
    # Compute and store current output pixel
    mat_partial[i, j] <- mat_o[i, j]
    
    # Top left: input raster with kernel
    left <- plot_raster(mat, kernel_pos = c(i, j))
    
    # Top right: progressively filled output raster
    right <- plot_raster(mat_partial, highlight_pos = c(i, j))
    top_row <- arrangeGrob(left, right, ncol = 2)
    
    # Bottom: kernel values + current output pixel
    bottom <- plot_kernel_info(mat, mat_o[i, j], i, j, kernel_size)
    
    frame_grob <- arrangeGrob(
      top_row, bottom,
      ncol = 1,
      heights = c(2, 1),
      top = textGrob("Morphological Opening (terra::focal, 3×3)",
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
image_write(animation, "morphological_opening.gif")
