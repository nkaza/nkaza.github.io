library(magick)

set.seed(7)
nr <- 4; nc <- 4

band <- function(phase) {
  m <- outer(1:nr, 1:nc, function(i, j) sin(i / 1.6 + phase) + cos(j / 1.8 + phase))
  round((m - min(m)) / diff(range(m)) * 255)
}
R <- band(0); G <- band(2); B <- band(4)
composite_col <- matrix(rgb(R / 255, G / 255, B / 255), nr, nc)

cellw <- 1; cellh <- 1
gridw <- nc * cellw; gridh <- nr * cellh

draw_grid <- function(mat, x0, y0, col_fun, show_numbers, alpha = 1, border = "grey30") {
  for (i in 1:nr) {
    for (j in 1:nc) {
      x1 <- x0 + (j - 1) * cellw; x2 <- x1 + cellw
      y1 <- y0 - (i - 1) * cellh; y2 <- y1 - cellh
      col <- adjustcolor(col_fun(mat[i, j]), alpha.f = alpha)
      rect(x1, y2, x2, y1, col = col, border = adjustcolor(border, alpha.f = alpha))
      if (show_numbers > 0) {
        txtcol <- if (mean(col2rgb(col_fun(mat[i, j]))) > 140) "black" else "white"
        text((x1 + x2) / 2, (y1 + y2) / 2, mat[i, j],
             col = adjustcolor(txtcol, alpha.f = show_numbers), cex = 0.75)
      }
    }
  }
}

draw_bracket <- function(x0, y0, w, h, alpha = 1) {
  col <- adjustcolor("grey20", alpha.f = alpha)
  tick <- w * 0.12
  segments(x0, y0, x0, y0 - h, col = col, lwd = 2)
  segments(x0, y0, x0 + tick, y0, col = col, lwd = 2)
  segments(x0, y0 - h, x0 + tick, y0 - h, col = col, lwd = 2)
  segments(x0 + w, y0, x0 + w, y0 - h, col = col, lwd = 2)
  segments(x0 + w, y0, x0 + w - tick, y0, col = col, lwd = 2)
  segments(x0 + w, y0 - h, x0 + w - tick, y0 - h, col = col, lwd = 2)
}

n_explode <- 22
n_hold <- 14
n_frames <- n_explode + n_hold

frame_dir <- tempfile("mframes"); dir.create(frame_dir)
frame_files <- character(n_frames)

dx_step <- 1.2; dy_step <- -0.7   # diagonal fan-out per layer

band_cols <- list(
  function(v) rgb(v / 255, 0.08, 0.08),
  function(v) rgb(0.08, v / 255, 0.08),
  function(v) rgb(0.08, 0.08, v / 255)
)
band_mats <- list(R, G, B)
band_labels <- c("Band 1 (R)", "Band 2 (G)", "Band 3 (B)")

x_start <- 6.4; y_start <- 4.3

for (k in seq_len(n_frames)) {
  t <- min(1, (k - 1) / (n_explode - 1))
  ease <- t * t * (3 - 2 * t)

  f <- file.path(frame_dir, sprintf("frame_%03d.png", k))
  png(f, width = 1150, height = 620, res = 115)
  par(mar = c(0.5, 0.5, 2, 0.5))
  plot(NULL, xlim = c(0, 13), ylim = c(-2.3, 5.3), asp = 1,
       xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
  title(main = "A multi-band raster = a stack of matrices", cex.main = 1.15)

  # left: composite image (static)
  for (i in 1:nr) for (j in 1:nc) {
    x1 <- 0.3 + (j - 1) * cellw; x2 <- x1 + cellw
    y1 <- y_start - (i - 1) * cellh; y2 <- y1 - cellh
    rect(x1, y2, x2, y1, col = composite_col[i, j], border = "grey30")
  }
  text(0.3 + gridw / 2, y_start + 0.4, "What you see: one image", cex = 0.85, font = 2)

  # right: three band matrices fanning out diagonally from fully overlapped
  for (k2 in 3:1) {
    xk <- x_start + (k2 - 1) * ease * dx_step
    yk <- y_start + (k2 - 1) * ease * dy_step
    draw_grid(band_mats[[k2]], xk, yk, band_cols[[k2]], show_numbers = ease,
              alpha = if (ease < 0.98) max(0.6, ease) else 1)
    if (ease > 0.55) {
      lab_alpha <- (ease - 0.55) / 0.45
      draw_bracket(xk - 0.12, yk + 0.12, gridw + 0.24, gridh + 0.24, alpha = lab_alpha)
    }
  }
  if (ease > 0.55) {
    lab_alpha <- (ease - 0.55) / 0.45
    text(x_start + 1.2 + gridw / 2, y_start + 0.4, "What's stored: 3 separate matrices",
         cex = 0.85, font = 2, col = adjustcolor("grey10", alpha.f = lab_alpha))
    # one clean caption row below the whole fanned stack, clear of any grid
    cap_y <- -1.7
    cap_x <- x_start + c(0, 1, 2) * 2.1
    for (m in 1:3) {
      text(cap_x[m], cap_y, band_labels[m], cex = 0.8, font = 2,
           col = adjustcolor(c("firebrick", "forestgreen", "blue3")[m], alpha.f = lab_alpha))
    }
    text(mean(cap_x), cap_y - 0.35, paste0("each a ", nr, " x ", nc, " matrix"),
         cex = 0.75, col = adjustcolor("grey30", alpha.f = lab_alpha))
  }

  dev.off()
  frame_files[k] <- f
}

imgs <- image_read(frame_files)
anim <- image_animate(imgs, fps = 10, loop = 0)
image_write(anim, "raster_as_matrix.gif")
cat("wrote gif, size:", file.info("raster_as_matrix.gif")$size, "bytes\n")
