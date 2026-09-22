library(magick)

set.seed(42)

# ---- shared frame count ----
n_frames <- 50

# ---- time series panel setup ----
x <- 1:70
y <- sin(x / 6) + rnorm(length(x), sd = 0.25)
half_w <- 3                      # window half-width (window = 7 points)
centers <- round(seq(half_w + 1, length(x) - half_w, length.out = n_frames))

roll_mean_at <- function(center) mean(y[(center - half_w):(center + half_w)])

# ---- raster panel setup: 7 rows x 12 cols, 3x3 window over interior cells ----
nr <- 7; nc <- 12
rmat <- outer(1:nr, 1:nc, function(i, j) sin(i / 2) + cos(j / 2.5))
positions <- expand.grid(i = 2:(nr - 1), j = 2:(nc - 1))
positions <- positions[order(positions$i, positions$j), ]
stopifnot(nrow(positions) == n_frames)

focal_mean <- function(i, j) mean(rmat[(i - 1):(i + 1), (j - 1):(j + 1)])

# precompute the full output raster and each interior cell's "visit order"
omat <- matrix(NA_real_, nr, nc)
visit_at <- matrix(Inf, nr, nc)
for (k in seq_len(n_frames)) {
  pi <- positions$i[k]; pj <- positions$j[k]
  omat[pi, pj] <- focal_mean(pi, pj)
  visit_at[pi, pj] <- k
}

raw_rng  <- range(rmat)
out_rng  <- range(omat, na.rm = TRUE)
raw_pal  <- colorRamp(hcl.colors(30, "Viridis"))
done_pal <- colorRamp(hcl.colors(30, "OrRd", rev = TRUE))
norm01 <- function(v, rng) pmin(1, pmax(0, (v - rng[1]) / diff(rng)))
raw_col  <- function(v) rgb(raw_pal(norm01(v, raw_rng)) / 255)
done_col <- function(v) rgb(done_pal(norm01(v, out_rng)) / 255)

frame_dir <- tempfile("frames"); dir.create(frame_dir)
frame_files <- character(n_frames)

for (k in seq_len(n_frames)) {

  pi <- positions$i[k]; pj <- positions$j[k]

  f <- file.path(frame_dir, sprintf("frame_%03d.png", k))
  png(f, width = 1000, height = 460, res = 110)
  par(mfrow = c(1, 2), mar = c(3, 3, 3, 1), oma = c(0, 0, 2, 0))

  # --- left: time series with sliding window ---
  cc <- centers[k]
  plot(x, y, type = "l", col = "grey60", xlab = "time", ylab = "value",
       main = "Rolling window over a time series")
  rect(cc - half_w, min(y) - 1, cc + half_w, max(y) + 1,
       col = adjustcolor("steelblue", alpha.f = 0.25), border = NA)
  done_t <- centers[centers <= cc]
  lines(done_t, sapply(done_t, roll_mean_at), col = "firebrick", lwd = 2)
  abline(v = cc, col = "steelblue", lty = 2)

  # --- right: raster with sliding window, output filling in as it goes ---
  plot(NULL, xlim = c(0.5, nc + 0.5), ylim = c(0.5, nr + 0.5),
       xlab = "column", ylab = "row", main = "Moving window over a raster",
       axes = FALSE)
  axis(1, at = 1:nc); axis(2, at = 1:nr, labels = nr:1)
  for (i in 1:nr) {
    for (j in 1:nc) {
      yb <- nr - i + 1
      if (visit_at[i, j] <= k) {
        col <- done_col(omat[i, j]); lbl <- round(omat[i, j], 1)
      } else {
        col <- raw_col(rmat[i, j]); lbl <- round(rmat[i, j], 1)
      }
      rect(j - 0.5, yb - 0.5, j + 0.5, yb + 0.5, col = col, border = "grey40")
      text(j, yb, lbl, cex = 0.65,
           col = if (visit_at[i, j] <= k) "grey15" else "white", font = if (visit_at[i,j] <= k) 2 else 1)
    }
  }
  rect(pj - 1.5, (nr - pi + 1) - 0.5, pj + 1.5, (nr - pi + 1) + 0.5,
       border = "firebrick", lwd = 3)
  legend("bottomleft", inset = -0.02, xpd = NA, bty = "n", cex = 0.65,
         legend = c("raw", "smoothed (done)"), fill = c(raw_col(mean(raw_rng)), done_col(mean(out_rng))))

  mtext("Same mechanism, different neighborhood: a local window slides and summarizes",
        outer = TRUE, cex = 0.9, font = 2)
  dev.off()
  frame_files[k] <- f
}

imgs <- image_read(frame_files)
anim <- image_animate(imgs, fps = 10, loop = 0)
image_write(anim, "moving_window_parallel.gif")
cat("wrote gif, size:", file.info("moving_window_parallel.gif")$size, "bytes\n")
