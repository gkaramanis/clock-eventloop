library(eventloop)

clock <- function(...){
  
  # set radius for clock
  r <- 1
  
  # get time
  time <- Sys.time()
  
  # extract seconds, minutes, hour
  s <- as.numeric(format(as.POSIXct(time, format = "%H:%M:%S"), "%S"))
  m <- as.numeric(format(as.POSIXct(time, format = "%H:%M:%S"), "%M")) + s / 60
  h <- as.numeric(format(as.POSIXct(time, format = "%H:%M:%S"), "%H")) %% 12 + m / 60
  
  # calculate angle, x and y for seconds
  sa <- s * 6 * pi / 180 + pi/2
  sx <- -0.9 * r * cos(sa)
  sy <- 0.9 * r * sin(sa)
  
  # calculate angle, x and y for minutes
  ma <- m * 6 * pi / 180 + pi/2
  mx <- -0.85 * r * cos(ma)
  my <- 0.85 * r * sin(ma)
  
  # calculate angle, x and y for seconds
  ha <- h * 30 * pi / 180 + pi/2
  hx <- -0.65 * r * cos(ha)
  hy <- 0.65 * r * sin(ha)
  
  # make new "empty" plot
  plot(0, 0, xlim = c(-1, 1), ylim = c(-1, 1), xaxt = "n", yaxt = "n", ann = FALSE, asp = 1, bty = "n", pch = 21, col = "black", bg = "black", cex = 1.25)
  
  # draw outline of clock
  t = seq(0, 2 * pi, length = 100) 
  lines(x = r * cos(t), y = r * sin(t), lwd = 4)
  
  # draw hours
  hours <- data.frame(a = seq(pi/2 + pi/6, 2 * pi + pi/2, length.out = 12))
  hours$label = 1:12
  hours$x = -0.9 * r * cos(hours$a)
  hours$y = 0.9 * r * sin(hours$a)
  
  text(hours$x, hours$y, cex = 2)
  
  # draw minute marks
  minutes <- data.frame(a = seq(pi/2 + pi/30, 2 * pi + pi/2, length.out = 60))
  minutes$label = 1:60
  minutes$size = ifelse(1:60 %% 5 == 0, 1.2, 0.5)
  minutes$x = -r * cos(minutes$a)
  minutes$y = r * sin(minutes$a)
  
  points(x = 0.75 * minutes$x, y = 0.75 * minutes$y, bg = "black", pch = 21, cex = minutes$size)
  
  # draw hands
  segments(x0 = 0, y0 = 0, x1 = hx, y1 = hy, lwd = 4)
  segments(x0 = 0, y0 = 0, x1 = mx, y1 = my, lwd = 4)
  segments(x0 = -0.15 * sx, y0 = -0.15 * sy, x1 = sx, y1 = sy, col = "coral2", lwd = 2)
  }

# start event loop
eventloop::run_loop(clock)

