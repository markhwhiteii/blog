library(imager)
library(tidyverse)

plot_point = function(img, n, ...) {
  ggplot(slice_sample(img, n = n), aes(x, y)) + 
    geom_point(aes(color = hex), ...) +
    scale_color_identity() +
    scale_y_reverse() +
    theme_void()
}

img <- load.image("20221112_DSC_0068_TP.JPG") # load image in

dims <- dim(img)[1:2] # get dimensions for exporting

# change to data frame
img_dat <- img %>% 
  as.data.frame(wide = "c") %>% 
  mutate(hex = rgb(c.1, c.2, c.3), xy = paste(x, y, sep = "_"))

# make up an "algorithm", do it like 100 times
set.seed(1839)
for (i in seq_len(100)) {
  cat("starting", i, "\n")
  
  # jumble with probability
  p_jumble <- runif(1, .25, .75)
  
  # figure out which points to jumble
  to_jumble <- as.logical(rbinom(nrow(img_dat), 1, p_jumble))
  
  # make a jumbled order, brb
  jumbled <- order(runif(sum(to_jumble)))
  
  # add some error to each color column
  # then turn to hex value
  c_err <- rnorm(3, 0, .1)
  img_dat_edit <- img_dat %>% 
    mutate(
      # need to make between 0 and 1
      c.1 = c.1 + c_err[1], 
      c.1 = ifelse(c.1 > 1, 1, c.1),
      c.1 = ifelse(c.1 < 0, 0, c.1),
      c.2 = c.2 + c_err[2], 
      c.2 = ifelse(c.2 > 1, 1, c.2),
      c.2 = ifelse(c.2 < 0, 0, c.2),
      c.3 = c.3 + c_err[3],
      c.3 = ifelse(c.3 > 1, 1, c.3),
      c.3 = ifelse(c.3 < 0, 0, c.3),
      hex = rgb(c.1, c.2, c.3)
    )
  
  # then use jumble to jumble the colors
  img_dat_edit$hex[to_jumble] <- img_dat_edit$hex[jumbled]
  
  # select n random pixels of random size
  n <- round(runif(1, 1000, 1000000))
  size = round(runif(1, 5, 30))
  
  # plot and save
  p <- plot_point(img_dat_edit, n, shape = "square", size = size)
  ggsave(
    paste0("plaza/plaza_iter_", i, ".png"),
    p,
    width = dims[1], 
    height = dims[2], 
    units = "px"
  )
}

# make gif, write out
img_anim <- list.files("plaza/", full.names = TRUE) %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_animate(fps = 10)

image_write(img_anim, "plaza.gif", quality = 1)
