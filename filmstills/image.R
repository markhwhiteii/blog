# prep -------------------------------------------------------------------------
library(imager)
library(tidyverse)

norm <- function(x) (x - min(x)) / (max(x) - min(x))

shuffle <- function(x) x[sample(seq_along(x), length(x))]

get_palette <- function(filename, k, mdn = FALSE) {
  
  dat_pal <- load.image(filename) %>% 
    as.data.frame(wide = "c")
  
  res_pal <- dat_pal %>% 
    select(starts_with("c")) %>% 
    kmeans(k, algorithm = "Lloyd", iter.max = 500)
  
  if (!mdn) {
    pal <- res_pal$centers %>% 
      as_tibble() %>% 
      mutate(hex = rgb(c.1, c.2, c.3)) %>% 
      pull(hex)
  } else if (mdn) {
    pal <- dat_pal %>% 
      mutate(cluster = res_pal$cluster) %>% 
      group_by(cluster) %>% 
      summarise(across(starts_with("c"), median)) %>% 
      mutate(hex = rgb(c.1, c.2, c.3)) %>% 
      pull(hex)
  }
  
  return(pal)
}

make_plot <- function(filename_in, pal, xy = TRUE) {
  
  the_shot <- load.image(filename_in)
  
  dat_shot <- the_shot %>% 
    as.data.frame(wide = "c")
  
  dat_shot_norm <- dat_shot %>% 
    when(!xy ~ select(., starts_with("c")), ~ .) %>% 
    mutate(across(everything(), norm))
  
  res_shot <- kmeans(
    dat_shot_norm, 
    length(pal), 
    algorithm = "Lloyd", 
    iter.max = 500
  )
  
  dat_shot$clust <- factor(res_shot$cluster)
  
  p <- ggplot(dat_shot, aes(x = x, y = y)) +
    geom_raster(aes(fill = clust)) +
    scale_y_reverse() +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_manual(values = pal)
  
  return(list(plot = p, dims = dim(the_shot)[1:2]))
}

# umbrellas of chungking -------------------------------------------------------
set.seed(1839)

pal1 <- get_palette("umbrellas.jpeg", 12)
plot1 <- make_plot("chungking.jpeg", pal1)

ggsave(
  "chungking_k12.png", 
  plot1$plot, 
  width = plot1$dims[1], 
  height = plot1$dims[2],
  units = "px"
)

# her runner -------------------------------------------------------------------
set.seed(1839)

pal2 <- get_palette("her.jpeg", 3, TRUE)
plot2 <- make_plot("bladerunner.jpeg", shuffle(pal2))

plot2$plot

ggsave(
  "bladerunner_k3.png", 
  plot2$plot, 
  width = plot2$dims[1],
  height = plot2$dims[2],
  units = "px"
)

# 2001 arrivals ----------------------------------------------------------------
set.seed(1839)

pal3 <- get_palette("2001.jpeg", 5, TRUE)
plot3 <- make_plot("arrival.jpeg", shuffle(pal3), xy = FALSE)

plot3$plot

ggsave(
  "arrival_k5.png", 
  plot3$plot, 
  width = plot3$dims[1],
  height = plot3$dims[2],
  units = "px"
)

# lion fargo -------------------------------------------------------------------
set.seed(1839)

pal4 <- get_palette("lionking.jpeg", 2, TRUE)
plot4 <- make_plot("fargo.jpeg", rev(pal4), xy = FALSE)

plot4$plot

ggsave(
  "fargo_k2.png", 
  plot4$plot, 
  width = plot4$dims[1],
  height = plot4$dims[2],
  units = "px"
)

# kill strangelove -------------------------------------------------------------
set.seed(1839)

pal5 <- get_palette("killbill.jpeg", 2, TRUE)
plot5 <- make_plot("strangelove.jpeg", pal5, xy = FALSE)

plot5$plot

ggsave(
  "strangelove_k2.png", 
  plot5$plot, 
  width = plot5$dims[1],
  height = plot5$dims[2],
  units = "px"
)
