library(tidyverse)
library(magick)

imgs <- list.files("output/fd/200dpi", full.names = T) %>%
  sort(decreasing = T)

img_list <- lapply(c(imgs[1:10],imgs[9:2]), image_read)

## join the images together
img_joined <- image_join(img_list)

## animate
img_animated <- image_animate(img_joined, fps = 5, optimize = F)

## save to disk
image_write(image = img_animated,
            path = "output/post/fd-animate-flight-200.gif")
