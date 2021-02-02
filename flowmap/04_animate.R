library(tidyverse)
library(magick)

# fd
imgs <- list.files("output/fd/200dpi", full.names = T) %>%
  sort(decreasing = T)

img_list <- lapply(c(imgs[1:10],imgs[9:2]), image_read)

## join the images together
img_joined <- image_join(img_list)

## animate
img_animated <- image_animate(img_joined, fps = 10, optimize = F)

## save to disk
image_write(image = img_animated,
            path = "output/post/fd-animate-flight-200.gif")

# he
# old <- list.files("output/he/200dpi", full.names = T)
# new <- old %>%
#   gsub("bw1_", "bw1.0_", .) %>%
#   gsub("bw0_", "bw0.0_", .) %>%
#   gsub("decay1_", "decay1.0_", .) %>%
#   gsub("decay0_", "decay0.0_", .)
# new
# 
# file.rename(old, new)

imgs <- list.files("output/he/200dpi", full.names = T)

img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate
img_animated <- image_animate(img_joined, fps = 10, optimize = F)

## save to disk
image_write(image = img_animated,
            path = "output/post/he-animate-flight-200-10fps.gif")
