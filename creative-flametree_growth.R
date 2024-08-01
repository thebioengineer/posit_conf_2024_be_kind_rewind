
library(flametree)
library(tidyverse)
library(camcorder)

set.seed(as.Date("2024/08/14"))

## Code ideas from Danielle Navarro
## https://blog.djnavarro.net/posts/2021-10-19_rtistry-posts/

# pick some colours
shades <- c("#1b2e3c", "#0c0c1e", "#74112f", "#f3e3e2")


gg_record(
  dir = "flametree_growth",
  device = "png",
  width = 5,
  height = 5,
  units = "in",
  dpi = 500
)

ntrees <- 10
growth_duration <- 10

seeds <- as.integer(as.numeric(as.Date("2024/08/14")) * runif(ntrees))
growth_offsets <- c(0,1,0,2,1,0,1,1,0,2)
growth_start_time <- cumsum(growth_offsets)

flame_trees_data <- lapply(1:ntrees,function(ntree){

  tree_time_delay <- growth_start_time[ntree]

  # data structure defining the tree. Seed is set, so should grow
  flametree_grow(
    seed = seeds[[ntree]],
    time = growth_duration,
    trees = 1,
    ) |>
    mutate(
      id_time = id_time + tree_time_delay,
      id_tree = ntree,
      id_pathtree = paste0(id_tree,"_", id_path)
    )
  }) |>
  bind_rows()


range_x <- range(flame_trees_data$coord_x)
range_y <- range(flame_trees_data$coord_y)

for(time_idx in 2:max(flame_trees_data$id_time)){

  dat <- flame_trees_data |>
    filter(id_time <= (time_idx + 1)) |>
    group_split(id_tree) |>
    keep(~max(.x$id_time) > 2) |>
    bind_rows()


  gg_flametree <- dat %>%
    flametree_plot(
      background = "antiquewhite",
      palette = shades,
      style = "nativeflora"
    ) +
    xlim(range_x[1]-.25, range_x[2]+.25) +
    ylim(range_y[1]-.25, range_y[2]+.25)


  print(gg_flametree)

}

gg_playback(
  "flametree_growth.gif",
  first_image_duration = 1,
  last_image_duration = 30,
  frame_duration = .2,
  last_as_first = FALSE
)


