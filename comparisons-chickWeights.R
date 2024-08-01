
library(tidyverse)
library(camcorder)

gg_record(
  dir = "chickweight_comparisons",
  device = "png",
  width = 5,
  height = 5,
  units = "in",
  dpi = 300
)


for(chick_diet_idx in levels(ChickWeight$Diet)){

  gg_chickweights <- ggplot(ChickWeight) +
    aes(
      group = Chick,
      x = Time,
      y = weight,
      color = ifelse(Diet == chick_diet_idx,"blue","grey10"),
      alpha = ifelse(Diet == chick_diet_idx,.9,.5)
    ) +
    geom_path() +
    geom_point() +
    theme_classic() +
    scale_x_continuous(
      limits = c(0, 21)
    ) +
    scale_y_continuous(
      limits = c(0, 400)
    ) +
    scale_color_identity(
      guide = FALSE
    ) +
    scale_alpha(
      guide = FALSE
    ) +
    labs(
      title = paste0("Chick Weights - Diet #",chick_diet_idx),
      subtitle = "Data Source: Crowder, M. and Hand, D. (1990), Analysis of Repeated Measures, \nChapman and Hall (example 5.3)",
      x = 'Time (Days)',
      y = 'Weight (gm)'
    ) +
    theme(
      legend.position = "bottom",
      title = element_text(size = 10),
      plot.subtitle = element_text(size = 7),
      axis.title = element_text(size = 8),
      legend.title = element_text(size = 8)
    )

  print(gg_chickweights)

}

gg_playback(
  "chickweight_diets_compared.gif",
  first_image_duration = 1,
  last_image_duration = 1,
  frame_duration = 1,
  last_as_first = FALSE
)


