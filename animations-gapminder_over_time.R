
library(tidyverse)
library(camcorder)
library(gapminder)

gg_record(
  dir = "gapminder_over_time",
  device = "png",
  width = 15,
  height = 8,
  units = "in",
  dpi = 200
)

for(plot_year in sort(unique(gapminder$year))){

  gapminder_year <- gapminder |>
    filter(year == plot_year)

  gg_gapminder <- ggplot(gapminder_year) +
    geom_point(
      aes(
        x = gdpPercap,
        y = lifeExp,
        size = pop,
        color = continent
        ),
      alpha = .5
    ) +
    scale_x_log10(
      limits = c(250, 11500)
    ) +
    scale_y_continuous(
      limits = c(20, 90)
    ) +
    scale_size(
      range = c(1,20),
      guide = "none"
    ) +
    labs(
      title = paste0("Gapminder - ",plot_year),
      x = 'GDP per capita',
      y = 'Life Expectancy'
    ) +
    guides(
      color = guide_legend(title = "Continent")
    ) +
    theme(
      legend.position = "bottom",
      title = element_text(size = 20),
      axis.title = element_text(size = 15),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 12)
    )

  print(gg_gapminder)

}

gg_playback(
  "gapminder_1952_2007_animated.gif",
  first_image_duration = 1,
  last_image_duration = 5,
  frame_duration = 1,
  image_resize = 1200,
  last_as_first = FALSE
)


