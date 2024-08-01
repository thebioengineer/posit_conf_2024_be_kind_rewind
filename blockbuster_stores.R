library(tidyverse)
library(rvest)
library(camcorder)
library(usmap)
library(patchwork)

# Based on
# https://www.reddit.com/r/dataisbeautiful/comments/hdrnfw/oc_blockbuster_video_us_store_locations_between/
#

blockbuster_10k <- tribble(
  ~Year, ~SEC_URL,
  "1999","https://www.sec.gov/Archives/edgar/data/1085734/000093066100000673/0000930661-00-000673.txt",
  "2000","https://www.sec.gov/Archives/edgar/data/1085734/000093066101000794/0000930661-01-000794.txt",
  "2001","https://www.sec.gov/Archives/edgar/data/1085734/000093066102000951/d10k.txt",
  "2002","https://www.sec.gov/Archives/edgar/data/1085734/000093066103001225/d10k.htm",
  "2003","https://www.sec.gov/Archives/edgar/data/1085734/000119312504041361/d10k.htm",
  "2004","https://www.sec.gov/Archives/edgar/data/1085734/000119312505063510/d10k.htm",
  "2005","https://www.sec.gov/Archives/edgar/data/1085734/000119312506055023/d10k.htm",
  "2006","https://www.sec.gov/Archives/edgar/data/1085734/000119312507044360/d10k.htm",
  "2008","https://www.sec.gov/Archives/edgar/data/1085734/000119312508048757/d10k.htm",
  "2009","https://www.sec.gov/Archives/edgar/data/1085734/000119312509073613/d10k.htm",
  "2010","https://www.sec.gov/Archives/edgar/data/1085734/000119312510058339/d10k.htm",
  "2011","https://www.sec.gov/Archives/edgar/data/1085734/000119312511186981/d10k.htm"
)

get_us_stores_txt <- function(path){

  temp_file <- tempfile(fileext = ".txt")

  utils::download.file(
    path,
    temp_file,
    headers = c("User-Agent"="ellishughes@live.com", Host= "www.sec.gov")
  )

  ten_k <- readLines(temp_file)

  tab_start <- grep("STATE OF TERRITORY",ten_k)[1]

  store_lines <- ten_k[(tab_start+2):(tab_start+55)] |>
    gsub("[.]+","", x=_) |>
    gsub("\\s+(\\d)","\\\t\\1", x=_)

  read.delim(text=paste0(store_lines,collapse = "\n"), sep = "\t",header = FALSE,dec = ",") |>
    setNames(c("state","stores"))

}

get_us_stores_htm <- function(path){

  temp_file <- tempfile(fileext = ".htm")

  utils::download.file(
    path,
    temp_file,
    headers = c("User-Agent"="ellishughes@live.com", Host= "www.sec.gov"),
  )

  ten_k <- read_html(temp_file)

  ten_k_tables <- html_table(ten_k)

  store_table <- ten_k_tables |>
    keep(~nrow(.x)>3) |>
    keep(~sum(grepl("STATE OR TERRITORY",.x,ignore.case = TRUE)) >0) |>
    bind_rows()

  if(nrow(store_table) > 0){
    store_table |>
      filter(!(toupper(.data[["X1"]]) %in% c("","STATE OR TERRITORY"))) |>
      select(
        state = X1,
        stores = last_col()
      ) |>
      mutate()
  }
}

blockbuster_10k_data <- blockbuster_10k |>
  mutate(
    n_stores = map(SEC_URL, \(x){
      ext <- tools::file_ext(x)
      get_us_stores <- switch(ext,
        "txt" = get_us_stores_txt,
        "htm" = get_us_stores_htm
      )
      get_us_stores(x)
    })
  )

blockbuster_10k_data_years <- blockbuster_10k_data |>
  select(Year, n_stores) |>
  mutate(
    n_stores = map(n_stores,\(x){
      if(is.character(x$stores)){
        x |>
          mutate(
            state = tolower(state),
            stores = readr::parse_number(stores)
            )
      }else{
        x
      }
    })
  ) |>
  unnest(n_stores,keep_empty = FALSE) |>
  mutate(Year = as.numeric(Year))

blockbuster_10k_data_years_wide <- blockbuster_10k_data_years |>
  filter(!grepl("totals", state, ignore.case = TRUE)) |>
  mutate(state = gsub("[.]","",toupper(state))) |>
  pivot_wider(
    names_from = Year,
    names_prefix = "Y",
    values_from = stores
  ) |>
  mutate(
    Y2002 = NA_integer_,
    Y2003 = NA_integer_,
    Y2007 = NA_integer_
  ) |>
  select(
    state, all_of(paste0("Y",as.character(1999:2010)))
  )

blockbuster_10k_data_years_imputed <- blockbuster_10k_data_years_wide |>
  pivot_longer(
    cols = starts_with("Y"),
    names_to = "Year",
    names_prefix = "Y",
    values_to = "stores"
  ) |>
  group_by(state,.drop = FALSE) |>
  group_map(\(x, ...){
    mod <- lm(stores ~ year_idx, data = x |> mutate(year_idx = row_number()))
    x$stores[is.na(x$stores)] <- floor(predict(mod, x |> mutate(year_idx = row_number())))[is.na(x$stores)]
    x
  }, .keep = TRUE) |>
  bind_rows()


## My data is prepared, now to record my plots!

camcorder::gg_record(
  dir = "blockbuster_stores_over_time",
  device = "png",
  width = 10,
  height = 7
)

## Make my plots

blockbuster_10k_data_year <- blockbuster_10k_data_years |>
  filter(Year == 1999)

state_labels <- blockbuster_10k_data_year |>
  mutate(
    lab = stores,
    state = tolower(state)
  ) |>
  rename(
    full = state
  ) |>
  left_join(
    usmapdata::centroid_labels("states") |>
      mutate(full = tolower(full)),
    by = "full"
  )


plot_usmap(data= blockbuster_10k_data_year, values = "stores") +
  labs(title = paste0(
    "Blockbuster Stores In Each State - ",1999,
    ", ",sum(blockbuster_10k_data_year$stores), " Total")
    ) +
  scale_fill_viridis_c(name = "Stores") +
  geom_sf_text(
    data = state_labels,
    aes(
      geometry = geom,
      label = lab
        ),
    color = "white"
    ) +
  theme(
    title = element_text(size = 20)
  )

camcorder::gg_resize_film(
  height = 15,
  width = 10
)







## I'm ready to stop recording for now!

camcorder::gg_stop_recording()


## Record a new set of plots to a new dir
if(dir.exists("blockbuster_stores_over_time_animation")){
  unlink("blockbuster_stores_over_time_animation",recursive = TRUE)
}


## Create a specific recording to a new location
camcorder::gg_record(
  dir = "blockbuster_stores_over_time_animation",
  device = "png",
  width = 13,
  height = 13
)

## for each year, make the same plot of number of stores
## and a line plot of all previous years store totals

for(year in unique(blockbuster_10k_data_years_imputed$Year)){

  blockbuster_10k_data_year <- blockbuster_10k_data_years_imputed |>
    filter(Year == year)

  state_labels <- blockbuster_10k_data_year |>
    mutate(
      lab = stores,
      state = tolower(state)
    ) |>
    rename(
      full = state
    ) |>
    left_join(
      usmapdata::centroid_labels("states") |>
        mutate(full = tolower(full)),
      by = "full"
    )


  blockbuster_plot <- plot_usmap(data= blockbuster_10k_data_year, values = "stores") +
    labs(title = paste0(
      "Blockbuster Stores In Each State - ",year,
      ", ",sum(blockbuster_10k_data_year$stores), " Total")
    ) +
    scale_fill_viridis_c(name = "Stores",limits = c(1, 681)) +
    geom_sf_text(
      data = state_labels,
      aes(
        geometry = geom,
        label = lab
      ),
      color = "white"
    ) +
    theme(
      title = element_text(size = 20)
    )

  store_lineplot <- blockbuster_10k_data_years_impute |>
    filter(as.numeric(Year) <= year) |>
    group_by(Year) |>
    summarize(stores = sum(stores)) |>
    ggplot(data = _,  aes(x = as.numeric(Year), y = stores)) +
    geom_path() +
    geom_point() +
    scale_x_continuous(
      name = "Year",
      limits = c(1998, 2011),
      # breaks = c(1999, 2001, 2003, 2005, 2007, 2009),
    ) +
    scale_y_continuous(
      name = "Stores",
      limits = c(4000,6000)
    ) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 15)
    )

  bb_plots <- blockbuster_plot/store_lineplot +
    patchwork::plot_layout(heights = c(5,2))

  print(bb_plots)
}

## Stop recording and make the gif
camcorder::gg_playback(
  name = "Blockbuster_Stores_1999-2010.gif",
  frame_duration = 1,
  first_image_duration = 3,
  last_image_duration = 10,
  last_as_first = FALSE,
  background = "white",
  )







