func_map_static <- function(input_data, state_data, input_metric, map_psize, map_tsize) {
  
  ggplot(data = input_data) +
    geom_sf(data = state_data, fill = "white") +
    geom_point(aes(x = lon, y = lat, size = value, color = value),
               alpha = 0.5) + # skyblue
    geom_text(aes(x = lon, y = lat, label = msa),
              position = position_nudge(x = -5000*map_psize, y = 7000*map_psize),
              size.unit = "pt", size = map_tsize*0.9) +
    theme_void() +
    guides(color = guide_legend(title = input_metric),
           size = guide_legend(title = input_metric)) +
    theme(legend.position = "bottom",
          text = element_text(size = map_tsize)) +
    scale_size_area(max_size = map_psize)
  
}

func_map_animate <- function(input_data, state_data, input_metric, map_psize, map_tsize){
  
  plot_map <- ggplot(data = input_data) +
    geom_sf(data = state_data, fill = "white") +
    geom_point(aes(x = lon, y = lat, size = value, color = value),
               alpha = 0.5) +
    geom_text(aes(x = lon, y = lat, label = msa),
              position = position_nudge(x = -5000*map_psize, y = 7000*map_psize),
              size.unit = "pt", size = map_tsize*0.9) +
    theme_void() +
    guides(color = guide_legend(title = input_metric),
           size = guide_legend(title = input_metric)) +
    labs(title = "Year: {round(frame_time,0)}") +
    theme(legend.position = "bottom",
          text = element_text(size = map_tsize)) +
    scale_size_area(max_size = map_psize) +
    transition_time(year)
  
  anim_save("outfile.gif",
            gganimate::animate(plot_map, renderer = gifski_renderer(), nframes = 20,
                               height = 700, width = 1000))
  
  # Return a list containing the filename
  list(src = "outfile.gif", contentType = "image/gif")
  
}
