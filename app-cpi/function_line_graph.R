func_secondary_axis <- function(primary, secondary) {
  
  from <- range(secondary, na.rm = TRUE)
  to   <- range(primary, na.rm = TRUE)
  
  # Forward transform for the data
  forward <- function(x) {
    scales::rescale(x, from = from, to = to)
  }
  
  # Reverse transform for the secondary axis
  reverse <- function(x) {
    scales::rescale(x, from = to, to = from)
  }
  
  list(fwd = forward, rev = reverse)
  
}




func_single_graph = function(input_dataset, pick_metric_1){
  
  single_line_graph = input_dataset |>
    ggplot2::ggplot(aes(x = year, y = value)) +
    #axis for metric 1
    ggplot2::geom_line(aes(color = msa_label_short)) +
    ggplot2::labs(x = 'Year', 
                  y = pick_metric_1) +
    ggplot2::theme_minimal()
  
  return(single_line_graph)
  
}




func_ratio_graph = function(input_dataset, pick_metric_1, pick_metric_2){ 
  
  pick_ratio = paste(pick_metric_1, pick_metric_2, sep = '_')
  
  fred_filter_msa_base = input_dataset |>
    dplyr::filter(metric %in% c(pick_metric_1, pick_metric_2)) |>
    dplyr::mutate(
      metric = dplyr::case_when(
        metric == pick_metric_1 ~ 'metric_1',
        metric == pick_metric_2 ~ 'metric_2')) |>
    tidyr::pivot_wider(id_cols = c(year, msa), names_from = metric, values_from = value) 
  
  secondary_axis = with(fred_filter_msa_base, func_secondary_axis(metric_1, metric_2))
  
  min_year = min(fred_filter_msa_base$year)
  max_year = max(fred_filter_msa_base$year)
  
  # top facet, both metrics plotted at same time 
  p1 = fred_filter_msa_base |>
    ggplot2::ggplot(aes(x = year)) +
    #axis for metric 1
    ggplot2::geom_line(aes(y = metric_1, 
                           color = msa), linetype = 'solid') +
    #axis for metric 2
    ggplot2::geom_line(aes(y = secondary_axis$fwd(metric_2), 
                           color = msa), linetype = 'dotted') +
    ggplot2::xlim(c(min_year, max_year)) +
    ggplot2::scale_color_manual(values = c(colors[2], colors[3])) +
    ggplot2::scale_y_continuous(sec.axis = sec_axis(~secondary_axis$rev(.), name = pick_metric_2)) +
    ggplot2::labs(x = element_blank(),
                  y = pick_metric_1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = element_blank(), 
                   legend.position = c(0.1, 0.85))
  
  #bottom facet, ratio plotted 
  p2 = input_dataset |>
    dplyr::filter(metric == pick_ratio) |>
    ggplot2::ggplot(aes(x = year, 
                        y = value)) + 
    ggplot2::geom_line(aes(color = msa)) +
    ggplot2::xlim(c(min_year, max_year)) +
    ggplot2::labs(x = "Year", 
                  y = paste0("Ratio of ", pick_metric_1, " to ", pick_metric_2, sep = ''), 
                  color = "Metric") + 
    ggplot2::theme_minimal() 
  
  #stack plots together 
  ratio_line_graph = p1 + p2 + plot_layout(ncol = 1, nrow = 2)
  
  return(ratio_line_graph)
  
}




wrapper_graph_function = function(input_dataset, pick_msa, pick_metric_1, pick_metric_2 = NULL){
  
  if(pick_metric_2 == 'None'){
    
    title_text = paste0(pick_metric_1, ' in ', pick_msa, ', ', min(input_dataset$year), '-', 
                        max(input_dataset$year))
    
    output_single_metric_graph = 
      func_single_graph(input_dataset = input_dataset,  
                        pick_metric_1 = pick_metric_1) +
      plot_annotation(title = title_text)
    
    return(output_single_metric_graph)
    
  }else{
    
    title_text = paste0(pick_metric_1, ' & ', pick_metric_2, ' in ', pick_msa, ', ', 
                        min(input_dataset$year), '-', max(input_dataset$year))
    
    output_double_metric_graph = 
      func_ratio_graph(input_dataset = input_dataset, pick_metric_1 = pick_metric_1, 
                       pick_metric_2 = pick_metric_2) +
      plot_annotation(title = title_text)
    
    return(output_double_metric_graph)
    
  }
  
}

fred_smetric = fred_smetric |> filter(msa == 'SFO')

#wrapper_graph_function(input_dataset = fred_smetric, pick_msa = c('SFO'), pick_metric_1 = 'personal_income', pick_metric_2 = 'cpi_all')
