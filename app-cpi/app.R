## Libraries
# library(shiny)
library(bslib)
library(shinycssloaders)
library(tidyverse)
library(sf)
library(here)
library(ggh4x)
library(patchwork)
library(gganimate)
library(sfarrow)
library(gifski)
library(av)

## Setup
thematic::thematic_shiny()

colors <-  c('#A5CDF3', # Uranian blue
             "#ee964b", # sandy brown
             "#8fc93a", # yellow green
             '#188C88', # dark cyan 
             "#0d3b66", # yale blue
             '#BB6011', # Alloy orange
             '#395016', # Dark moss green 
             "#e1faf9") # light cyan 

data_file_path = here('app-cpi/app_data/')

fred_data = arrow::read_parquet(paste0(data_file_path, "/fred_data_clean.parquet"))

# Spatial: states
states_sf <- st_read_parquet(paste0(data_file_path, "/states_sf.parquet")) 

#source(here('app-cpi/function_line_graph.R'))
source(here('app-cpi/function_map.R'))

msa_prep = fred_data |>
  distinct(msa, msa_label_short, msa_label_long) |>
  arrange(msa_label_short)

metric_prep = fred_data |>
  filter(!is.na(metric_label_short)) |>
  distinct(metric, metric_label_short, metric_label_long) |>
  arrange(metric_label_short) 

metric_prep_2 = metric_prep |>
  rbind(c(msa = 'None', metric_label_short = 'None', metric_label_long = 'None'))

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

## App 
# UI #############################
ui <- bslib::page_navbar(
  title = "Fred Reserve Economic Data Dashboard",
  theme = bs_theme(bootswatch = "darkly"),
  # Tab 1: Across Time
  nav_panel(title = "Across Time", 
    layout_sidebar(
      sidebar = sidebar(
        strong("Filter Options"), 
        checkboxGroupInput(inputId = "yr_smsa", label = "City",
                           choiceValues = msa_prep$msa, 
                           choiceNames = msa_prep$msa_label_short,
                           selected = "SFO"),
        radioButtons(inputId = "yr_smetric1", label = "Metric 1",
                     choiceValues = metric_prep$metric,
                     choiceNames = metric_prep$metric_label_short,
                     selected = 'cpi_all'),
        radioButtons(inputId = "yr_smetric2", label = "Metric 2",
                        choiceValues = metric_prep_2$metric,
                        choiceNames = metric_prep_2$metric_label_short,
                        selected = 'None'),
        conditionalPanel(
          condition = "input.yr_smetric2 != 'None'", 
          checkboxInput(inputId = "toggle_ratio", label = "Plot Ratios?",
                        value = FALSE)),
        strong("Plotting Options"),
        sliderInput(inputId = "yr_tsize", label = "Text Size",
                    min = 12, max = 24, value = 20)),
      card(plotOutput("plot_yr") %>% withSpinner()))
    ),
  # Tab 2: Geographic Map
  nav_panel(title = "Geographic Map", 
            layout_sidebar(
              sidebar = sidebar(
                checkboxInput(inputId = "map_sanim", label = "ANIMATE!",
                              value = FALSE),
                strong("Filter Options"), 
                radioButtons(inputId = "map_smetric", label = "Metric",
                             choices = c("CPI", "Income"), selected = "CPI"),
                # Change year slider depending on animated plot setting
                conditionalPanel(
                  condition = "!input.map_sanim", 
                  sliderInput(inputId = "map_syear", label = "Year",
                              min = 1914, max = 2024, value = 2000)),
                conditionalPanel(
                  condition = "input.map_sanim",  
                  sliderInput(inputId = "map_syear_rng", label = "Year",
                              min = 1914, max = 2024, value = c(1980, 2000))),
                strong("Plotting Options"),
                sliderInput(inputId = "map_tsize", label = "Text Size",
                            min = 12, max = 24, value = 20),
                sliderInput(inputId = "map_psize", label = "Point Size",
                          min = 20, max = 40, value = 30)),
                card(
                  conditionalPanel(
                    condition = "!input.map_sanim",
                    plotOutput("plot_map_static") %>% withSpinner()),
                  conditionalPanel(
                    condition = "input.map_sanim",
                    imageOutput("plot_map_anim") %>% withSpinner())
                ))
  ),
)

# Server #############################
server <- function(input, output, session) {
  
  # Clean Data
  clean_yr <- reactive({
    
    fred_msa <- fred_data |> 
      dplyr::filter(msa %in% input$yr_smsa) 
    
    fred_smetric <- fred_msa |> 
      dplyr::filter(metric %in% 
                      c(input$yr_smetric1, input$yr_smetric2, 
                        str_c(input$yr_smetric1, '_', input$yr_smetric2)))
    
    # Return
    fred_smetric = fred_smetric
    
  })
  
  ratio_on_off <- reactive({
    
    if(exists('toggle_ratio')){
      
      ratio_on_off_flag = input$toggle_ratio
      
    }else{
      
      ratio_on_off_flag = FALSE
      
    }
    
    ratio_on_off_flag = ratio_on_off_flag
    
  })
  
  
  clean_map <- reactive({
    
    fred_smetric <- fred_data |>
      dplyr::filter(metric == input$map_smetric)
    
    fred_syear <- fred_smetric |> 
      dplyr::filter(year == input$map_syear)
    
    fred_syear_rng <- fred_smetric |> 
      dplyr::filter(year >= input$map_syear_rng[1] & year <= input$map_syear_rng[2])
    
    # Return
    list(fred_smetric = fred_smetric,
         fred_syear = fred_syear,
         fred_syear_rng = fred_syear_rng) 
    
  })
  
  
  # Update filter
  observe({
    
    fred_yr <- clean_map() |>
      purrr::pluck("fred_smetric", "year")
    
    min_yr <- fred_yr |> min()
    
    max_yr <- fred_yr |> max()
    
    updateSliderInput(session, inputId = "map_syear",
                      min = min_yr, max = max_yr)
    
    updateSliderInput(session, inputId = "map_syear_rng",
                      min = min_yr, max = max_yr)
    
  })
  
  
  # Create plots
  output$plot_yr <- renderPlot({
    
    fred_smetric <- clean_yr() 
    
    ratio_on_off_flag <- ratio_on_off()
  
    if(input$yr_smetric2 == 'None'){
      
      metric_label_1 = metric_prep$metric_label_short[metric_prep$metric == input$yr_smetric1]
      msa_label = msa_prep$msa_label_short[msa_prep$msa %in% input$yr_smsa]
        
        title_text = paste0(metric_label_1, ' in ', paste0(msa_label, collapse = ' and '), ', ', 
                            min(fred_smetric$year), '-', max(fred_smetric$year))
        
        plot = fred_smetric |>
          ggplot2::ggplot(aes(x = year, y = value)) +
          #axis for metric 1
          ggplot2::geom_line(aes(color = msa_label_short), linewidth = 1) +
          ggplot2::labs(x = 'Year', 
                        y = metric_label_1) +
          ggplot2::theme_minimal() +
          ggplot2::scale_color_manual(values = colors) +
          ggplot2::guides(color = guide_legend(title = "City")) +
          ggplot2::theme(legend.title = element_text('hjust', size = 20, color = 'white'),
                         legend.text = element_text(size = 20, color = 'white'),
                         axis.text = element_text(size = 20, color = 'white'),
                         axis.title = element_text(size = 20, color = 'white')) +
          plot_annotation(title = title_text,
                          theme = theme(plot.title = element_text(size = 20, color = 'white')))
        
      }else{
        
        metric_label_1 = metric_prep$metric_label_short[metric_prep$metric == input$yr_smetric1]
        metric_label_2 = metric_prep$metric_label_short[metric_prep$metric == input$yr_smetric2]
        msa_label = msa_prep$msa_label_short[msa_prep$msa %in% input$yr_smsa]
        
        title_text = paste0(metric_label_1, ' & ', 
                            metric_label_2, ' in ', 
                            paste0(msa_label, collapse=' and '), 
                            ', ', min(fred_smetric$year), '-', max(fred_smetric$year))
        
        pick_ratio = paste(input$yr_smetric1, input$yr_smetric2, sep = '_')
        
        fred_filter_msa_base = fred_smetric |>
          dplyr::filter(metric %in% c(input$yr_smetric1, input$yr_smetric2)) |>
          dplyr::mutate(
            metric = dplyr::case_when(
              metric == input$yr_smetric1 ~ 'metric_1',
              metric == input$yr_smetric2 ~ 'metric_2')) |>
          tidyr::pivot_wider(id_cols = c(year, msa, msa_label_short), names_from = metric, values_from = value) 
        
        secondary_axis = with(fred_filter_msa_base, func_secondary_axis(metric_1, metric_2))
        
        min_year = min(fred_filter_msa_base$year)
        max_year = max(fred_filter_msa_base$year)
        
        # top facet, both metrics plotted at same time 
        p1 = fred_filter_msa_base |>
          ggplot2::ggplot(aes(x = year)) +
          #axis for metric 1
          ggplot2::geom_line(aes(y = metric_1, 
                                 color = msa_label_short), linetype = 'solid', linewidth = 1) +
          #axis for metric 2
          ggplot2::geom_line(aes(y = secondary_axis$fwd(metric_2), 
                                 color = msa_label_short), linetype = 'longdash', linewidth = 1) +
          ggplot2::xlim(c(min_year, max_year)) +
          ggplot2::scale_color_manual(values = colors) +
          ggplot2::scale_y_continuous(sec.axis = sec_axis(~secondary_axis$rev(.), name = metric_label_2)) +
          ggplot2::labs(x = element_blank(),
                        y = metric_label_1) +
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.title = element_text('hjust', size = 20, color = 'white'),
                         legend.text = element_text(size = 20, color = 'white'),
                         axis.text = element_text(size = 20, color = 'white'),
                         axis.title = element_text(size = 20, color = 'white'))
        
        if(ratio_on_off_flag == TRUE) {
        
          #bottom facet, ratio plotted 
          p2 = fred_smetric |>
            dplyr::filter(metric == pick_ratio) |>
            ggplot2::ggplot(aes(x = year, 
                                y = value)) + 
            ggplot2::geom_line(aes(color = msa_label_short), linewidth = 1) +
            ggplot2::xlim(c(min_year, max_year)) +
            ggplot2::labs(x = "Year", 
                          y = paste0("Ratio of ", metric_label_1, " to ", metric_label_2, sep = ''), 
                          color = "msa_label_short") + 
            ggplot2::theme_minimal() +
            ggplot2::scale_color_manual(values = colors) +
            ggplot2::theme(legend.title = element_text('hjust', size = 20, color = 'white'),
                           legend.text = element_text(size = 20, color = 'white'),
                           axis.text = element_text(size = 20, color = 'white'),
                           axis.title = element_text(size = 20, color = 'white'))
          
          #stack plots together 
          ratio_line_graph = p1 + p2 + plot_layout(ncol = 1, nrow = 2)
          
        }else{
          
          ratio_line_graph = p1
          
        }
        
        plot = ratio_line_graph + plot_annotation(title = title_text,
                        theme = theme(plot.title = element_text(size = 20, color = 'white')))
        
      }
    
    
    plot
    
  }, height = 700, width = 1000)
  
  
  output$plot_map_static <- renderPlot({
    
    if(!input$map_sanim){
      
      fred_year = clean_map() |> purrr::pluck("fred_syear")
      
      func_map_static(input_data = fred_year, state_data = states_sf,
                      input_metric = input$smetric,map_psize = input$map_psize, 
                      map_tsize = input$map_tsize)
      
    } 
  }, 
  height = 700, width = 1000)
  
  output$plot_map_anim <- renderImage({
    
    if (input$map_sanim){
      
      fred_syear_rng <- clean_map() |> purrr::pluck("fred_syear_rng")
    
      func_map_animate(input_data = fred_syear_rng, state_data = states_sf, 
                       input_metric = input$smetric, map_psize = input$map_psize, 
                       map_tsize = input$map_tsize)
      
    }
      
  }, 
  deleteFile = TRUE)
  
}


## RUN APP #############################
shinyApp(ui = ui, server = server)
