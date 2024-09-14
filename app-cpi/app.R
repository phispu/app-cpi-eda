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

colors <-  c("#0d3b66", # yale blue
             "#ee964b", # sandy brown
             "#8fc93a", # yellow green
             "#252b2d", # gunmetal
             "#e1faf9") # light cyan 

data_file_path = here('app-cpi/app_data/')

fred_data = arrow::read_parquet(paste0(data_file_path, "/fred_data_clean.parquet"))

# Spatial: states
states_sf <- st_read_parquet(paste0(data_file_path, "/states_sf.parquet")) 

source(here('app-cpi/function_line_graph.R'))
source(here('app-cpi/function_map.R'))


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
        checkboxGroupInput(inputId = "yr_smsa", label = "MSA",
                           choices = c("SFO", "NEW"), selected = "SFO"),
        radioButtons(inputId = "yr_smetric1", label = "Metric 1",
                     choices = c("cpi_all", "personal_income"), selected = "cpi_all"),
        radioButtons(inputId = "yr_smetric2", label = "Metric 2",
                     choices = c("None", "cpi_all", "personal_income"), selected = "None"),
        conditionalPanel(
          condition = "input.yr_smetric2 != 'None'", 
          checkboxInput(inputId = "ratio", label = "Plot Ratios?",
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
    
    plot = wrapper_graph_function(
      input_dataset = fred_smetric, pick_msa = input$yr_smsa, pick_metric_1 = input$yr_smetric1, 
      pick_metric_2 = input$yr_smetric2)
    
    plot
#jenny keeping these in as comments in case we want to use some of this info in updating the plot function 
    # plot_yr1 <- fred_yr |> 
    #   ggplot(aes(x = year, color = msa)) +
    #   geom_line(aes(y = value)) +
    #   # Add manual colors?
    #   labs(x = 'Year', 
    #        y = input$yr_smetric1,
    #        color = "MSA") +
    #   theme_minimal() +
    #   theme(legend.position = "bottom",
    #         text = element_text(size = input$yr_tsize))
    # 
    # if(input$yr_smetric2 == "None"){
    #   plot_yr1 <- plot_yr1
    # } else {
    #   sec <- ggh4x::help_secondary(fred_yr, 
    #                                primary = value, secondary = value2, method = "fit",
    #                                name = input$yr_smetric2)
    #   plot_yr1 <- plot_yr1 +
    #     geom_line(aes(y = sec$proj(value2)), linetype = "dotted") +
    #     scale_y_continuous(sec.axis = sec)
    # }
    # 
    # if(input$ratio != TRUE){
    #   plot_yr1
    # } else {
    #   plot_yr1 <- plot_yr1 + guides(color = "none")
    #   plot_yr2 <- fred_ratio |>
    #     ggplot(aes(x = year, color = msa)) +
    #     geom_line(aes(y = value)) + 
    #     labs(x = 'Year',
    #          y = str_c(input$yr_smetric1, "/", input$yr_smetric2),
    #          color = "MSA") +
    #     theme_minimal() +
    #     theme(legend.position = "bottom",
    #           text = element_text(size = input$yr_tsize))
    #   plot_yr1 / plot_yr2
    # }
    
    
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
