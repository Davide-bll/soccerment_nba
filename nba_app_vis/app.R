remove(list = ls())

# an example of data exploration:



require(shiny)
require(dplyr)
require(RColorBrewer)
require(fmsb)
require(scales)
require(tidyr)
library(bslib)

ovl_dist <- read.csv("ovrl_distances.csv", stringsAsFactors = FALSE) %>% as_tibble()
byrole_dist <-read.csv("by_roles_overall_distances.csv", stringsAsFactors = FALSE) %>% as_tibble()
scl_players <- read.csv("scaled_features_post_corr.csv", stringsAsFactors = FALSE) %>% as_tibble()
players <- read.csv("players_post_corr.csv", stringsAsFactors = FALSE) %>% as_tibble()

# drop players with mjore than 1 role (tmp)
players <- players %>% separate("Pos", c("Pos", "Pos2"), sep = "-", fill="right") %>% 
  select(-Pos2)

# attach roles to standardized data
scl_players <- scl_players <- scl_players %>% left_join(select(players, c("Pos", "Player", "ID")), by = "ID")

# order by similarity
byrole_dist <- byrole_dist %>% arrange(Player1, -similarity)

# param for spider charts modules
roles <- players$Pos %>% unique()
  
pl_byroles <- players %>% 
  select(ID, Pos) 
pl_byroles <- pl_byroles %>% split(pl_byroles$Pos)

source('functions.R')


# define UI  -----
ui <- fluidPage(
  theme = bs_theme(),
    # tab1
  tabsetPanel(
    
    tabPanel("Data Exploration Players", 
             actionButton("start", "Start Exploration"),
             typeplotUi("type")),
    # tab 2
    tabPanel("PCA",
             pcaUI("pca")),

    # tab 3
    tabPanel("Spider Chart and Comparison",
             spider_chart_module_ui("sc")),
    # tab 4
    tabPanel("Spider Chart and Comparison by role",
             bygroup_spider_chart_module_ui("br_sc"))
    
    ) 
    
  )
  


# define server logic-----
server <- function(input, output, session) {
  bs_themer()
  # tab 3 and 4: Data exploration ------
  observeEvent(input$start, {
    

      
      # first tab
      callModule(typeplot, "type", data = scl_players)
      
      # second tab
      callModule(pca, "pca", data = scl_players)
      
      # third tab
      callModule(spider_chart_module, "sc", data_plot = scl_players, data_comp = ovl_dist)
      
      # fourth tab
      callModule(bygroup_spider_chart_module, "br_sc", data_plot = scl_players, 
                 data_comp = byrole_dist)
    
  })
  
}

shinyApp(ui, server)
