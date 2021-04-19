remove(list = ls())

# an example of data exploration:

# require scripts for UI modules ---------


require(shiny)
require(dplyr)
require(RColorBrewer)
require(fmsb)
require(scales)
require(tidyr)

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
byrole_dist %>% arrange(Player1, -similarity)

# param for spider charts modules
roles <- players$Pos %>% unique()
  
pl_byroles <- players %>% 
  select(ID, Pos) 
pl_byroles <- pl_byroles %>% split(pl_byroles$Pos)

source('functions.R')
spider_chart_module_ui <- function(id) {
  
  ns <- NS(id)
  choices <- scl_players %>% pull(ID) %>% unique()
  
  tagList(
    
    sidebarPanel(
      selectInput(ns("players"), "Choose players:", 
                  choices = choices, 
                  selected = choices[1], 
                  multiple = TRUE)
    ), 
    
    mainPanel(
      plotOutput(ns("spider_plot")), 
      tableOutput(ns("table_sim"))
    )
    
  )
  
}

spider_chart_module <- function(input, output, session, data_comp=ovl_dist) {
  
  pl_choosed <- reactive({
    validate(need(input$players, message = FALSE))
    input$players
    
  })
  
  output$spider_plot <- renderPlot({
    #browser()    
    spider_chart(scl_players, pl_choosed())
    
  })
  
  output$table_sim <- renderTable({
    
    data_comp %>% 
      filter((Player1 %in% pl_choosed() & Player2 %in% pl_choosed())) %>% 
      mutate_if(is.numeric, scales::percent)
    
  })
  
}
byr_spider_chart_module_ui <- function(id) {
  
  ns <- NS(id)
  choices <- scl_players %>% pull(ID) %>% unique()
  
  tagList(
    
    sidebarPanel(
      selectInput(ns("role"), "Choose role:", 
                  choices = roles, 
                  selected = roles[1], 
                  multiple = FALSE), 
      uiOutput(ns("player1")), 
      uiOutput(ns("player2"))
    ), 
    
    mainPanel(
      plotOutput(ns("spider_plot")), 
      tableOutput(ns("table_sim"))
    )
    
  )
  
}

byr_spider_chart_module <- function(input, output, session, data_comp=ovl_dist) {
  
  role_ch <- reactive({
    validate(need(input$role, message = FALSE))
    input$role
    
  })
  
  output$player1 <- renderUI({
    ns <- session$ns
    
    req(input$role)
    
      
    choices_player1 <- data_comp %>% 
      filter(Pos %in% role_ch()) %>% 
      pull(Player1)

    selectInput(ns("player1"), label = paste("Choose", role_ch(), "1:"), 
                choices = choices_player1, 
                selected = choices_player1[1], 
                multiple = FALSE)
    
    
    
  })
  
  output$player2 <- renderUI({
    ns <- session$ns
    
    req(input$player1)
    # list of chices base on player 1
    choices_player2 <- data_comp %>% 
      filter(Player1 == input$player1) %>% 
      arrange(-similarity) %>%
      pull(Player2)
    label2 <- paste("Choose", role_ch(), "2, ", "ascending order of complementarity")
    selectInput(ns("player2"), label = label2, 
                choices = choices_player2, 
                selected = choices_player2[1], 
                multiple = FALSE)
    
  })
  
  
  pl_choosed <- reactive({
    req(input$role)
    req(input$player1)
    req(input$player2)
    
    c(input$player1, input$player2)
    
  })
  
  output$spider_plot <- renderPlot({
    #browser()    
    spider_chart(scl_players, pl_choosed())
    
  })
  
  output$table_sim <- renderTable({
    req(pl_choosed())
    data_comp %>% 
      filter((Player1 == pl_choosed()[1] & Player2 == pl_choosed()[2])) %>% 
      mutate_if(is.numeric, scales::percent)
    
  })
  
}

# define UI  -----
ui <- fluidPage(
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
             byr_spider_chart_module_ui("br_sc"))
    
    ) 
    
  )
  


# define server logic-----
server <- function(input, output, session) {
  # tab 3 and 4: Data exploration ------
  observeEvent(input$start, {
    

      
      # first tab
      callModule(typeplot, "type", data = scl_players)
      
      # second tab
      callModule(pca, "pca", data = scl_players)
      
      # third tab
      callModule(spider_chart_module, "sc")
      # fourth tab
      callModule(byr_spider_chart_module, "br_sc", data = byrole_dist)
    
  })
  
}

shinyApp(ui, server)
