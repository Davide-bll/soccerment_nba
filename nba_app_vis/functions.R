##### funzioni di shinymod per app mobile
require(tidyverse)

# Utils-----
#  UTILS:
# create new columns using case_when
# deal with different types of vars
# filter a reactieve input in a dataframe



# 1:logical----
# check if the arguments are equal to the x input, according to the type function.



#' Generalise logical statements
#' look if x is equal to the dots arguments. If ... is a list, then you can specify
#' a function rule in type, passing by name.
#'
#' @param x A character
#' @param ... A vector of character
#' @param type a function
#'
#' @return A logical
#' @import purrr shiny
#' @export
#'
#' @examples logi_test("ciao", c("come", "va", "ciao"))
#'           logi_test("ciao", c("come", "va", "ciao"), type = "all")
logi_test <- function(x, ..., type = "any") {
  
  arg <- list(...)
  
  # if x is created
  if(shiny::isTruthy(x)) {
    res <- purrr::map_lgl(arg, ~isTRUE(x == .x))
    # action on the result
    res <-  do.call(type, list(res))
    
    res
  }
}


# 2: detect type of vars----


#' Detect Date-Datetime object
#'
#' @param x 
#'
#' @return logical
#' @export
#'
is.datetime <- function(x) {
  lubridate::is.Date(x) | lubridate::is.POSIXct(x) | lubridate::is.POSIXt(x)
}

#' Extract class of variables from a dataframe
#' @description Extract class of variables from a dataframe. Factors are considered character
#' @param x Dataframe
#' @param num_to_chr Character string. Numerical Columns ending/starting with \code{num_to_chc}
#' will be considered character.
#'
#' @return list of numerical, character, datevars
#' @import dplyr
#' @export
#'
extract_types <- function(x, num_to_chr = "id") {
  
  # numerical vars
  num_vars <- x %>% select_if(is.numeric) %>% names()
  
  # find character vars
  
  chc_vars <- x %>% select_if(~(is.character(.) | is.factor(.))) %>% names()
  # date- date/time vars
  date_vars <- x %>% select_if(is.datetime) %>% names()
  
  # data consistency : if starts or ends with "id" don t consider them num
  add_vars <- num_vars[(endsWith(num_vars, num_to_chr) | startsWith(num_vars, num_to_chr))]
  
  # update chc_vars
  chc_vars <- c(chc_vars, add_vars)
  
  # update num vars
  num_vars <- num_vars[!(endsWith(num_vars, "id") | startsWith(num_vars, "id"))]
  
  list(
    chc_vars  = chc_vars,
    num_vars  = num_vars,
    date_vars = date_vars
  )
}

#' Extract class of variables from a dataframe dataframe
#'
#' @param x df
#'
#' @return list
#' @import dplyr
#' @export
#'
extract_types_original <- function(x) {
  
  # numerical vars
  num_vars <- x %>% select_if(is.numeric) %>% names()
  
  # find character vars
  chc_vars <- x %>% select_if(~(is.character(.) | is.factor(.))) %>% names()
  
  # date- date/time vars
  date_vars <- x %>% select_if(is.datetime) %>% names()
  
  
  list(
    chc_vars  = chc_vars,
    num_vars  = num_vars,
    date_vars = date_vars
  )
}



# 3 filtering-----
# userfilter
#' Filter chr or num vars
#'
#' @param x df
#' @param param col name 
#' @param val numeric interval or charachter string
#'
#' @import dplyr
#' @export
#' @return filtered df
#'
user_filter <- function(x, param, val) {
  # works for numerical and datevars
  param <- sym(param)
  if(is.numeric(val) | is.datetime(val)) {
    x <- x %>%
      filter(!!param <= max(val) & !!param >= min(val))
  }
  
  if(is.character(val)) {
    x <- x %>% filter(!!param %in% val)
  }
  
  x
}

# apply simaultaneous filter
# 
#' Simultaneous filter
#'
#' @param x df
#' @param params list of params 
#' @param vals  list of vals
#' @export
#'
#' @return filtered df
#'
user_filter_m <- function(x, params, vals) {
  
  for (i in (1:length(params))) {
    
    x <- user_filter(x, params[[i]], vals[[i]])
  }
  x
}

# look for a specific element inside a list
#'look for a specific element inside a list
#'
#' @param x list
#' @param look name of elemet in the list to look for
#' @export
#'
#' @return list
#'
look_for <- function(x, look = "transformed") {
  if(look %in% names(x)) {
    return(x[[look]])
  } else {
    x
  }
}


# LOAD DATA SHINY ---------

# Module UI CSV files Input
#' Ui function foa loading csv Data 
#'
#' @param id Id module
#'
#' @return UI module
#' @import shiny
#' @export
#'
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading"),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    )), 
    selectInput(ns("sep"), "sep", c(
      "comma"     = ",", 
      "semicolon" = ";", 
      "space"     = " "
    ))
  )
}




# Module server function: csv 
#' Server function of Load - data CSV
#' @description @seealso \link{csvFileInput}
#' @param stringsAsFactors FALSE by default.
#'
#' @return A reactive Dataframe
#' @import shiny utils
#' @export
#'
csvFile <- function(input, output, session, stringsAsFactors = FALSE) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()$datapath,
             header = input$heading,
             quote  = input$quote,
             sep    = input$sep,
             stringsAsFactors = stringsAsFactors)
  })
  
  
  # Return the reactive that yields the data frame
  return(dataframe)
}

# proviamo con i file txt
# Module UI function
#'UI:Txt Input 
#'
#' @param id 
#' @return UI Module
#' @import shiny
#' @export 
#'
txtFileInput <- function(id, label = "TXT file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label),
    checkboxInput(ns("heading"), "Has heading"),
    selectInput(ns("quote"), "Quote", c(
      "None" = "",
      "Double quote" = "\"",
      "Single quote" = "'"
    ))
  )
}

# Module server function
#' Server function for loading txt files
#' @param stringsAsFactors FALSE by default
#'
#' @return A A reactiv dataframe
#' @import utils shiny
#' @export
#'
txtFile <- function(input, output, session, stringsAsFactors = FALSE) {
  # The selected file, if any
  userFile <- reactive({
    
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    validate(need(userFile(), message = FALSE))
    
    
    read.table(userFile()$datapath,
               header = input$heading,
               quote = input$quote,
               stringsAsFactors = stringsAsFactors, 
               fill = TRUE, 
               row.names = NULL)
  })
  
  
  # Return the reactive that yields the data frame
  return(dataframe)
}


# read feather fie
#' Load Feather file in shiny
#'
#' @param id 
#'
#' @return A UI module
#' @import shiny
#' @export
#'
featherFileInput <- function(id, label = "FEATHER format") {
  
  ns <- NS(id)
  
  
  tagList(
    fileInput(ns("file"), label)
  )
  
  
}

# server function
#'Server function of  load Feather data module
#'
#' @return A Reactive Dataframe
#' @import shiny
#' @export
#'
featherFile <- function(input, output, session) {
  inputfile <- reactive({
    
    validate(need(input$file, message = "Please choose a file"))
    
    input$file
  })
  
  dataframe <- reactive({
    
    feather::read_feather(inputfile()$datapath)
  })
  
  return(dataframe)
}

# UI function 
#' Load Data UI module
#' @description Load Data in shiny. Choose between .txt, .csv, .FEATHER format
#'
#' @param id UI identification 
#'
#' @return Ui function.
#' @import shiny
#' @export
loadfileUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    selectInput(ns("format"), "Select the format file", 
                choices = c("csv", "txt", "feather"), 
                selected = "txt"), 
    
    uiOutput(ns("datafile"))
  )
  
}


#' Load Data in Shiny. Server function
#' @description Choose between .txt, .csv, .FEATHER format. 
#' @return A reactive Dataframe.
#' @import shiny
#' @export
#' @examples
#' require(shiny)
#' if (interactive()) {
#' ui <- 
#'   fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(
#'         loadfileUI(id = "read")
#'     ), 
#'       mainPanel(tableOutput("tbl"))
#'   )
#' )
#' 
#' 
#' server <- function(input, output, session) {
#'   
#'   data <- callModule(loadfile, "read")
#'   
#'   output$tbl <- renderTable({
#'     validate(need(data(), message = FALSE))
#'     
#'     head(data()())
#'   })
#' }
#' 
#'shinyApp(ui, server)
#' }
loadfile <- function(input, output, session) {
  # detect which case
  inputfile <- reactive({
    
    validate(need(input$format, message = FALSE))
    
    
    switch(input$format, 
           csv     = csvFile, 
           txt     = txtFile, 
           feather = featherFile)
  })
  
  # generate ui momdule
  output$datafile <-  renderUI({
    validate(need(input$format, message = FALSE))
    
    ns <- session$ns
    
    UImodule <- paste0(input$format, "FileInput")
    label <-    paste(input$format, "Format", sep = "  ")
    
    # call the UI module
    do.call(what = UImodule, 
            args =list(id = ns("datafile"), label = label))
    
    
    
    
  })
  
  
  datafile <- reactive({
    
    validate(need(inputfile(), message = FALSE))
    
    # call the server module 
    callModule(inputfile(), "datafile")
    
  })
  
  return(datafile)
}


# Filter modules---------
# filter panel 

##################################r 
# apply reactively some filter
##################################r

# # load functions----
# folder <- "C://Users//Utente//Desktop//shiny_modules//functions"
# source(paste(folder, "utils.R", sep = "//"))

# UI function----
#' Ui Module for filtering
#' @description @seealso \link{filterpanel}
#' @param id 
#'
#' @return A Ui module
#' @import dplyr
#' @export
#'
filterpanelUi <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    checkboxGroupInput(ns("flags"), label = "Choose action",
                       choices = c("filter", "select")),
    uiOutput(ns("action")),      # action button: apply the filters
    uiOutput(ns("params")),
    uiOutput(ns("values")),
    uiOutput(ns("excluded")),
    uiOutput(ns("recovery")))     # recovery the input data
  
}

# SERVER function----
#' Filter Shiny Module
#'
#' @param data A Dataframe to filter. Accept numerical and chr vars
#'
#' @return A reactive Dataframe
#' @import dplyr
#' @export
#'
#' @examples
#' require(shiny)
#' if(interactive()) {
#'
#' ui <- fluidPage(
#' 
#'   filterpanelUi("prova"),
#' 
#'   mainPanel(tableOutput("tbl"))
#' 
#' )
#' 
#' server <- function(input, output, session) {
#' 
#'   dt <- callModule(filterpanel, "prova", data = CO2)
#' 
#'   output$tbl <- renderTable({
#' 
#'     dt()
#'   })
#' 
#' }
#' shinyApp(ui, server)
#' 
#'}
filterpanel <- function(input, output, session, data) {
  
  # detect types of data
  l_vars <- extract_types(data)
  
  # initialise filtered data ----
  new_data <- reactiveVal({ data })
  
  output$action <- renderUI({
    ns <- session$ns
    
    if(!is.null(input$flags)) actionButton(ns("action"), "DO ACTION")
    
  })
  
  # initialize output
  observeEvent(input$action,{
    
    if("filter" %in%  input$flags){
      new_val <-  user_filter(new_data(), input$params, input$values)
      new_data(new_val)
    }
    
  })
  
  observeEvent(input$action, {
    
    vars <- input$excluded
    if(length(vars > 0)) {
      new_val <- new_data() %>% select(-vars)
      new_data(new_val)
    }
    
  })
  
  # generate UIs ----
  # choose the variable to which apply the filter
  
  # filter ui----
  output$params <- renderUI({
    ns <- session$ns
    
    # consider only num vars or date vars
    choices <- c(l_vars[["num_vars"]], l_vars[["date_vars"]], l_vars[["chc_vars"]])
    req(input$flags)
    test <- "filter" %in% input$flags
    if(test) {
      selectInput(ns("params"), "Choose var to apply filter", choices = choices)
    }
    
  })
  
  # reactive param
  params <- reactive({
    req(input$params)
    
    input$params
  })
  
  # select the range of params
  output$values <- renderUI({
    
    ns <- session$ns
    req(input$flags)
    test <- ("filter" %in% input$flags) & (!is.null(input$params))
    if(test) {
      vals <- new_data()[[params()]]
      if(is.numeric(vals) | is.datetime(vals)) {
        sliderInput(ns("values"), "Choose range" , min = min(vals), max = max(vals), value = c(min(vals), max(vals)))
        
      } else if(is.character(vals) | is.factor(vals)) {
        
        selectInput(ns("values"), "choose levels", choices = unique(vals), multiple = TRUE)
      }
    }
    
    
  })
  
  # select function  UI----
  output$excluded <- renderUI({
    ns <- session$ns
    
    req(input$flags)
    test <- "select" %in% input$flags
    
    if(test) {
      selectInput(ns("excluded"), "Select vars to exclude", choices = names(new_data()), multiple = TRUE)
    }
    
  })
  
  # recovery button -----
  #UI
  output$recovery <- renderUI({
    ns <- session$ns
    
    # if an action is made
    if(!is.null(input$flags)) {
      actionButton(ns("recovery"), "Recovery Data")
    }
  })
  
  # recovery observer
  observeEvent(input$recovery, {
    new_val <- data
    new_data(new_val)
  })
  
  return(new_data)
}

# Graphic functions-----
# customize scatter plots
# histograms
# check normal hypotesy
# create an histogram object from the column "x_col" of the dataframe "df"
#'create an histogram object 
#'
#' @param df df
#' @param x_col col to represent 
#' @param title optional title
#' @param custom optional cusyom
#' @import ggplot2
#' @export
#' @return histogram object
#'
df_hist <- function(df, x_col, title = "Aggregate", custom = NULL) {
  
  if(is.null(custom)) {
    df_h <- df %>%
      ggplot(aes_string(x = x_col)) +
      geom_histogram(fill ="darkblue", color = "black", 
                     binwidth = custom) + 
      labs(title = paste(x_col, title, sep = "  -  "))
  } else {
    
    # create histogram object
    df_h <- df %>%
      ggplot(aes_string(x = x_col)) +
      geom_histogram(fill ="darkblue", color = "black") + 
      labs(title = paste(x_col, title, sep = "  -  "))
  }
  
  
  df_h
}

df_hist_grp <- function(df, x_col, title = "Aggregate", custom = NULL, grp = NULL) {
  
  if(is.null(custom)) {
    df_h <- df %>%
      ggplot(aes_string(x = x_col)) +
      geom_histogram(fill ="darkblue", color = "black", 
                     binwidth = custom) + 
      labs(title = paste(x_col, title, sep = "  -  "))
  } else {
    
    # create histogram object
    df_h <- df %>%
      ggplot(aes_string(x = x_col)) +
      geom_histogram(fill ="darkblue", color = "black") + 
      labs(title = paste(x_col, title, sep = "  -  "))
  }
  
  if(! is.null(grp)) df_h <- df_h + facet_wrap(grp)
  
  df_h
}


# from a numerical vector x, return a list different of plots verifing normal hyp
# scatter okit if x vs y 
# x limits and y limits are additional argment according to which df will be filtered
# if "color" is not null, then the scatter is colored wrt "color"
#' Scatter plot
#'
#' @param df dataframe
#' @param x col name of x axis
#' @param y col name of y axis
#' @param x_limits interval to filter x axis
#' @param y_limits interval to filter y axis
#' @param color chr.  Name of the variable to which apply color logic.
#' @param wrap chr. column to which apply facet_wrap, y scale is free 
#' @param title Optional.
#'
#' @import ggplot2 dplyr
#' @export
#' @return ggplot object
#'
scatter_plot <- function(df, x = NULL, y, x_limits = NULL, y_limits = NULL, 
                         color = NULL, title = "Scatter", wrap = NULL) {
  
  
  if(!is.null(x_limits)) {
    df <- df %>% filter(!!sym(x) <= max(x_limits) & !!sym(x) >= min(x_limits))
  }
  
  if(!is.null(y_limits)) {
    df <- df %>% filter(!!sym(y) <= max(y_limits) & !!sym(y) >= min(y_limits))
  }
  
  if(is.null(x)) {
    x <- 1:nrow(df)
  }
  
  if(is.null(color)) {
    
    res <- df %>%
      ggplot(aes_string(x = x, y = y)) + 
      geom_point() + 
      geom_line() +
      labs(xlab = element_blank(), 
           title = title)
  } else {
    
    res <- df %>%
      ggplot(aes_string(x = x, y = y, color = color)) + 
      geom_point() + 
      geom_line() +
      labs(xlab = element_blank(), 
           title = title)
    
  }
  if(!is.null(wrap)) res <- res + facet_wrap(wrap, scales = "free_y")
  
  return(res)
}


#' Graphical representation of Normal assumption, Univariate data
#'
#' @param x Num Vector
#' @param title_list Optional
#' @return list of plots: Scatter of \code{x}, histogram of \code{x}, qqplot of \code{x}
#' @export
#' @import ggplot2 dplyr
#'
normal_plots <- function(x, title_list = list(hist = "histogram", 
                                              scatter = "scatter", 
                                              qqnorm = "Theoretical vs Sample quantiles")) {
  
  if(!("hist" %in% names(title_list) & "scatter" %in% names(title_list) & "qqnorm" %in% names(title_list))) {
    
    names(title_list) <- c("hist", "scatter", "qqnorm")
  }
  
  df <- tibble::enframe(x, name = "obs", value = "value")  
  # scatter of x
  sx <- scatter_plot(df, y = "value", title = title_list[["scatter"]])
  
  # histogram of x
  hx <-   df_hist(df, "value", title = title_list[["hist"]])
  
  # qq norm
  qqn <- qqnorm(scale(x)) %>% 
    as_tibble() %>% 
    rename(theoretical = x, 
           sample_scaled = y) 
  
  qqn <- scatter_plot(qqn, x = "theoretical", y = "sample_scaled", 
                      title = title_list[["qqnorm"]]) 
  
  qqn <- qqn + geom_abline(slope = 1, intercept = 0)
  # results
  list(scatter = sx, 
       hist    = hx, 
       qqnorm  = qqn 
  )  
  
}

spider_df <- function(df, values_id, id_name = "ID", lower = 0, upper = 1) {
  df <- df %>% filter(!!sym(id_name) %in% values_id)
  num_vars <- extract_types_original(df)[['num_vars']]
  n_cols <- length(num_vars)
  
  df_bounds <- rbind(rep(upper, n_cols), rep(lower, n_cols)) %>% data.frame()
  colnames(df_bounds) <- num_vars
  
  bind_rows(df_bounds, select(df, num_vars))
  
}

spider_chart <- function(df, values_id, id_name = "ID", lower = 0, upper = 1) {
  colors_border <-  RColorBrewer::brewer.pal(length(values_id), 'Dark2')
  
  spider_df(df, values_id, id_name = id_name, lower = lower, upper = upper) %>% 
    radarchart( axistype=1 , 
                #custom polygon
                plwd=4 , plty=1,
                pcol = colors_border,
                #custom the grid
                cglcol="grey", 
                cglty=1, 
                axislabcol="grey", 
                cglwd=0.8,
                #custom labels
                vlcex=0.8 
    )
  
  # Add a legend
  legend(x=1, y=1, legend = values_id, bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=1.2, pt.cex=3)
  
}


# Plot module------
# UI module: histogram, scatter 1/2 vars, boxplot
# define UI
#' Automatic Data Visualization in Shiny
#'
#' @param id Identification Module
#'
#' @return Ui module
#' @export
#'
typeplotUi <- function(id) {
  ns <- NS(id) 
  
  tagList(
    
    # Panels on the left---------------
    sidebarPanel(
      selectInput(ns("type_plot"), "Choose a type of visualisation:", 
                  choices = c("boxplot", "histogram",  
                              "plot_1var", "plot_2var"), 
                  selected = "histogram"),
      
      uiOutput(ns("custom")),
      uiOutput(ns("width")),
      uiOutput(ns("grp")),
      uiOutput(ns("custom2")), 
      uiOutput(ns("width2")),
      uiOutput(ns("grp_selection")),
      uiOutput(ns("x_col")),
      uiOutput(ns("set_axis1")),
      uiOutput(ns("y_axis1")), 
      uiOutput(ns("x_col2")),
      uiOutput(ns("set_axis2")),
      uiOutput(ns("y_axis2")),
      uiOutput(ns("x_axis2"))
    ),
    
    # output ----------------
    mainPanel(
      
      plotOutput(ns("plot_1")),
      plotOutput(ns("plot_2")))
  )
}

# server function of type plot  UI
#' Server function of Data Visualization in Shiny
#'
#' @param data A Dataframe
#'
#' @return A Shiny server module
#' @import dplyr graphics shiny 
#' @export
#'
#' @examples
#' # an example 
#' require(shiny)
#' if(interactive()) {
#'
#' ui <- fluidPage(
#' 
#'   typeplotUi("type")
#' 
#' 
#' )
#' 
#' server <- function(input, output, session) {
#' 
#'   callModule(typeplot, "type", data = CO2)
#' 
#' }
#' 
#' 
#' shinyApp(ui, server)
#' 
#'}
typeplot <- function(input, output, session, data) {
  
  # change factpr to charachter
  data <- data %>% mutate_if(is.factor, as.character)
  # extract different type of vars: chc, num, date/datetime  
  l_vars <- extract_types(data)
  
  # reactive variables-----    
  type_pl <- reactive({
    validate(need(input$type_plot, message = FALSE))
    input$type_plot
    
  })
  
  # dynamic UI:  conditional panels---------------
  # customize width of histogram
  output$x_col <- renderUI({
    
    ns <- session$ns
    
    test <- logi_test(type_pl(), "histogram", "plot_1var", "plot_2var", type = "any")
    # if histogram is present
    if(isTRUE(test)) {   
      selectInput(ns("x_col"), "Variable to show:",
                  choices = l_vars[["num_vars"]])
    }
  })
  
  xcol <- reactive({
    
    validate(need(input$x_col, message = FALSE))
    input$x_col
  })
  # customize histogram?
  output$custom <- renderUI({
    ns <- session$ns
    
    test <- logi_test(type_pl(), "histogram")
    if(isTRUE(test)) {
      checkboxInput(ns("custom"), "Customize", FALSE)
    } 
  })
  
  # choose width of the first histogram classes
  output$width <- renderUI({
    ns <- session$ns
    validate(need(input$custom, message = FALSE))
    
    if(input$custom && type_pl() == "histogram") {
      
      sliderInput(ns("width"), "Change Width", min = 0, max = 150, value = 50)  
    }
    
  })
  
  # limit y axis?
  output$set_axis1 <- renderUI({
    
    ns <- session$ns
    
    test <- logi_test(type_pl(), "plot_1var")
    if(isTRUE(test)) {
      checkboxInput(ns("set_axis1"), "limit values Y-axis", FALSE)
    } 
  })
  
  output$y_axis1 <- renderUI({
    
    ns <- session$ns
    
    validate(need(input$set_axis1, message = FALSE))
    
    if(input$set_axis1 && type_pl() == "plot_1var") {
      # find min and max vals
      min_val <- data[[xcol()]] %>% min
      max_val <- data[[xcol()]] %>% max
      
      sliderInput(ns("y_axis1"), paste0("Change range of ", xcol(), "-axis" ), min = min_val, max = max_val, 
                  value = c(min_val, max_val))  
    }
    
    
  })
  
  # limits for y of scatter plot 1var
  y_axis1 <- reactive({
    validate(need(input$y_axis1, message = FALSE))
    
    c(min(input$y_axis1) , max(input$y_axis1))
    
  })
  
  # grp selection
  output$grp <- renderUI({ 
    
    ns <- session$ns
    test <- logi_test(type_pl(), "histogram", "plot_1var", "plot_2var", type = "any")
    
    
    
    if(isTRUE(test)) {
      
      selectInput(ns("grp"), "Choose a Grouping Variable:", 
                  choices = c(l_vars[["chc_vars"]], "aggregate"), 
                  selected = "aggregate") }
    
  })
  
  grp_var <- reactive({
    
    validate(need(input$grp, message = FALSE))    
    input$grp
    
  })
  
  # customize histogram 2 classes width?
  output$custom2 <- renderUI({
    
    
    ns <- session$ns
    
    test <- all(logi_test(type_pl(), "histogram"), 
                logi_test(grp_var(), "aggregate", type = "!"))
    
    if(isTRUE(test)) {
      checkboxInput(ns("custom2"), "Customize grouped histogram", FALSE)
    } 
    
  })
  
  
  output$width2 <- renderUI({
    ns <- session$ns
    
    validate(need(input$custom2, message = FALSE))
    
    test <- all(logi_test(type_pl(), "histogram"), 
                logi_test(grp_var(), "aggregate", type = "!"))
    
    if(input$custom2 && test) {
      
      sliderInput(ns("width2"), "Change Width plot 2", min = 0, max = 150, value = 30)  
    }
  })
  
  # choose the level of the selcted group
  output$grp_selection <- renderUI({
    
    # if it makes sense evaluating groupvar
    ns <- session$ns
    test <- all(logi_test(grp_var(), "aggregate", type = "!"), logi_test(type_pl(), "histogram"))
    
    if(isTRUE(test)) {
      
      grp_sel <- data[[grp_var()]] %>% unique()
      selectInput(ns("grp_selection"), paste("Choose the Level of", grp_var(), sep = ": "),
                  choices = grp_sel)
      
    }
    
  })
  
  
  
  output$x_col2 <- renderUI({
    
    ns <- session$ns
    
    
    
    # evaluate logical condition: plot2 var present
    test <- logi_test(type_pl(), "plot_2var")
    
    if(isTRUE(test)) {  
      
      selectInput(ns("x_col2"), "Second Variable to show:",
                  choices = c(l_vars[["date_vars"]], l_vars[["num_vars"]]))
    }
    
    
  })
  
  
  xcol2 <- reactive({
    
    req(input$x_col2)
    input$x_col2
  })
  
  
  # change limits of axis 2? 
  output$set_axis2 <- renderUI({
    
    ns <- session$ns
    
    test <- logi_test(type_pl(), "plot_2var")
    if(isTRUE(test)) {
      checkboxInput(ns("set_axis2"), "limit values of  XY-axis", FALSE)
    } 
  })
  
  # UI y_axis 2
  output$y_axis2 <- renderUI({
    
    ns <- session$ns
    
    validate(need(input$set_axis2, message = FALSE))
    
    if(input$set_axis2 && type_pl() == "plot_2var") {
      # find min and max vals
      min_val <- data[[xcol()]] %>% min
      max_val <- data[[xcol()]] %>% max
      
      sliderInput(ns("y_axis2"), paste0("Change range of ", xcol(), "-axis" ), min = min_val, max = max_val, 
                  value = c(min_val, max_val))  
    }
    
    
  })
  
  # limits for y of scatter plot 2var
  y_axis2 <- reactive({
    validate(need(input$y_axis2, message = FALSE))
    
    c(min(input$y_axis2),  max(input$y_axis2))
    
  })
  
  # UI: set x axis 2
  output$x_axis2 <- renderUI({
    
    ns <- session$ns
    
    validate(need(input$set_axis2, message = FALSE))
    
    if(input$set_axis2) {
      # find min and max vals
      min_val <- data[[xcol2()]] %>% min
      max_val <- data[[xcol2()]] %>% max
      
      sliderInput(ns("x_axis2"), paste0("Change range of ", xcol2(), "-axis" ), min = min_val, max = max_val, 
                  value = c(min_val, max_val))  
    }
    
    
  })
  
  # limits for x of scatter plot 1var
  x_axis2 <- reactive({
    validate(need(input$x_axis2, message = FALSE))
    
    c(min(input$x_axis2) , max(input$x_axis2))
    
  })
  
  
  ###### dyamic list of data ------
  #they change depending on the grouping variable 
  
  l_data <- reactive({  
    
    if(grp_var() != "aggregate") {
      l_data <- split(data, data[[grp_var()]])   
      return(l_data)
      
    }
  })
  
  
  
  ######  plot 1 window:   --------
  #### aggregated plot: boxplot, histogram,  plot of xvars 
  ###  loadings of the vars for the first k pca
  output$plot_1 <- renderPlot ({
    
    if(type_pl() == "histogram") {
      
      width <- 30
      
      # check if custom control is activated
      if(isTruthy(input$custom)) {
        if(input$custom) {
          validate(need(input$width, message = FALSE))
          
          width <- input$width 
        }
      }
      
      validate(need(xcol(), message = FALSE))
      # generate histogram
      df_hist(data, xcol(), custom = width) %>% plot()
      
    }  
    
    
    if(type_pl() == "boxplot") {
      
      # do boxplot aggregated (of the selected numerical vars)
      data[l_vars[["num_vars"]]] %>%
        graphics::boxplot(main = "Vars Box plot")
    }
    
    if(type_pl() == "plot_1var") {
      # by default, don't do any filter
      y_lim <- NULL
      
      # if setyaxis is tru
      if(isTRUE(input$set_axis1)) {
        y_lim <- y_axis1()
      }
      scatter_plot(data, y = xcol(), y_limits = y_lim) %>% plot()
      
    }
    
    if(type_pl() == "plot_2var"){
      # by default, don't do any filter
      x_lim <- NULL
      y_lim <- NULL
      
      # if setyaxis is tru
      if(isTRUE(input$set_axis2)) {
        
        y_lim <- y_axis2()
        x_lim <- x_axis2()
        
      }
      
      
      scatter_plot(data, x = xcol2(), y = xcol(), x_limits = x_lim, y_limits = y_lim) %>% plot()
      
    }
    
    
    
    
    
  })
  
  ######  plot 2 window:  ----------
  ####### grouped histogram, boxplot, plot
  
  
  output$plot_2 <-  renderPlot ({
    
    # wait for input.grp_var    
    validate(need(input$grp, message = FALSE))
    
    if(grp_var() != "aggregate") {
      
      if(type_pl() == "histogram") {
        
        if(isTruthy(input$grp_selection)) {
          width2 <- 30
          
          if(input$custom2) {
            validate(need(input$width2, message = FALSE))
            width2 <- input$width2
          } 
          
          lev1 <- input$grp_selection
          lev2 <- xcol()
          df <- l_data()[[lev1]]

          # grouped histograms
          df_hist(df, x_col = lev2, title = lev1, custom = width2) %>% plot()
          #df_hist_grp(data, x_col = lev2, title = lev1, custom = width2, grp = grp_var()) %>% plot()
        }
      }
      
      if(type_pl() == "plot_1var"){
        
        # by default, don't do any filter
        y_lim = NULL
        
        # if setyaxis is tru
        if(isTRUE(input$set_axis1)) {
          y_lim <- y_axis1()
        }
        
        
        scatter_plot(data, y = xcol(), color = grp_var(), y_limits = y_lim) %>% plot()
        
      }
      
      if(type_pl() == "plot_2var") {
        # by default, don't do any filter
        x_lim <- NULL
        y_lim <- NULL
        
        # if setyaxis is tru
        if(isTRUE(input$set_axis2)) {
          y_lim <- y_axis2()
          x_lim <- x_axis2()
        }
        
        
        
        scatter_plot(data, x = xcol2(), y = xcol(), color = grp_var(), 
                     x_limits = x_lim, y_limits = y_lim) %>% plot()
        
      }
      
    }
    
  })
  
}
# PCA modules------
# Variance module: 
# numerical summaries
# pca on normal and scaled data

# makes sure the function in  "C://Users//Utente//Desktop//shiny_modules//function//utils.R"
# are loaded 


# require(dplyr)
# 
# # load functions----
# folder <- "C://Users//Utente//Desktop//shiny_modules//functions"
# source(paste(folder, "utils.R", sep = "//"))

# UI function ----
#' PCA shiny module
#'
#' @param id Module identification
#'
#' @return Shiny Ui module
#' @import shiny
#' @export
#'
pcaUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarPanel(
      selectInput(ns("type_pca"), "PCA TYPE:",
                  choices = c("normal", "scaled"), 
                  selected = "normal"), 
      uiOutput(ns("k_c")),
      selectInput(ns("plot2"), "Choose the type of visualisation:", 
                  choices =c("cum_sd" ,"stat_2c"), 
                  selected = "cum_sd"), 
      textOutput(ns("stats")), 
      checkboxInput(ns("exclude"), "exclude some vars? ", value = FALSE), 
      uiOutput(ns("vars_excluded"))
    ), 
    mainPanel(
      plotOutput(ns("plot1")), 
      plotOutput(ns("plot2"))
    )
  )
}

# server logic -----
#' Perform PCA analysis on shiny
#'
#' @param data  A Dataframe
#'
#' @return
#' @import dplyr shiny
#' @export
#'
#' @examples
#' require(shiny)
#' if(interactive()) {
#'
#' ui <- fluidPage(
#'   title = "Principal Component Analysis",
#'   pcaUI("pca")
#' 
#'   )
#' 
#' server <- function(input, output, session) {
#' 
#'   callModule(pca, "pca", data = mtcars)
#' }
#' shinyApp(ui, server)
#'} 
pca <- function(input, output, session, data) {
  
  # detect types of columns
  l_vars <- extract_types(data)
  
  # consider only num vars
  data <- data[l_vars[["num_vars"]]]
  
  # logical must some numerical vars be excluded?
  output$vars_excluded <- renderUI({
    
    ns <- session$ns
    
    validate(need(input$exclude, message = FALSE))
    
    if(input$exclude) {
      selectInput(ns("vars_excluded"), "Select the vars to be excluded", 
                  choices  = names(data), 
                  multiple = TRUE)
    }
  })
  # reactive dataframe----
  r_data <- reactive({
    
    res <- data
    
    
    if(!is.null(input$vars_excluded)) {
      excluded <- input$vars_excluded
      
      res <- data %>%
        select(-excluded)
      
    }
    
    res
  })
  # get PC's
  # reactive pca object -------
  l_pca <- reactive({
    
    # main statistics
    # covariance Matrix
    S <- r_data() %>% 
      as.matrix() %>%
      cov()
    
    # generalized Variance
    Gv <- eigen(S)$values %>% prod()
    
    # total variance
    Tv <- eigen(S)$values %>% sum()
    
    validate(need(input$type_pca, message = FALSE))
    
    cor <- switch(input$type_pca, 
                  normal = FALSE, 
                  scaled = TRUE)
    
    df_pca <- r_data() %>%
      princomp(scores = TRUE, cor = cor)
    
    # loadings
    df_load <- df_pca$loadings
    
    # scores
    df_scores <- df_pca$scores
    
    # explained cumulative std
    cum_var <- cumsum(df_pca$sdev/ sum(df_pca$sdev)) 
    
    # list of results
    list( S         = S, 
          Gv        = Gv, 
          Tv        = Tv,
          df_pca    = df_pca, 
          df_load   = df_load, 
          df_scores = df_scores,
          cum_var   = cum_var)
  })
  
  output$k_c <- renderUI({
    
    
    ns <- session$ns
    
    sliderInput(ns("k_c"), "Select the component:", min = 1, max = ncol(r_data()), 
                value = 1, step = 1)
    
    
  })  
  
  output$stats <- renderText ({
    paste("Total Variance:", as.character(round(l_pca()[["Tv"]], digits = 1)),
          "    Generalized Variance:", as.character(round(l_pca()[["Gv"]], digits = 1)))
    
  })
  
  # first plot-----
  output$plot1 <- renderPlot({
    
    
    validate(need(input$k_c, message = FALSE))  
    
    loads <- l_pca()[["df_load"]]  
    # first  k components    
    graphics::barplot(loads[, input$k_c], 
                      main  = paste0("PCA  component: ", input$k_c), 
                      col = "red")
    
  })
  
  output$plot2 <- renderPlot({
    
    validate(need(input$plot2, message = FALSE))
    
    
    if(input$plot2 == "stat_2c") {
      scores <-l_pca()[["df_scores"]]
      graphics::plot(scores[, 1:2], main = "Stat-Units against first two PC")              
    }
    if(input$plot2 == "cum_sd") {
      
      df <- l_pca()[["cum_var"]]
      graphics::plot(df, main = "Cumulative Standar Deviation",
                     type = "b", 
                     xlab = "Principal Components", 
                     ylab = "Cumulative Standard Deviation",
                     ylim = c(0:1))
    }
  })
}

# Spider chart modules -----
# generalize this
spider_chart_module_ui <- function(id) {
  
  ns <- NS(id)
  # questo deve essere creata in una ui reattiva
  
  tagList(
      
    sidebarPanel(
      uiOutput(ns("players"))
    ),
    mainPanel(
      plotOutput(ns("spider_plot")), 
      tableOutput(ns("table_sim"))
    )
    
  )
  
}

spider_chart_module <- function(input, output, session, data_plot, col_id = "ID", data_comp) {
  
  output$players <- renderUI({
    choices <- data_plot %>% pull(col_id) %>% unique()
    ns <- session$ns
    
    selectInput(ns("players"), "Choose players you want to compare", choices = choices, 
                selected = choices[1], multiple = TRUE)
    
  })
  
  pl_choosed <- reactive({
    validate(need(input$players, message = FALSE))
    input$players
    
  })
  
  output$spider_plot <- renderPlot({
    #browser()    
    spider_chart(data_plot, pl_choosed())
    
  })
  
  output$table_sim <- renderTable({
    req(pl_choosed())
    
    data_comp %>% 
      filter((Player1 %in% pl_choosed() & Player2 %in% pl_choosed())) %>% 
      mutate_if(is.numeric, scales::percent)
    
  })
  
}

bygroup_spider_chart_module_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    uiOutput(ns("role")), 
    uiOutput(ns("player1")), 
    uiOutput(ns("player2")), 
    
    mainPanel(
      plotOutput(ns("spider_plot")), 
      tableOutput(ns("table_sim"))
    )
    
  )
  
}

# data_comp -> Player1, Player2, similarity and complementarity 
bygroup_spider_chart_module <- function(input, output, session, grp_col = "Pos", data_plot, data_comp, 
                                        grp_col_1 = "Player1", grp_col_2 = "Player2") {
  
  # render UI with role
  output$role <- renderUI({
    
    ns <- session$ns
    
    choices <- data_plot %>% pull(grp_col) %>% unique()
    
    
    selectInput(ns("role"), "Choose role:", 
                choices = choices, 
                selected = choices[1], 
                multiple = FALSE)
    
  })
  
  role_ch <- reactive({
    validate(need(input$role, message = FALSE))
    input$role
    
  })
  
  output$player1 <- renderUI({
    ns <- session$ns
    
    req(input$role)
    
    
    choices_player1 <- data_comp %>% 
      filter(Pos %in% role_ch()) %>% 
      pull(grp_col_1)
    
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
      pull(grp_col_2)
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
    req(pl_choosed())
    spider_chart(data_plot, pl_choosed())
    
  })
  
  output$table_sim <- renderTable({
    req(pl_choosed())
    data_comp %>% 
      filter((!!sym(grp_col_1) == pl_choosed()[1] & !!sym(grp_col_2) == pl_choosed()[2])) %>% 
      mutate_if(is.numeric, scales::percent)
    
  })
  
}

