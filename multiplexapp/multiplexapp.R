
library(shiny)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(plotly)
library(readxl)
library(magrittr)
library(stats)


TemplateExample <- read_csv("TemplateExample.csv")
TitrationLevels <- read_csv("TitrationLevels.csv")

js <- HTML("
$(function() {
  let observer = new MutationObserver(callback);

  function clickHandler(evt) {
    Shiny.setInputValue('group_select', $(this).children('span').text());
  }

  function callback(mutations) {
    for (let mutation of mutations) {
      if (mutation.type === 'childList') {
        $('.dropdown-header').on('click', clickHandler).css('cursor', 'pointer');

      }
    }
  }

  let options = {
    childList: true,
  };

  observer.observe($('.inner')[0], options);
})
")


ui <- fluidPage(
  
  #load in the font from google 
  tags$link(href = 'https://fonts.googleapis.com/css?family=Montserrat', rel = 'stylesheet'),
  
  #Lots of CSS
  tags$head(tags$style(HTML('
    
                                body {
                               font-family:"Montserrat";
                                }
                              .navbar-bg {
                               background-color: #003776 !important;
                              }
                              .nav > li > a:hover, .nav > li > a:focus {
                              background-color: #FFFFFF;
                                }
                              .bg-grey {
                                background-color: #d6d6d6;
                              }
                              .bg-dark-grey {
                                background-color: #696868;
                                color: white;
                              }
                              hr{
                                height: 3px;
                                background-color: #014093;
                                border: none;
                                width:10%;
                              }
                              
                              .sidenav {
                                height: 100%; /* Full-height: remove this if you want "auto" height */
                                width: 230px; /* Set the width of the sidebar */
                                position: fixed; /* Fixed Sidebar (stay in place on scroll) */
                                z-index: 1; /* Stay on top */
                                top: 0; /* Stay at the top */
                                left: 0;
                                background-color: #111; /* Black */
                                overflow-x: hidden; /* Disable horizontal scroll */
                                padding-top: 20px;
                              }

                              /* The navigation menu links */
                              .sidenav a {
                                padding: 6px 8px 6px 16px;
                                text-decoration: none;
                                font-size: 20px;
                                color: #818181;
                                display: block;
                              }

                              /* When you mouse over the navigation links, change their color */
                              .sidenav a:hover {
                                color: #f1f1f1;
                              }


                              /* On smaller screens, where height is less than 450px, change the style of the sidebar (less padding and a smaller font size) */
                              @media screen and (max-height: 450px) {
                                .sidenav {padding-top: 15px;}
                                .sidenav a {font-size: 18px;}
                              }
                              
                              
                              ')), 
            tags$script(js)),
  tags$style(type = "text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ), 
  
  
  div(class = "fluid-page",
      div(class = "row text-center",
          div(class = "col-xl-12",style = "margin-left: 180px; margin-right: 180px;",
              br(),
              strong(h1('Multiplex Analysis')),
              hr(),
              uiOutput("controls_ui"),
              hr(),
              br(),
              br(),
              radioGroupButtons(
                inputId = "display_type",
                label = "",
                #This is all the types of pages
                choices = c("Base Graph", "Target", "Density", "Results"),
                status = "info",
                justified = TRUE,
                checkIcon = list(
                  yes = icon("ok", 
                             lib = "glyphicon"))
              ),
              br(),
              uiOutput("selection"),
              br(),
              #This is the graph output
              column(12, align = "center",
                     plotlyOutput("graph", width = "90%", height = 600),
                     br(),
                     br(),
                     DTOutput("results_table"),
                     br(),
                     br(),
                     downloadButton('downloadPlot', 'Download Plot'),
                     br(),
                     br(),
                     br(),
                     h1("Difference Summaries"),
                     hr(),
                     h4("Base Level is cycles 5-10, End Level is cycles 50-end "),
                     br(),
                     br(),
                     DTOutput("sum_dat"),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br()
              )
              
              
          )
          
          
      )
  )
  
  
  
)

server <- function(input, output, session) {
  
  #When the app is open enter information
  
  showModal(modalDialog(
    title = "Enter Multiplex Data",
    easyClose = FALSE,
    footer = NULL,
    size = "m",
    fileInput("csvFile", "Input Fluoresence File (csv)", width = 1200, accept = ".csv"),
    fileInput("xlsxFile", "Input Plate File (xlsx)", width = 1200),
    downloadLink('downloadData', 'Download Template/Example Plate File'),
    br(),
    br(),
    actionButton("submit_files", label = "Done")
  ))
  
  
  #Get the raw data from upload
  #TODO: Add error message if data not csv file, in wrong format, etc.
  rawData <- reactive({
    inFile <- input$csvFile
    if (is.null(inFile)) return(NULL)
    # ext <- tools::file_ext(inFile$datapath)
    # req(file)
    #validate(need(ext == "csv", c(print("Please upload a csv file"), return(NULL))))
    data <- read.csv(inFile$datapath, header = TRUE)
    data
  })
  
  #Get the template from upload
  #TODO: Add error message if data not csv file, in wrong format, etc.
  rawData2 <- reactive({
    inFile <- input$xlsxFile
    if (is.null(inFile)) return(NULL)
    data <- read_excel(inFile$datapath, sheet = "Data")
    data
  })
  
  #Merge them bois
  full_data <- reactive({
    TitrationLevels <- rawData()
    TemplateExample <- rawData2()
    
    colnames(TitrationLevels)[1] <- "Cycle"
    
    titration_long <- TitrationLevels %>% 
      pivot_longer(-Cycle)
    
    full_data <- left_join(titration_long, TemplateExample, by = c("name" = "Sample"))
    
    colnames(full_data)[4] <- "Probe concentration"
    colnames(full_data)[6] <- "Target Concentration"
    
    
    full_data
  })
  
  output$sum_dat <- renderDT({
    full_data <- full_data()
    
    
    Name <- unique(full_data$name)
    base_level <- rep(0, length(Name))
    end_level <- rep(0, length(Name))
    target <- rep(0, length(Name))
    index <- 1
    for (i in Name) {
      name_dat <- full_data[full_data$name == i,]
      target[index] <- name_dat$Target[1]
      low <- name_dat$value[name_dat$Cycle >= 5 & name_dat$Cycle <= 10]
      high <- name_dat$value[name_dat$Cycle >= 50]
      base_level[index] <- round(mean(low),2)
      end_level[index] <- round(mean(high),2)
      index = index + 1
    }
    difference <- end_level - base_level
    sum_dat <- data.frame(Name, target, base_level, end_level, difference)
    datatable(sum_dat, filter = "top")
    
    
  })
  
  
  
  #Download a template
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0('TemplateExample-', Sys.Date(), '.csv')
    },
    content = function(con) {
      write.csv(TemplateExample, con)
    }
  )
  
  #Remove modal when done 
  observeEvent(input$submit_files, {
    
    removeModal()
    
  })
  
  #Check that tables are working correctly 
  output$table <- renderTable({
    
    full_data_summary()
    
  })
  
  
  
  
  
  output$controls_ui <- renderUI({
    
    full_data <- full_data()
    wells <- list("A" = str_subset(full_data$name, "A") %>% unique(),
                  "B" = str_subset(full_data$name, "B") %>% unique(),
                  "C" = str_subset(full_data$name, "C") %>% unique(),
                  "D" = str_subset(full_data$name, "D") %>% unique(),
                  "E" = str_subset(full_data$name, "E") %>% unique(),
                  "F" = str_subset(full_data$name, "F") %>% unique(),
                  "G" = str_subset(full_data$name, "G") %>% unique(),
                  "H" = str_subset(full_data$name, "H") %>% unique())
    
    tagList(
      column(12, align = "center",
             pickerInput(
               inputId = "filter_target",
               label = "Filter Targets", 
               choices = as.character(unique(full_data$Target)),
               selected = as.character(unique(full_data$Target)),
               options = list(
                 `actions-box` = TRUE), 
               multiple = TRUE
             ),
             
             pickerInput(
               inputId = "filter_samples",
               label = "Filter Samples",
               choices = wells,
               selected = unique(full_data$name),
               options = list(
                 `actions-box` = TRUE,
                 `live-search` = TRUE),
               multiple = TRUE
             ),
             
             pickerInput(
               inputId = "filter_tc",
               label = "Filter Target Concentration", 
               choices = as.character(unique(full_data$`Target Concentration`)),
               selected = as.character(unique(full_data$`Target Concentration`)),
               options = list(
                 `actions-box` = TRUE,
                 `live-search` = TRUE), 
               multiple = TRUE
             ),
             
             pickerInput(
               inputId = "filter_pc",
               label = "Filter Probe Concentration", 
               choices = as.character(unique(full_data$`Probe concentration`)),
               selected = as.character(unique(full_data$`Probe concentration`)),
               options = list(
                 `actions-box` = TRUE,
                 `live-search` = TRUE), 
               multiple = TRUE
             ),
             
             sliderInput("filter_cycles", "Filter Cycles",
                         min = min(full_data$Cycle), 
                         max = max(full_data$Cycle), 
                         value =  c(min(full_data$Cycle),max(full_data$Cycle) ),
                         step = 1
             ),
             
             
      )
      
    )
    
  })
  
  observeEvent(input$group_select, {
    req(input$group_select)
    if (all(wells[[input$group_select]] %in% input$filter_samples)) {             
      sel <- input$filter_samples[!(input$filter_samples %in% wells[[input$group_select]])]         
    } else {             
      sel <- union(input$filter_samples, wells[[input$group_select]])         
    }
    updatePickerInput(session, "filter_samples", selected = sel)
  })
  
  output$selection <- renderUI({
    selection <- input$display_type
    
    if (selection == "Results") {
      full_data <- full_data()
      column(12, align = "center",
             pickerInput(
               inputId = "filter_control",
               label = "Select Controls", 
               choices = as.character(unique(full_data$Target)),
               options = list(`actions-box` = TRUE), 
               multiple = TRUE
             ),
             
             pickerInput(
               inputId = "get_sample",
               label = "Identify Samples",
               choices = as.character(unique(full_data$Target)),
               options = list(`actions-box` = TRUE),
               multiple = TRUE
             ),
             
             sliderInput("filter_endpoint", "Create Endpoint Target",
                         min = min(full_data$Cycle), 
                         max = max(full_data$Cycle), 
                         value =  c(max(full_data$Cycle) - 10,max(full_data$Cycle) ),
                         step = 1
             ))
    }
  })
  
  
  output$graph <- renderPlotly({
    
    full_data <- full_data()
    
    #filter Samples
    full_data <- full_data[full_data$name %in% input$filter_samples,]
    #Filter Targets
    full_data <- full_data[full_data$Target %in% input$filter_target,]
    #Filter Target Concentration
    full_data <- full_data[full_data$`Target Concentration` %in% input$filter_tc,]
    #Filter probe Concentration
    full_data <- full_data[full_data$`Probe concentration` %in% input$filter_pc,]
    #Filter Cycles
    full_data <- full_data[full_data$Cycle <= input$filter_cycles[2],]
    full_data <- full_data[full_data$Cycle >= input$filter_cycles[1],]
    #Set names in correct order
    full_data$name <- factor(full_data$name, levels = unique(full_data$name))
    
    selection <- input$display_type
    
    
    if (selection == "Base Graph") {
      ggplotly(
        ggplot(data = full_data) +
          geom_line(aes(x = Cycle, y = value, color =  name, group = Target)) +
          theme_minimal() +
          ylab("")
      ) %>% 
        config(displayModeBar = F) 
    }
    else if (selection == "Target") {
      ggplotly(
        ggplot(data = full_data) +
          geom_point(aes(x = Cycle, y = value, color =  Target)) +
          theme_minimal() +
          ylab("")
      ) %>% 
        config(displayModeBar = F) 
    }
    else if (selection == "Density") {
      
      full_data_ends <- full_data[full_data$Cycle == max(full_data$Cycle),]
      
      full_data_summary <- full_data_ends %>% 
        group_by(Target) %>% 
        summarise(sd = sd(value), avg = mean(value))
      
      ggplotly(
        ggplot() +
          geom_density(data = full_data_ends, aes(x = value, fill = Target), alpha = 0.5) +
          scale_y_reverse() +
          coord_flip() +
          theme_minimal() +
          geom_point(data = full_data_ends, aes(x = value, y = -.001, color = Target)) +
          geom_text(data = full_data_summary, aes(x = avg, y = -.003, label = round(sd, 2))) +
          geom_text(aes(x = 900, y = -.003, label = "std dev")) +
          ylab("")
      ) %>% 
        config(displayModeBar = F) 
      
    }
    else {
      find_outliers <- full_data %>% 
        filter(Cycle %in% 45:60 & Target %in% input$filter_control) %>%
        group_by(Target,name) %>%
        mutate(avg = mean(value)) %>%
        ungroup() %>%
        group_by(Target) %>%
        mutate(Q1 = quantile(value, .25),
               Q3 = quantile(value, .75),
               IQR_x = IQR(value),
               mini = Q1 - 1.5*IQR_x,
               maxi = Q3 + 1.5*IQR_x,
               outlier = case_when(avg > mini & avg < maxi ~FALSE, 
                                   TRUE ~ TRUE)) %>%
        select(name, avg, Q1, Q3, IQR_x, mini, maxi, outlier) %>%
        unique() %>%
        filter(outlier)
      
      full_data %<>% filter(!name %in% find_outliers$name)
      
      find_outliers <- full_data %>% 
        filter(Cycle %in% 45:60 & Target %in% input$filter_control) %>%
        group_by(Target,name) %>%
        mutate(avg = mean(value)) %>%
        ungroup() %>%
        group_by(Target) %>%
        mutate(Q1 = quantile(value, .25),
               Q3 = quantile(value, .75),
               IQR_x = IQR(value),
               mini = Q1 - 1.5*IQR_x,
               maxi = Q3 + 1.5*IQR_x,
               outlier = case_when(avg > mini & avg < maxi ~FALSE, 
                                   TRUE ~ TRUE)) %>%
        select(name, avg, Q1, Q3, IQR_x, mini, maxi, outlier) %>%
        unique() %>%
        filter(outlier)
      
      full_data %<>% filter(!name %in% find_outliers$name)
      
      more_dat <- full_data %>%
        group_by(Target, Cycle) %>%
        mutate(avg_point = mean(value),
               sd = sd(value),
               se = sd/sqrt(n())) %>%
        ungroup()
      
      # create controls dataframe - uses just the targets and finds avg, sd, and var of the last few cycles
      controls <- full_data %>% 
        filter(Cycle %in% input$filter_endpoint) %>% 
        group_by(Target) %>%
        summarize(avg = mean(value), 
                  sd = sd(value), 
                  var = var(value)) %>% 
        filter(Target %in% c(input$filter_control))
      
      # get all combinations
      n <- controls$Target %>% n_distinct()
      combos <- list()
      datalist <- list()
      t = 1
      
      for (i in 1:n) {
        x <- combn(controls$Target, i)
        
        for (j in 1:ncol(x)) {
          combos[t] <- paste(x[,j], collapse = ",")
          t = t + 1
        }
      }
      
      i = 1
      for (target in combos) {
        
        dat <- more_dat %>% 
          filter(Target %in% c(str_split(target, ",")[[1]])) %>%
          group_by(Cycle, Target) %>%
          summarize(avg_point = mean(avg_point),
                    se = mean(se)) %>%
          ungroup() %>%
          group_by(Cycle) %>%
          summarize(avg_point = sum(avg_point),
                    se = sqrt(sum(se^2))) %>%
          mutate(Target = target)
        
        datalist[[i]] = dat
        i = i + 1
      }
      
      all_controls = do.call(rbind, datalist)
      
      # find wells that test "positive"
      if (length(input$get_sample) > 0) {
        combo_test <- all_controls %>% 
          filter(Cycle %in% input$filter_endpoint) %>% 
          group_by(Target, Cycle) %>% 
          mutate(control_avg = mean(avg_point))
        
        ## take span of each well, compare to the span of control
        combo_tar_names <- all_controls$Target %>% unique()
        sample <- full_data %>% filter(Target == input$get_sample)
        wells <- sample$name %>% unique()
        
        i = 1
        pos_virus <- list()
        
        for (well in wells) {
          for (combo in combo_tar_names) {
            
            x1 <- full_data %>% 
              filter(name == well & Cycle %in% input$filter_endpoint)
            
            x2 <- combo_test %>% 
              filter(Target == combo)
            
            pos <- mean(abs(x1$value - x2$avg_point) < x2$se*2)
            x3 <- if (pos > .7) well
            
            pos_virus[[i]] <- x3
            i = i + 1
          }
        }
        
        if (length(pos_virus) > 0){
          positive <- do.call(rbind, pos_virus) %>% 
            as.data.frame() %>% left_join(full_data, by = c("V1" = "name")) %>% 
            select("Well" = V1, Target) %>% unique()
        }
      }
      if (exists("positive")){
        ggplotly(
          ggplot() +
            
            geom_line(data = full_data, aes(x = Cycle, y = value, group = name,
                                            color = name %in% positive$Well)) +
            geom_ribbon(data = all_controls, aes(x = Cycle, ymin = avg_point - 2*se,
                                                 ymax = avg_point + 2*se, group = Target),
                        alpha = .5) +
            geom_line(data = all_controls, aes(x = Cycle, y = avg_point, group = Target),
                      color = "firebrick") +
            theme_bw() +
            labs(title = "Standard Curve",
                 x = "Cycle",
                 y = "Fluorescence",
                 color = "Positive") +
            theme(plot.title = element_text(hjust = .5))) %>%
          config(displayModeBar = F) 
      } 
      else{
        ggplotly(
          ggplot() +
            
            geom_line(data = full_data, aes(x = Cycle, y = value, group = name,
                                            color = Target)) +
            geom_ribbon(data = all_controls, aes(x = Cycle, ymin = avg_point - 2*se,
                                                 ymax = avg_point + 2*se, group = Target),
                        alpha = .5) +
            geom_line(data = all_controls, aes(x = Cycle, y = avg_point, group = Target),
                      color = "firebrick") +
            theme_bw() +
            labs(title = "Standard Curve",
                 x = "Cycle",
                 y = "Fluorescence") +
            theme(plot.title = element_text(hjust = .5))) %>%
          config(displayModeBar = F)
      }
      
    }
  })
  
  output$results_table <- renderDT({
    
    full_data <- full_data()
    
    #filter Samples
    full_data <- full_data[full_data$name %in% input$filter_samples,]
    #Filter Targets
    full_data <- full_data[full_data$Target %in% input$filter_target,]
    #Filter Target Concentration
    full_data <- full_data[full_data$`Target Concentration` %in% input$filter_tc,]
    #Filter probe Concentration
    full_data <- full_data[full_data$`Probe concentration` %in% input$filter_pc,]
    #Filter Cycles
    full_data <- full_data[full_data$Cycle <= input$filter_cycles[2],]
    full_data <- full_data[full_data$Cycle >= input$filter_cycles[1],]
    #Set names in correct order
    full_data$name <- factor(full_data$name, levels = unique(full_data$name))
    
    selection <- input$display_type
    
    if (selection == "Results"){
      
      find_outliers <- full_data %>% 
        filter(Cycle %in% 45:60 & Target %in% input$filter_control) %>%
        group_by(Target,name) %>%
        mutate(avg = mean(value)) %>%
        ungroup() %>%
        group_by(Target) %>%
        mutate(Q1 = quantile(value, .25),
               Q3 = quantile(value, .75),
               IQR_x = IQR(value),
               mini = Q1 - 1.5*IQR_x,
               maxi = Q3 + 1.5*IQR_x,
               outlier = case_when(avg > mini & avg < maxi ~FALSE, 
                                   TRUE ~ TRUE)) %>%
        select(name, avg, Q1, Q3, IQR_x, mini, maxi, outlier) %>%
        unique() %>%
        filter(outlier)
      
      full_data %<>% filter(!name %in% find_outliers$name)
      
      find_outliers <- full_data %>% 
        filter(Cycle %in% 45:60 & Target %in% input$filter_control) %>%
        group_by(Target,name) %>%
        mutate(avg = mean(value)) %>%
        ungroup() %>%
        group_by(Target) %>%
        mutate(Q1 = quantile(value, .25),
               Q3 = quantile(value, .75),
               IQR_x = IQR(value),
               mini = Q1 - 1.5*IQR_x,
               maxi = Q3 + 1.5*IQR_x,
               outlier = case_when(avg > mini & avg < maxi ~FALSE, 
                                   TRUE ~ TRUE)) %>%
        select(name, avg, Q1, Q3, IQR_x, mini, maxi, outlier) %>%
        unique() %>%
        filter(outlier)
      
      full_data %<>% filter(!name %in% find_outliers$name)
      
      more_dat <- full_data %>%
        group_by(Target, Cycle) %>%
        mutate(avg_point = mean(value),
               sd = sd(value),
               se = sd/sqrt(n())) %>%
        ungroup()
      
      # create controls dataframe - uses just the targets and finds avg, sd, and var of the last few cycles
      controls <- full_data %>% 
        filter(Cycle %in% input$filter_endpoint) %>% 
        group_by(Target) %>%
        summarize(avg = mean(value), 
                  sd = sd(value), 
                  var = var(value)) %>% 
        filter(Target %in% c(input$filter_control))
      
      # get all combinations
      n <- controls$Target %>% n_distinct()
      combos <- list()
      datalist <- list()
      t = 1
      
      for (i in 1:n) {
        x <- combn(controls$Target, i)
        
        for (j in 1:ncol(x)) {
          combos[t] <- paste(x[,j], collapse = ",")
          t = t + 1
        }
      }
      
      i = 1
      for (target in combos) {
        
        dat <- more_dat %>% 
          filter(Target %in% c(str_split(target, ",")[[1]])) %>%
          group_by(Cycle, Target) %>%
          summarize(avg_point = mean(avg_point),
                    se = mean(se)) %>%
          ungroup() %>%
          group_by(Cycle) %>%
          summarize(avg_point = sum(avg_point),
                    se = sqrt(sum(se^2))) %>%
          mutate(Target = target)
        
        datalist[[i]] = dat
        i = i + 1
      }
      
      all_controls = do.call(rbind, datalist)
      
      # find wells that test "positive"
      
      combo_test <- all_controls %>% 
        filter(Cycle %in% input$filter_endpoint) %>% 
        group_by(Target, Cycle) %>% 
        mutate(control_avg = mean(avg_point))
      
      ## take span of each well, compare to the span of control
      combo_tar_names <- all_controls$Target %>% unique()
      sample <- full_data %>% filter(Target %in% input$get_sample)
      wells <- sample$name %>% unique()
      
      i = 1
      pos_virus <- list()
      
      for (well in wells) {
        for (combo in combo_tar_names) {
          
          x1 <- full_data %>% 
            filter(name == well & Cycle %in% input$filter_endpoint)
          
          x2 <- combo_test %>% 
            filter(Target == combo)
          
          pos <- mean(abs(x1$value - x2$avg_point) < x2$se*2)
          x3 <- if (pos > .7) well
          
          pos_virus[[i]] <- x3
          i = i + 1
        }
      }
      
      if (length(pos_virus) > 0){
        positive <- do.call(rbind, pos_virus) %>% 
          as.data.frame() %>% left_join(full_data, by = c("V1" = "name")) %>% 
          select("Well" = V1, Target) %>% unique() %>% datatable()
      }
    }
    
  })
  
  
  plotInput <- reactive({
    
    full_data <- full_data()
    
    #filter Samples
    full_data <- full_data[full_data$name %in% input$filter_samples,]
    #Filter Targets
    full_data <- full_data[full_data$Target %in% input$filter_target,]
    #Filter Target Concentration
    full_data <- full_data[full_data$`Target Concentration` %in% input$filter_tc,]
    #Filter probe Concentration
    full_data <- full_data[full_data$`Probe concentration` %in% input$filter_pc,]
    #Filter Cycles
    full_data <- full_data[full_data$Cycle <= input$filter_cycles[2],]
    full_data <- full_data[full_data$Cycle >= input$filter_cycles[1],]
    #Order names
    full_data$name <- factor(full_data$name, levels = unique(full_data$name))
    
    selection <- input$display_type
    
    
    if (selection == "Base Graph") {
      
      ggplot(data = full_data) +
        geom_line(aes(x = Cycle, y = value, color =  name)) +
        theme_minimal() +
        ylab("")
      
    }
    else if (selection == "Target") {
      
      ggplot(data = full_data) +
        geom_point(aes(x = Cycle, y = value, color =  Target)) +
        theme_minimal() +
        ylab("")
      
    } 
    else if (selection == "Density"){
      
      full_data_ends <- full_data[full_data$Cycle == max(full_data$Cycle),]
      
      full_data_summary <- full_data_ends %>% 
        group_by(Target) %>% 
        summarise(sd = sd(value), avg = mean(value))
      
      
      ggplot() +
        geom_density(data = full_data_ends, aes(x = value, fill = Target), alpha = 0.5) +
        scale_y_reverse() +
        coord_flip() +
        theme_minimal() +
        geom_point(data = full_data_ends, aes(x = value, y = -.001, color = Target)) +
        geom_text(data = full_data_summary, aes(x = avg, y = -.003, label = round(sd, 2))) +
        geom_text(aes(x = 900, y = -.003, label = "std dev")) +
        ylab("")
      
    }
    else {
      find_outliers <- full_data %>% 
        filter(Cycle %in% 45:60 & Target %in% input$filter_control) %>%
        group_by(Target,name) %>%
        mutate(avg = mean(value)) %>%
        ungroup() %>%
        group_by(Target) %>%
        mutate(Q1 = quantile(value, .25),
               Q3 = quantile(value, .75),
               IQR_x = IQR(value),
               mini = Q1 - 1.5*IQR_x,
               maxi = Q3 + 1.5*IQR_x,
               outlier = case_when(avg > mini & avg < maxi ~FALSE, 
                                   TRUE ~ TRUE)) %>%
        select(name, avg, Q1, Q3, IQR_x, mini, maxi, outlier) %>%
        unique() %>%
        filter(outlier)
      
      full_data %<>% filter(!name %in% find_outliers$name)
      
      find_outliers <- full_data %>% 
        filter(Cycle %in% 45:60 & Target %in% input$filter_control) %>%
        group_by(Target,name) %>%
        mutate(avg = mean(value)) %>%
        ungroup() %>%
        group_by(Target) %>%
        mutate(Q1 = quantile(value, .25),
               Q3 = quantile(value, .75),
               IQR_x = IQR(value),
               mini = Q1 - 1.5*IQR_x,
               maxi = Q3 + 1.5*IQR_x,
               outlier = case_when(avg > mini & avg < maxi ~FALSE, 
                                   TRUE ~ TRUE)) %>%
        select(name, avg, Q1, Q3, IQR_x, mini, maxi, outlier) %>%
        unique() %>%
        filter(outlier)
      
      full_data %<>% filter(!name %in% find_outliers$name)
      
      controls <- full_data %>% 
        filter(Cycle %in% input$filter_endpoint) %>% 
        group_by(Target) %>%
        summarize(avg = mean(value), sd = sd(value), var = var(value)) %>% 
        filter(Target %in% input$filter_control)
      
      more_dat <- full_data %>% 
        group_by(Target, Cycle) %>%
        mutate(avg_point = mean(value),
               sd = sd(value),
               se = sd/sqrt(n())) %>% 
        ungroup()
      
      
      n <- controls$Target %>% n_distinct()
      combos <- list()
      datalist <- list()
      t = 1
      
      for (i in 1:n) {
        x <- combn(controls$Target, i)
        
        for (j in 1:ncol(x)) {
          combos[t] <- paste(x[,j], collapse = ",")
          t = t + 1
        }
      }
      
      i = 1
      for (target in unique(combos)) {
        dat <- more_dat %>% 
          filter(Target %in% c(str_split(target, ",")[[1]])) %>%
          group_by(Cycle, Target) %>% 
          summarize(avg_point = mean(avg_point),
                    se = mean(se)) %>%
          ungroup() %>%
          group_by(Cycle) %>%
          summarize(avg_point = sum(avg_point),
                    se = sqrt(sum(se^2))) %>%
          mutate(Target = target)
        
        datalist[[i]] = dat
        i = i + 1
      }
      
      all_controls <- do.call(rbind, datalist)
      
      ggplot() +
        
        geom_line(data = full_data, aes(x = Cycle, y = value, group = name,
                                        color = Target)) +
        geom_ribbon(data = all_controls, aes(x = Cycle, ymin = avg_point - 2*se,
                                             ymax = avg_point + 2*se, group = Target),
                    alpha = .5) +
        geom_line(data = all_controls, aes(x = Cycle, y = avg_point, group = Target),
                  color = "firebrick") +
        theme_bw() +
        labs(title = "Standard Curve",
             x = "Cycle",
             y = "Fluorescence") +
        theme(plot.title = element_text(hjust = .5))
      
    }
    
  })
  
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste('plot', '.png', sep='') },
    content = function(file) {
      ggsave(file,plotInput())
    }
  )
  
}


shinyApp(ui = ui, server = server)