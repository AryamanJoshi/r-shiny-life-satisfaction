#PART 1: SET UP#################################################################

# Data
library(shiny)
library(shinythemes)
library(bslib)
library(ggplot2)
library(tidyverse)
library(plotly)
library(modeldata)
#library(DataExplorer)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(shinycssloaders)
library(shinycustomloader)
library(shinydashboard)
library(ggthemr)
ggthemr("earth")
ggthemr("earth", type = "outer", 
        layout = "scientific", spacing = 2)

# Datasets

#final_project0 <- read_csv("final_project.csv")
final_project0 <- read_csv(
  "life-satisfaction.csv")

final_project <- final_project0 %>%
  mutate(life_satisfaction, ls_category = ifelse(life_satisfaction > 5, "5-6.5", "0-5")) %>%
  mutate(ls_category, ls_category = ifelse(life_satisfaction > 6.5, "6.5+", ls_category)) 

final_project_factor_rename <- final_project %>%
  rename("Country" = country,
         "Life Satisfaction" = life_satisfaction,
         "Economic Freedom" = economic_freedom,
         "Population Growth" = population_growth,
         "Life Expectancy" = life_expectancy,
         "Inflation" = inflation,
         "GDP" = gdp,
         "Income" = income,
         "Unemployment" = unemployment,
         "Political Rights" = political_rights,
         "Urban Population" = urban_population,
         "Female Labour" = female_labour,
         "Population Density" = population_density,
         "Civil Liberties" = civil_liberties,
         "Beer Consumption" = beer_consumption,
         "Suicide Rate" = suicide_all,
         "Male Suicide Rate" = suicide_male,
         "Female Suicide Rate" = suicide_female)

final_project_boxplots <- final_project %>%
  filter(!is.na(incomelevel)) %>% 
  mutate(incomelevel = ifelse(incomelevel == "highincome", "High", incomelevel), 
         incomelevel = ifelse(incomelevel == "lowermiddleincome", "Lower-Middle", incomelevel), 
         incomelevel = ifelse(incomelevel == "uppermiddleincome", "Upper-Middle", incomelevel), 
         incomelevel = ifelse(incomelevel == "lowincome", "Low", incomelevel)) %>%
  rename("Income Level" = incomelevel,
         "Freedom Status" = freedom_status,
         "Region" = region)


#PART 2: UI AREA################################################################
ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel("A Study on Life Satisfaction Across the World",),
  tags$h4("MSCS 264A Final Project"),
  tags$h5("by Aryaman Manish Joshi"),
  
  navbarPage(
    
    title = div(img(src = "Spongebob.gif", height = 50, width = 50), "Contents:"),
    
    #ABOUT
    #-------------------------------------------------------------------------------------------------------------------------------------
    tabPanel("About",
             mainPanel(
               h1("Welcome!"),
               img(src = "Satisfied Photo.jpg", height = 400, width = 400),
               h5(""),
               ("For this project, we collect the life-satisfaction data from the Gallup World Survey for 132 countries from 1995 till 2019. Our main aim is 
                  to see how life-satisfaction levels differ across various demographics and analyse how different variables relate to life-satisfaction
                  These variables include political freedom, civil liberty indexes, beer per capita for countries, mental health resources, and other 
                  socio-economic factors that can influence a country's life-satisfaction levels. What causes higher life-satisfaction levels on a larger scale?"),
               h3("How were the life-satisfaction levels collected?"),
               ("The underlying source of the happiness scores in the World Happiness Report is the Gallup World Poll-a set of nationally representative surveys undertaken in more than 160 countries in over 140 languages. The main life evaluation question asked in the poll is: \"Please imagine a ladder, with steps numbered from 0 at the bottom to 10 at the top. The top of the ladder represents the best possible life for you and the bottom of the ladder represents the worst possible life for you. On which step of the ladder would you say you personally feel you stand at this time?\" (Also known as the \"Cantril Ladder\".)

The map plots the average answer that survey-respondents provided to this question in different countries. As with the steps of the ladder, values in the map. range from 0 to 10.

There are large differences across countries. According to 2016 figures, Nordic countries top the ranking: Finland, Norway, Denmark, the Netherlands and Iceland have the highest scores (all with averages above 7). In the same year, the lowest national scores correspond to Central African Republic, South Sudan, Tanzania, Rwanda and Haiti (all with average scores below 3.5)."),
               h3("Research Questions"),
               
               ("1) Which Countries are the most satisfied?"),
               h5(""),
               ("2) How do external factors like political freedom, civil liberty indexes, beer per capita for countries, mental health resources, and other 
                  socio-economic factors that can influence a country's life-satisfaction levels"),
               h5(""),
               ("3) Have life satisfaction levels changed over the years?"),
               h5(""),
               ("4) Are certain continents happier in general than others?"),
               h1(""),
               h1(""),
             )),
    
    # Life Satisfaction Rankings
    #-------------------------------------------------------------------------------------------------------------------------------------      
    navbarMenu("Life Satisfaction Rankings",
               tabPanel("World",
                        
                        sidebarPanel(                                   # sidebarPanel
                          
                          tags$h3("Select Year:"),
                          selectInput(inputId = "year", label = "Year", choices = final_project$year,
                                      selected = "Blue",multiple = F),
                          #selectInput(inputId = "year", label = "Region", choices = final_project$region,
                          #selected = "Blue",multiple = F),
                          
                        ), 
                        
                        mainPanel(                                      # mainPanel
                          
                          h1("World Life Satisfaction Rankings"), 
                          
                          ("Which are the top 10 countries with the highest life-satisfaction levels?
                             What regions are the most satisfied countries from?"),
                          
                          withLoader(tableOutput("satisfaction1"), type = "image", loader = "Loading.gif"),
                          
                        )
               ),
               
               #Life Satisfaction Levels by Continent
               
               tabPanel("Continent",
                        sidebarPanel(                                   # sidebarPanel
                          
                          tags$h3("Select Year and Continent:"),
                          
                          selectInput(inputId = "year2", label = "Year", choices = final_project$year,
                                      selected = "Blue", multiple = F),
                          selectInput(inputId = "region", label = "Region", choices = final_project$region,
                                      selected = "Blue", multiple = F),
                          
                        ), 
                        
                        mainPanel(                                      # mainPanel
                          
                          h1("Life Satisfaction Rankings by Continent"),          
                          
                          ("Which are the top 10 countries with the highest life-satisfaction levels?
                             What regions are the most satisfied countries from?"),
                          
                          withLoader(tableOutput("satisfaction2"), type = "image", loader = "Loading 2.gif"),
                          
                        )
               ),
               
    ), # Navbar 1, tabPanel
    
    #-------------------------------------------------------------------------------------------------------------------------------------
    #Factor Comparison
    tabPanel("Factor Comparison", 
             
             mainPanel(                                      # mainPanel
               h2("Comparing Life Satisfaction levels with Various Other Factors"),      
               
               ("What are the external factors that can affect life-satisfaction levels? Have a look at this scatterplot with a variable of your choice to analyse its 
                  correlation with regards to life-satisfaction levels. Hover over the dots to see the country and the relevant parameters! Click and drag on the plot to zoom into your desired spot! Double-Click/Tap to zoom out."),
               
               withLoader(plotlyOutput("ls_vs"), type = "image", loader = "Spinning GIF.gif"),
               
               
               varSelectInput(inputId = "variable", label = "Select Factor to Compare", data = final_project_factor_rename[c(5:16, 18:21)]
               ),
               #Factor Comparison with animations            
               ("The previous plot shows one country multiple times for different years. Here, you can click play to 
                  see countries change their values over time depending on the year!"),
               
               withLoader(plotlyOutput("ls_vs_animate"), type = "image", loader = "Brushing Teeth.gif"),
               
               varSelectInput(inputId = "variable_animate", label = "Select Factor to Compare", data = final_project_factor_rename[c(5:16, 18:21)]
               ),
               
               
             )),
    #-------------------------------------------------------------------------------------------------------------------------------------
    
    #Change Over the Years
    
    tabPanel("Change over the Years", 
             tags$h2("Have Life Satisfaction Levels Changed Over the Years?"),
             ("How have life satisfaction levels changed over the years? Look at this interactive plot to find out!
                Select your factor, then click play and observe the size of the orange ball with respect to the life-satisfaction levels (green line) to see if you find anything interesting. The bigger the ball, the higher the value of your selected factor"),
             
             mainPanel(                                      # mainPanel
               
               #Change Over the Years (World)
               h3("World"),           
               withLoader(plotlyOutput("COTY_out1"), type = "image", loader = "Loading.gif"),
               varSelectInput(inputId = "variable_coty_1", label = "Select Factor to Compare", data = final_project_factor_rename[c(5:16, 18:21)]
               ),
               
               #Change Over the Years (Country)
               tags$h3("by Country"),
               withLoader(plotlyOutput("COTY_out2"), type = "image", loader = "Brushing Teeth.gif"),
               selectInput(inputId = "COTY2", label = "Select Country", choices = final_project$country,
                           selected = "Blue", multiple = F),
               varSelectInput(inputId = "variable_coty_2", label = "Select Factor to Compare", data = final_project_factor_rename[c(5:16, 18:21)]
               ),
               
               #Change Over the Years (Continent)
               tags$h3("by Continent"),
               withLoader(plotlyOutput("COTY_out3"), type = "image", loader = "Spinning GIF.gif"),
               selectInput(inputId = "COTY3", label = "Select Continent", choices = final_project$region,
                           selected = "Blue", multiple = F),
               varSelectInput(inputId = "variable_coty_3", label = "Select Factor to Compare", data = final_project_factor_rename[c(5:16, 18:21)]
               ),
               
               
             )),
    
    #-------------------------------------------------------------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------------------------------------------------------------
    #World Map
    
    tabPanel("World Map", 
             
             mainPanel(                                      # mainPanel
               h2("Comparing Life Satisfaction with Various Other Factors on a World Map"),
               ("Here's a different angle of the visualization. See how the factors compare on a 
                  world map! Select your desired factors and observe the colour of the points. Note that the shape of 
                  the point reflects the life-satisfaction level of the country. Are the factors sensitive to the continent?
                  Do they affect life satisfaction levels?"),
               
               withLoader(plotOutput("world_map"), type = "image", loader = "Loading.gif"),
               
               varSelectInput(inputId = "variable2", label = "Select Factor to Compare", data = final_project_factor_rename[c(4:16, 18:21)]
               ),
               
               #Plotly Map was abandoned.
               
               #withSpinner(plotlyOutput("world_map_animate")),
               
               #varSelectInput(inputId = "variable_map_animate", label = "Select Factor to Compare", data = final_project_factor_rename[c(4:16, 18:21)]
               #),
               
             )),
    
    #-------------------------------------------------------------------------------------------------------------------------------------
    
    #Boxplot Was abandoned due to irreparable bugs. 
    
    #tabPanel("Boxplots", 
    
    #         mainPanel(                                      # mainPanel
    #           h1("Comparing Life Satisfaction with Various Other Factors"),           
    #           withSpinner(plotlyOutput("boxplot")),
    
    #           varSelectInput(inputId = "variable3", label = "Select Factor to Compare", data = final_project_boxplots[c(17, 22, 25)]
    #           )
    
    #          )),
    
    #-------------------------------------------------------------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------------------------------------------------------------
    
    #Sources 
    
    tabPanel("Sources",
             
             mainPanel(
               tags$h4("Sources:"),
               tags$h5("- Happiness Data (Gallup World Survey)"),
               tags$h5("- Economic Freedom(Fraser's Institute)"),
               tags$h5("- Political Freedom and Civil Liberty (Freedom House)"),
               tags$h5("- Beer Per capita (World Health Organization Global Health Observatory)"),
               tags$h5("- Mental Health Resources (World Health Organization Global Health Observatory)"),
               tags$h5("- Universal Health Coverage (World Health Organization Global Health Observatory)"),
               tags$h3("Thank you for viewing our app!"),
               img(src = "Thanks.gif", height = 464, width = 350),
             ) #Main Panel
    ),
    #-------------------------------------------------------------------------------------------------------------------------------------
    
  ) # navbarPage
) # fluidPage


#-------------------------------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------------------------------  


#PART 3: SERVER FUNCTION AREA###################################################  
server <- function(input, output) {
  
  #Life Satisfaction By Year (World)
  output$satisfaction1 <- renderTable({
    Sys.sleep(0.10)
    yearFilter <- subset(final_project, final_project$year == input$year) %>%
      select(country, life_satisfaction, region) %>%
      arrange(-life_satisfaction) %>%
      rename(Country = country, `Life Satisfaction` = life_satisfaction
             #, Region = region
      ) %>%
      mutate(Rank = row_number()) %>%
      summarise(Rank, Country, `Life Satisfaction`
                #, Region
      ) %>%
      na.omit()
  })
  
  
  #-----------------------------------------------------------------------------
  
  #Life Satisfaction By Year (Continent)
  output$satisfaction2 <- renderTable({
    Sys.sleep(0.10)
    yearFilter <- subset(final_project, final_project$year == input$year2) %>%
      select(country, life_satisfaction, region) %>%
      arrange(-life_satisfaction) %>%
      rename(Country = country, `Life Satisfaction` = life_satisfaction
             , Region = region
      ) %>%
      mutate(`World Rank` = row_number()) %>%
      summarise(`World Rank`, Country, `Life Satisfaction`
                , Region
      ) %>%
      na.omit() %>%
      filter(Region == input$region) %>%
      mutate(`Regional Rank` = row_number()) %>%
      summarise(`Regional Rank`,`World Rank`, Country, `Life Satisfaction`
                , Region
      )
  })
  
  #-----------------------------------------------------------------------------
  
  #Life Satisfaction Over the Years (World)
  output$COTY_out1 <- renderPlotly({
    
    final_project_factor_rename %>%
      group_by(year) %>%
      summarize(mean_ls = mean(`Life Satisfaction`, na.rm = TRUE),
                mean = mean(.data[[input$variable_coty_1]], na.rm = TRUE)) %>%
      ggplot(mapping = aes(x = year, y = mean_ls)) +
      geom_smooth() +
      labs(y = "Mean Life Satisfaction", x = "Year", title = "World") +
      scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) +
      geom_point(aes(size = mean, frame = year))
    
  })
  
  #-----------------------------------------------------------------------------
  
  #Life Satisfaction Over the Years (Country)
  output$COTY_out2 <- renderPlotly({
    
    final_project_factor_rename %>%
      group_by(year) %>%
      filter(Country == input$COTY2) %>%
      summarize(mean_ls = mean(`Life Satisfaction`, na.rm = TRUE),
                mean = mean(.data[[input$variable_coty_2]], na.rm = TRUE)) %>%
      ggplot(mapping = aes(x = year, y = mean_ls)) +
      geom_smooth() +
      geom_point(aes(size = mean, frame = year)) +
      labs(y = "Mean Life Satisfaction", x = "Year") +
      scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))
  }) 
  
  #-----------------------------------------------------------------------------
  
  #Life Satisfaction Over the Years (Continent)
  output$COTY_out3 <- renderPlotly({
    
    final_project_factor_rename %>%
      group_by(year) %>%
      filter(region == input$COTY3) %>%
      summarize(mean_ls = mean(`Life Satisfaction`, na.rm = TRUE),
                mean = mean(.data[[input$variable_coty_3]], na.rm = TRUE)) %>%
      ggplot(mapping = aes(x = year, y = mean_ls)) +
      geom_smooth() +
      geom_point(aes(size = mean, frame = year)) +
      labs(y = "Mean Life Satisfaction", x = "Year") +
      scale_x_continuous(breaks=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019))
  }) 
  
  #-----------------------------------------------------------------------------
  
  #Factor Comparisons
  
  output$ls_vs <- renderPlotly({
    Sys.sleep(1)
    
    final_project_factor_rename %>%
      ggplot(mapping = aes(y = `Life Satisfaction`, x = .data[[input$variable]])) +
      geom_point(aes(color = region, shape = freedom_status, text = paste("Country:", Country)), size = 2) +
      geom_smooth(aes(y = `Life Satisfaction`, x = .data[[input$variable]]), color = "black") +
      labs(y = "Life Satisfaction", shape = "Freedom Status", color = "Continent")
    
    
  })
  
  #Animated Factor Comparisons
  
  output$ls_vs_animate <- renderPlotly({
    Sys.sleep(1)
    
    final_project_factor_rename %>%
      ggplot(mapping = aes(y = `Life Satisfaction`, x = .data[[input$variable_animate]])) +
      geom_point(aes(color = region, shape = freedom_status, text = paste("Country:", Country), frame = year), size = 2) +
      geom_smooth(aes(y = `Life Satisfaction`, x = .data[[input$variable_animate]]), color = "black") +
      labs(y = "Life Satisfaction", shape = "Freedom Status", color = "Continent")
    
  })
  
  
  #-----------------------------------------------------------------------------
  #World Map Comparison
  
  memory.size(max = TRUE)
  memory.limit(size = NA)
  
  output$world_map <- renderPlot({
    
    world <- map_data("world") #Filter antarctica???
    
    final_project2 <- final_project_factor_rename %>%
      group_by(Country) %>%
      mutate(mean = mean(.data[[input$variable2]], na.rm = TRUE),
             mean_ls = mean(`Life Satisfaction`, na.rm = TRUE)) %>%
      mutate(mean_ls, ls_category2 = ifelse(mean_ls > 5, "5-6.5", "0-5")) %>%
      mutate(ls_category2, ls_category2 = ifelse(mean_ls > 6.5, "6.5+", ls_category2))
    
    ggplot() +
      
      geom_map(
        data = world, map = world,
        aes(long, lat, map_id = region),
        color = "black", fill = "lightgray", size = 0.1
      ) +
      
      geom_point(
        data = final_project2,
        aes(longitude, latitude, color = mean, shape = ls_category2),
        alpha = 0.7, size = 3
      ) +
      
      scale_color_viridis(option = "A")+
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(shape = "Life Satisfaction")
  })
  #-----------------------------------------------------------------------------
  
  #Animated World Maps (ABANDONED)
  
  output$world_map_animate <- renderPlotly({
    
    world <- map_data("world") #Filter antarctica???
    
    final_project_map_animate <- final_project_factor_rename %>%
      group_by(Country) %>%
      mutate(mean = mean(.data[[input$variable_map_animate]], na.rm = TRUE),
             mean_ls = mean(`Life Satisfaction`, na.rm = TRUE))
    
    ggplot() +
      
      geom_map(
        data = world, map = world,
        aes(long, lat, map_id = region),
        color = "black", fill = "lightgray", size = 0.1
      ) +
      
      geom_point(
        data = final_project_map_animate,
        aes(longitude, latitude, color = mean, shape = ls_category, frame = year),
        alpha = 0.7, size = 3
      ) +
      
      scale_color_viridis(option = "A")+
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(shape = "Life Satisfaction")
  })
  
  #-----------------------------------------------------------------------------
  #Boxplots (ABANDONED)
  
  output$boxplot <- renderPlotly({ 
    
    #try parse number
    
    final_project_boxplots %>%
      
      ggplot(mapping = aes(x = life_satisfaction, y = .data[[input$variable3]])) +
      geom_boxplot() +
      coord_flip() +
      labs(y = "Life Satisfaction")
    
  })
  
  #-----------------------------------------------------------------------------
  #-----------------------------------------------------------------------------
  
  
} # server



#PART 4: SHINY OBJECT CREATOR###################################################
shinyApp(ui = ui, server = server)

#library(rsconnect)
#rsconnect::deployApp('C:/Users/aryam/Desktop/Coding Projects/R/Life Satisfaction Shiny App')


#Thank you for the great semester! Here's a programming joke for you:
#Aah debugging, where you are the detective at a crime scene for a crime YOU committed.

