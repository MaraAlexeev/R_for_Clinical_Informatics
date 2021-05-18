library(shiny)
library(tidyverse)
library(magrittr)
library(plotly)
library(knitr)
library(kableExtra)
library(DT)

df_labeled  <- readRDS(file = "df_labeled.rds")
symptom_df <- readRDS(file = "symptom_df.rds")

all_dates_owid_data <- readRDS(file = "all_dates_owid_data.rds")

sx_name_values <- readRDS(file = "sx_name_values.rds")

sx_names <- readRDS(file = "sx_names.rds")

test_column <- c("Median Age",
                 "Life Expectancy",
                 "Rank of Life Expectancy",
                 "Total Cases",
                 "Rank of Total Cases",
                 "Total Deaths",
                 "Rank of Total Deaths",
                 "Total Vaccinations",
                 "Rank of Vaccinations",
                 "Population",
                 "Population Density",
                 "Rank of Population Density", 
                 "GDP Per Capita",
                 "Rank of GDP per Capita")

units <- c("Years", "Years", "Rank", 
           "per million", "Rank", 
           "per million", "Rank",
           "per hundred", "Rank",
           
           "People", "People per sq km", "Rank", 
           "USD per Person", "Rank"
           )

details_of_interest <- c("median_age", 
                         "life_expectancy",
                         "rank_life",
                         "total_cases_per_million",
                         "rank_total_cases",
                         "total_deaths_per_million",
                         "rank_deaths",
                         "total_vaccinations_per_hundred",
                         "rank_vac",
                         "population", 
                         "population_density",
                         "rank_pop_d",
                         "gdp_per_capita",
                         "rank_gdp")


ui <- shinyUI(fluidPage(
    
    tags$head(
        tags$style(HTML("
     # .item {
     #   background: #2196f3 !important;
     #   color: white !important;
     # }
     .selectize-dropdown-content .active {
       background: #2196f3 !important;
       color: white !important;
     }
  "))
    ),
    
    titlePanel("COVID-19: Countries in Context"),
    
    
    fluidRow(
        column(4,
            selectizeInput( inputId = "country_input", label = "Select a country or region:", choices = df_labeled$location, selected = "Singapore"),
            selectizeInput( inputId = "sx", label = "Select a symptom:", choices = sx_names$value, selected = "Fever"),
            
            
            h3("Explore symptom search frequency, cases, and deaths", align = "center"),

            
            h2(htmlOutput(("testHTML3"), align = "center")), 
            
            #h4(textOutput("column"), align = "center"),
            
            plotlyOutput("sx_plot", height = "800px"),
            
            
            p("Symptom explorer is currently only available for 4 countries: New Zealand, Singapore, the United Kingdom, and the United States"),
            
            
        ),
        column(5,
            
           # h4("Select a country from the dropdown menu or map to update the details and plot below."),
            # h2(htmlOutput("click")),
            h1("World Map of COVID-19 Deaths"),
            
            plotlyOutput("world_map", height = "500px", width = "100%"),
            
            
            
            h1("Country Rankings on COVID-19 Metrics and Selected Metrics"), 
            h3(htmlOutput("testHTML1"), "Positive characteristics ranked at the top of the plot besides Population Density which was arbitrarily assigned for more dense to be at the top."),
           # h4(p("Below are rankings of countries based on different variables. Positive characteristics will be ranked at the top of the plot")),
            #h4(p("Generally positive characteristics will be ranked at the top of the plot, though for some variables this is an abitrary assignment as with population density.")),
            
            plotlyOutput("pc_plot", height = "400px", width = "100%"),
            
            ## can put another plot here 2/3 options
            
 
        ), 
        
        column(3,
              #  h3("Symptom Explorer", align = "center"),
              #  
              #  h2(htmlOutput(("testHTML3"), align = "center")), 
              # 
              # #h4(textOutput("column"), align = "center"),
              #  
              #  plotlyOutput("sx_plot"),
              
              h2(htmlOutput(("testHTML2"), align = "center")), 
              htmlOutput("table"),
               
               h3("User Notes", align = "center"), 
               textAreaInput("notes", "Add your notes here", width = "100%", height = "400px"),
               actionButton("submit", "Save note"),
               dataTableOutput("prior_notes"),
               downloadButton("downloadData", "Download"), 

              
              
              h5(uiOutput("tab")),
              h5(uiOutput("tab3")),
              h5(uiOutput("tab2"))
        
              
        ## can put another plot here 3/3 options
    )
)))


server <- shinyServer(function(input, output, session) {
    
    
    ############
    values <- reactiveValues(df = data.frame(country = character(), symptom = character(), note = character()))
    
    
    
    note_data <- eventReactive(input$submit, {
        values$df <- rbind(values$df,
                                         data_frame(country = input$country_input, 
                                                    symptom = input$sx,
                                                    note = input$notes)
        )
    })
    
    
    
    output$prior_notes <- renderDataTable(note_data())
    
        
        
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(Sys.Date(), "_my_shiny_notes", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(note_data(), file)
        }
    )
    
    ############    
    
    
    sx_data <- reactive({
        symptom_df %>% 
            dplyr::filter(year != 2019) %>% 
            dplyr::filter(
                country_region == input$country_input)
            
    })
    
    date_data <- reactive({
        all_dates_owid_data  %>% 
            dplyr::filter(
                location == input$country_input)
        
    })
    
    sx_selected <- reactive({
        sx_name_values %>% 
            dplyr::filter(
                simple == input$sx) %>% pull(complex)
        
        
        
    })
    
    output$column <- renderText({
        sx_selected() 
    } )
    
    output$symptom <- renderText({
        input$sx
    } )
    
    output$sx_plot <- renderPlotly({
        
        y_name <-  sx_selected()
        
        
        
        symptom_plot <- sx_data() %>% 
            rename(y_plot = y_name) %>% 
            ggplot(aes(x = day_of_year, y = (y_plot), group = as_factor(year), color = as_factor(year), text = paste("Date", date, "<br>Count", y_plot, "<br>Year", (year)) )) + 
                       #text = paste("Day of the year", day_of_year, "<br>Frequency", y_plot, "<br>Year", (year)))) +
                  
            geom_line() +
            scale_color_brewer(palette="Dark2") +
            labs(title ="",
                 x = "Day of the Year", 
                 y = "Search Frequency",
                 colour = "Year") 
        
        top <- ggplotly(symptom_plot, 
                 tooltip = "text") 
        
        # %>% 
        #     layout(hovermode = "x unified", 
        #            legend = list(
        #                orientation = "v", x = 0.85, y = .85))
        # 
        

        cases_plot <-  date_data() %>%
            rename(y_plot = new_cases_smoothed_per_million) %>%
            ggplot(aes(x = lubridate::yday(date), y = y_plot, color = as_factor(year)), text = paste("Date", date, "<br>Cases", y_plot, "<br>Year", (year))) +
                        geom_line() +
            scale_color_brewer(palette="Dark2") +
            labs(title = "", y = "Cases per million", x = "Day of the year")

        case <- ggplotly(cases_plot)
        
        deaths_plot <- date_data() %>%
            ggplot(aes(x = lubridate::yday(date), y = new_deaths_smoothed_per_million, color = as_factor(year)),
                   text = paste("Date", date, "<br>Cases", new_deaths_smoothed_per_million, "<br>Year", year)) +
            geom_line() +
            scale_color_brewer(palette="Dark2") +
            labs(title = "", y = "Deaths per million", x = "Day of the year")

        deaths <- ggplotly(deaths_plot)
        
        subplot(list(top, case, deaths), nrows = 3, shareX = TRUE) %>% 
            layout(showlegend = FALSE, xaxis = list(title ="Day of the Year"),
                yaxis = list(title = "Search term Frequency"), yaxis2 = list(title = "Cases per million"), yaxis3 = list(title = "Deaths per million"))
    })
    
    

    
    
    

        
        


    
    output$testHTML1 <- renderText({
        paste("<b>", input$country_input, "</b>", "is highlighted in", "<font color = \"#0000FF\"><b>blue</b></font>", "below. All other countries in light gray.")
    }) 
    
    output$testHTML2 <- renderText({
        paste0("<b>", input$country_input, ": Country COVID-19 Details</b>")
    }) 
    
    output$testHTML3 <- renderText({
        paste0("<b> Searches for ", input$sx, " in ", input$country_input, "</b>")
    }) 
    
    newData <- reactive({
        df_labeled %>% 
            mutate(country_highlight = case_when(
                location %in% c(input$country_input) ~ 3,
                !location %in% c(input$country_input) ~ 1))
    })
    
    output$pc_plot <- renderPlotly({newData() %>% 
            plot_ly(type = 'parcoords', 
                    line = list(color = ~country_highlight, 
                                colorscale = list(c(0,'lightgray'),c(0.5,'green'),c(1,'blue'))),
                    
                    dimensions = list(
                        
                        list(range = c(203,1),
                             # tickvalues = c(1, 0),
                             # ticktext = c("highest", "lowest"),
                             # constraintrange = c(0,100),
                             label = '<b>Life Expectancy</b>', 
                             values = ~rank_life),
                        
                        list( range = c(1,203),
                              #constraintrange = c(0,1),
                              label = '<b>Total Cases per capita</b>', 
                              values = ~rank_total_cases),
                        
                        list( range = c(1,203),
                              #constraintrange = c(0,1),
                              label = '<b>Deaths per capita</b>', 
                              values = ~rank_deaths),
                        
                        list( range = c(203,1),
                              #constraintrange = c(0,1),
                              label = '<b>Vaccinations per captia</b>', 
                              values = ~rank_vac),
                        
                        list(range = c(203,1),
                             #constraintrange = c(0,1),
                             label = '<b>Population Density</b>', 
                             values = ~rank_pop_d),
                        
                        
                        list( range = c(203,1),
                              label = '<b>GDP per          <br>Capita     <b>', 
                              values = ~rank_gdp)
                    ))
    })
    
    
    output$table <- renderText({
        newData() %>%  
            dplyr::filter(country_highlight == 3) %>% 
            dplyr::select(details_of_interest) %>% 
            
            pivot_longer(cols = everything(), names_to = "metrics", values_to = "Details") %>% 
            mutate("Country Details" = round(Details, digits = 0)) %>% 
            mutate("Selected Metrics" = test_column) %>% 
            mutate("Units" = units) %>% 
            select("Selected Metrics", 
                  # metrics, 
                   "Country Details", 
                  "Units") %>% 
            
            kable() %>% 
            kable_styling(
                font_size = 15,
                bootstrap_options = c("striped", "hover", "condensed")
            )
    })
    
    output$world_map <- renderPlotly({
        test_scope <- "world"
        
        
        
        world_map <- newData() %>% 
            plot_ly(type = "choropleth", source = "world_map") %>% 
            add_trace(

z = ~round(total_deaths_per_million, 0),
color = ~total_deaths_per_million,
                colorscale = "Reds", #reversescale = TRUE,
                text = ~rank_deaths,
                key = ~location,
                
                hovertemplate = paste0(
                                        '<i>Deaths per Million</i>: %{z}',
                                        '<br><b>Rank of Deaths</b>: %{text}<br>',
                                        '<b>Country: %{location}</b>'
                ),
                locations = ~iso_code, 
                marker = list(line = list(color = toRGB("black"), width = 0.5))) %>% 
            colorbar(title = "<b>Total Deaths per million") %>% 
            layout(title = 'Reported Deaths Per Million by April 15, 2021', 
                   geo = list(
                       showframe = FALSE,
                       showcoastlines = TRUE,
                       scope = test_scope,
                       projection = list(type = 'natural earth', scale = 1.1)           ) )
        
        world_map %>%   event_register(event = "plotly_click")
        
        
    })
    
    
    
    
    observeEvent(event_data("plotly_click", source = "world_map"), {
        
        event_data <- event_data("plotly_click", source = "world_map")
        
        updateVarSelectizeInput(session, "country_input", selected = event_data[[4]])
        
    })

    
    
   
    
    url <- a("Our World in Data COVID-19 Data", href="https://github.com/owid/covid-19-data/tree/master/public/data")
    output$tab <- renderUI({
        tagList("Link to OWID data:", url)
    })
    
    url2 <- a("Mara Alexeev and Abbie Cheng's Process Book", href="https://maraalexeev.github.io/706_group_covid_project/")
    output$tab2 <- renderUI({
        tagList("Link to our process book:", url2)
    })

    url3 <- a("COVID-19 Symptoms Search Trends", href="https://pair-code.github.io/covid19_symptom_dataset")
    output$tab3 <- renderUI({
        tagList("Link to Google data:", url3)
    })
        
        

        
})


shinyApp(ui = ui, server = server)
