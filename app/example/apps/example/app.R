####TITLE: Reddit shiny app example####
#CREATION DATE: 20-01-2019
#AUTHOR: Carlo R. M. A. Santagiustina
#CONTACT: carlo.santagiustina@unive.it

# This is a Shiny web application. You can run direclty in rStudio the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

####PACKAGES####

require(shiny)
require(jsonlite)#used for data export import
require(httr)
require(tidyverse)
require(readr)
require(shinyjs)
require(DT)
require(openxlsx)
require(rtweet)#used for time series graph
require(shinythemes) #used for choosing the theme
require(ggplot2)
require(rreddit) #used for function to download dat from rreddit
#require(promises) #used for lounching the exampleAPI automatically at start
require(future)
# future::plan(multiprocess)
# require(plumber)


####FUNCTIONS####


# function to handle POST request to exampleAPI
# endpoint: /searchsubmissions_simplified
run_searchsubmissions_simplified = function (URL = "http://127.0.0.1",
              port = "5685",
              endpoint = "searchsubmissions_simplified",
              n = 1000,
              query)
    {
        response =   httr::POST(
            paste(URL, ":", port, "/", endpoint, sep  = ""),
            encode = "json",
            body = list("n" = n,
                        "query" = query)
        )
        header = httr::headers(response)
        body = fromJSON(httr::content(response)[[1]])
        return(list("header" = header,
                    "body" = body))
    }

#function to write json files
write_json = function(df,
                      file,
                      df_type = "rows",
                      raw_type = "mongo",
                      digits = NA) {
    require(readr)
    require(jsonlite)
    df %>%
        jsonlite::toJSON(
            dataframe = df_type,
            raw = raw_type,
            digits = digits,
            pretty = TRUE
        ) %>%
        readr::write_lines(file)
}

####USER INTERFACE####
# Define UI for application that draws a histogram
ui = fluidPage(
    useShinyjs(),
    theme = shinytheme("journal"),
    navbarPage(
        "Shiny App Example: Reddit Pipeline",
        id = "RE_tabs",
        tabPanel("Download data",
                 fluidPage(
                     sidebarLayout(
                         ####Start 1° level UI tab: Download data####
                         sidebarPanel(
                             actionButton("RE_submission_update",
                                          "POST request to APIexample",
                                          class = "btn-primary"),
                             br(),
                             HTML("Remember to run locally the APIexample.R") ,
                             br(),
                             HTML("The API should listen on port 5685"),
                             br(),
                             HTML(
                                 "You can do this directly from rStudio by sourcing as local job the script runAPIexample.R"
                             ),
                             HTML("<br><br>"),
                             selectInput(
                                 "RE_submission_q",
                                 "Subreddit: (dropdown menu example)",
                                 c("World News" = "worldnews",
                                   "Politics" = "politics")
                             ),
                             br()
                             ,
                             sliderInput(
                                 inputId = "RE_submission_n",
                                 "Number of submissions: (slider example)",
                                 min = 1000,
                                 max = 10000,
                                 value = 1000,
                                 step = 1000
                             )
                         ),
                         mainPanel(
                             verbatimTextOutput("RE_submission_observationsCount")
                             ,
                             br()
                             ,
                             div(
                                 style = "display:inline-block",
                                 downloadButton("RE_submission_downloadData", "Download CSV"),
                                 width = 6
                             ),
                             div(
                                 style = "display:inline-block",
                                 downloadButton("RE_submission_downloadDataExcel", "Download Excel")
                             ),
                             div(
                                 style = "display:inline-block",
                                 downloadButton("RE_submission_downloadDataJSON", "Download JSON")
                             ),
                             br()
                             ,
                             DT::DTOutput("RE_submission_rendered_observations")
                             
                         )
                     )
                 ))
        ####End 1° level UI tab: Download data####
        ,
        ####Start 1° level UI tab: Analyse data####
        tabPanel("Analyse data",
                 fluidPage(
                     ####Start 2° level UI tab: type of analysis  ####
                     navbarPage(
                         "TYPE OF ANALYSIS:",
                         ####Start 3° level UI tab: stats by variable ####
                         tabPanel("STATS BY VARIABLE"
                                  ,
                                  sidebarLayout(
                                      # Sidebar with a slider and selection inputs
                                      sidebarPanel(uiOutput('RE_submission_summary_select'))
                                      ,
                                      mainPanel(navbarPage(
                                          "OUTPUTS:",
                                          tabPanel(
                                              "SUMMARY",
                                              strong("Summary Statistics"),
                                              br(),
                                              htmlOutput('RE_submission_summary'),
                                              br(),
                                              strong("Most Frequently Observed Values"),
                                              br(),
                                              DT::DTOutput('RE_submission_table')
                                          )
                                      ))
                                  )) ####End 3° level UI tab: stats by variable ####
                         ,
                         ####Start 3° level UI tab: time series ####
                         tabPanel(
                             "TIME SERIES",
                             useShinyjs()
                             ,
                             sidebarLayout(
                                 # Sidebar with a slider and selection inputs
                                 sidebarPanel(
                                     textInput(
                                         inputId = "RE_submission_timeseries_breaks",
                                         "frequency of breaks for aggregation",
                                         value = "hours"
                                     ),
                                     br(),
                                     HTML("you can choose as frequency mins, hours, days, weeks, months, etc.."),
                                     HTML("<br><br><br>"),
                                     numericInput(
                                         "RE_submission_timeseries_filter",
                                         "Remove groups with less than this number of observations",
                                         "25",
                                         step = 1
                                     ),
                                     uiOutput(outputId = "RE_submission_timeseries_group_by")
                                     
                                 )
                                 ,
                                 mainPanel(navbarPage(
                                     "PLOTS:",
                                     tabPanel(
                                         "N. REDDIT SUBMISSIONS",
                                         plotOutput("RE_submission_timeseries")
                                     ),
                                     tabPanel(
                                         "N. REDDIT SUBMISSIONS GROUPED BY",
                                         plotOutput("RE_submission_timeseries_grouped")
                                     )
                                 ))
                             )
                         )####End 3° level UI tab: time series ####
                     )####End 2° level UI tab: type of analysis  ####
                 ))####End 1° level UI tab: Analyse data####
    )
)


####SERVER####
server = function(input, output) {
    #####options at initialisation####
    
    ##disable specific commands and options at start and run computationally expensive tasts and local APIs
    {
        disable("RE_submission_downloadData")
        disable("RE_submission_downloadDataJSON")
        disable("RE_submission_downloadDataExcel")
        hideTab(inputId = "RE_tabs", target = "Analyse data")
        
            # isolate({
            #     RtweeetAPI = plumber::plumb(file= "../../apis/example/APIexample.R")  # Where the file describing the APIexample.R is file is located
            #     future::future(RtweeetAPI$run(port=5685,host="127.0.0.1", swagger=TRUE))#run exampleAPI in another R session
            #     # do other computationally expensive stuff here
            # })
        
    }
    
    
    
    ####search for reddit submissions by calling exampleAPI at local ip#####
    RE_submission_observations = eventReactive(input$RE_submission_update, {
        withProgress({
            setProgress(message = "Gathering reddit submissions...")
            run_searchsubmissions_simplified(
                URL = "http://127.0.0.1",
                port = "5685",
                endpoint = "searchsubmissions_simplified",
                query = input$RE_submission_q,
                n = input$RE_submission_n
            )[["body"]][["data"]]
        })
    })
    
    output$RE_submission_rendered_observations = DT::renderDT(
        as.data.frame(RE_submission_observations()),
        options = list(
            lengthChange = FALSE,
            searching = FALSE,
            pageLength = 15
        )
    )
    
    output$RE_submission_observationsCount = shiny::renderText({
        RE_submission_df = RE_submission_observations()
        enable("RE_submission_downloadData")
        enable("RE_submission_downloadDataJSON")
        enable("RE_submission_downloadDataExcel")
        showTab(
            inputId = "RE_tabs",
            target = "Analyse data",
            select = FALSE,
            session = getDefaultReactiveDomain()
        )
        paste("Number of Reddit Submissions Found: ",
              as.character(nrow(RE_submission_df)))
    })
    
    ####Download data from app reddit submissions####
    ####csv####
    output$RE_submission_downloadData <- downloadHandler(
        # Create the download file name
        filename  = function() {
            paste(
                "reddit_submissions_data ",
                input$RE_submission_q,
                
                " ",
                Sys.time(),
                ".csv",
                sep = ""
            )
        },
        content = function(file) {
            rtweet::write_as_csv(
                RE_submission_observations(),
                file_name = file,
                na = "",
                prepend_ids = TRUE,
                fileEncoding = "UTF-8"
            )                     # put Data() into the download file
        }
    )
    ####excel####
    output$RE_submission_downloadDataExcel <- downloadHandler(
        # Create the download file name
        filename  = function() {
            paste(
                "reddit_submissions_data ",
                input$RE_submission_q,
                
                " ",
                Sys.time(),
                ".xlsx",
                sep = ""
            )
        },
        content = function(file) {
            openxlsx::write.xlsx(
                rtweet::flatten(RE_submission_observations()),
                file = file,
                sheetName = "raw submissions",
                colNames = TRUE,
                rowNames = FALSE,
                borders = "surrounding",
                colWidths = "auto",
                overwrite = FALSE,
                keepNA = FALSE,
                zoom = 50,
                creator = "CarloWebDataApp"
            )                     # put Data() into the download file
        }
    )
    ####json####
    output$RE_submission_downloadDataJSON <- downloadHandler(
        # Create the download file name
        filename  = function() {
            paste(
                "reddit_submissions_data ",
                input$RE_submission_q,
                
                " ",
                Sys.time(),
                ".json",
                sep = ""
            )
        },
        content = function(file) {
            write_json(RE_submission_observations(), file = file)                     # put Data() into the download file
        }
    )
    
    #### preliminary Analyse data ####
    ##interactive UI elements
    {
        output$RE_submission_summary_select <- renderUI({
            df <- RE_submission_observations()
            selectInput("RE_submission_summary_variable",
                        "Variable:",
                        choices = names(df))
        })
        
        output$RE_submission_timeseries_group_by <- renderUI({
            df = RE_submission_observations()
            selectInput(
                inputId = "RE_submission_timeseries_variable",
                label = "Group by variable",
                choices = names(df),
                selected = "domain"
            )
        })
        
    }
    ####render summary####
    output$RE_submission_table = DT::renderDT({
        RE_submission_observations() %>% group_by(UQ(as.name(
            input$RE_submission_summary_variable
        ))) %>% select(UQ(as.name(
            input$RE_submission_summary_variable
        ))) %>% summarize(Count = n()) %>%
            mutate(Percent = round((Count / sum(Count) * 100))) %>%
            arrange(desc(Count))
    },
    options = list(
        lengthChange = FALSE,
        searching = TRUE,
        pageLength = 15
    ))
    
    #### time series #####
    output$RE_submission_timeseries = renderPlot({
        df = RE_submission_observations()
        df$created_utc = as.POSIXct(df$created_utc)
        df %>% filter(n() >= input$RE_submission_timeseries_filter) %>%
            rtweet::ts_plot(input$RE_submission_timeseries_breaks) +
            ggplot2::theme_gray(base_family = 'serif') +
            ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold"),
                axis.text.x = element_text(angle = 90, hjust = 2)
            ) +
            ggplot2::scale_x_datetime(date_breaks = input$RE_submission_timeseries_breaks) +
            ggplot2::labs(
                x = NULL,
                y = NULL,
                title = paste("Time series of submission counts", sep = ""),
                subtitle = paste(
                    "Submission counts aggregated at",
                    input$RE_submission_timeseries_breaks,
                    "frequency",
                    sep = " "
                ),
                caption = "\nSource: Reddit data collected from Pushshift's API"
            )
        
    })
    
    output$RE_submission_timeseries_grouped = renderPlot({
        df = RE_submission_observations()
        df$created_utc = as.POSIXct(df$created_utc)
        df %>%
            group_by(UQ(as.name(
                input$RE_submission_timeseries_variable
            )))  %>%
            filter(n() >= input$RE_submission_timeseries_filter) %>%
            rtweet::ts_plot(input$RE_submission_timeseries_breaks) +
            ggplot2::theme_gray(base_family = 'serif') +
            ggplot2::theme(
                plot.title = ggplot2::element_text(face = "bold"),
                axis.text.x = element_text(angle = 90, hjust = 2)
            ) +
            ggplot2::scale_x_datetime(date_breaks = input$RE_submission_timeseries_breaks) +
            ggplot2::labs(
                x = NULL,
                y = NULL,
                title = paste("Time series of reddit submissions counts", sep = ""),
                subtitle = paste(
                    "Submission counts aggregated at",
                    input$RE_submission_timeseries_breaks,
                    "frequency and grouped by",
                    input$RE_submission_timeseries_variable,
                    sep = " "
                ),
                caption = "\nSource: Reddit data collected from Pushshift's API"
            )
        
    })
    
    output$RE_submission_summary = renderPrint(summary(RE_submission_observations()  %>% select(UQ(
        as.name(input$RE_submission_summary_variable)
    )) %>% unnest()))
    
    
}

#####EXECUTION OPTIONS####
shinyApp(ui = ui,
         server = server,
         options = list(height = 800))
