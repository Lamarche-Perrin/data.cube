
html <- '<div class="attr-col shiny-input-radiogroup" id="datasource">
<head>
<meta charset=utf-8 />
<title></title>
</head>
<body>
 <hr>
  <label>
    <input id="twitter" type="radio" name="datasource" value="twitter" />
    <img src="twitter.jpg"  width= 70px>
  </label>
  &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <label>
    <input id="theguardian"  type="radio" name="datasource" value="theguardian"/>
    <img src="The_Guardian.svg.png"  width= 100px>
  </label>
  &nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
  <label>
    <input id="reddit" type="radio" name="datasource" value="reddit" /><img src="Reddit.svg.png" width= 70px>
  </label></div>'


ui <- shiny::fluidPage (
                 useShinyjs (),
                 theme = shinytheme ("journal"),
                 shiny::fluidPage (
                            fluidRow (
                                actionButton (
                                    inputId = "toggle_sources",
                                    label =  "Show/Hide Data Sources",
                                    icon = icon ("cog", lib = "glyphicon")
                                ),
                                div (
                                    id = "sources",
                                    column (
                                        8,
                                        align = "center",
                                        HTML (html)                                        
                                    )
                                )
                            )
                        )
             )

#### CONDITIONAL TAB SET: TWITTER ####
source ("./apps/climate-change/components/twitterUI.R", local = TRUE) [1]

#### CONDITIONAL TABSET: THE GUARDIAN ####
source ("./apps/climate-change/components/theguardianUI.R", local = TRUE) [1]

#### CONDITIONAL TABSET: REDDIT ####
source ("./apps/climate-change/components/redditUI.R", local = TRUE) [1]




panel.data <-
    tabPanel (
        "Data",
        fluidPage (
            useShinyjs (),
            theme = shinytheme ("journal"),
            sidebarLayout (
                sidebarPanel (
                    textInput (
                        inputId = "guardian_query",
                        "Keyword:",
                        value = ".*"
                    ),
                    radioButtons (
                        'guardian_articlesVScomments',
                        label = "Search keyword in articles or comments ",
                        choices = c ( "Find in articles"="articles","Find in comments"="comments"),
                        inline = FALSE
                    ),
                    dateRangeInput (
                        'guardian_date',
                        label = 'Date range input: yyyy-mm-dd',
                        start = "2019-01-01",
                        end = "2019-04-30"
                    ),
                    sliderInput (
                        inputId = "guardian_number",
                        "Number of articles:",
                        min = 10,
                        max = 1000,
                        value = 30,
                        step = 10
                    ),
                    actionButton (
                        "guardian_execute_getrequest",
                        "Start search",
                        class = "btn-primary"
                    )
                ),
                mainPanel (
                    verbatimTextOutput ("guardian_query_for_request"),
                    DT::DTOutput ("guardian_rendered_observations"),
                    jsoneditOutput ( "guardian_output")
                )
            )
        )
    )




panel.exploration.byvariables <-
    tabPanel (
        "BY VARIABLE",
        useShinyjs (),
        sidebarLayout ( # Sidebar with a slider and selection inputs
            sidebarPanel (
                uiOutput ('guardian_summary_select')
            ),
            mainPanel (
                navbarPage (
                    "",
                    tabPanel (
                        "SUMMARY",
                        br (),
                        strong ("Most Frequently Observed Values"),
                        br (),
                        DT::DTOutput ('guardian_table')
                    )
                )
            )
        )
    )


panel.exploration.plots <-
    tabPanel (
        "PLOTS",
        navbarPage (
            "TIME SERIES:",
            id = "guardian_timeseries",
            tabPanel (
                "FILTERED BY",
                fluidPage (
                    sidebarLayout ( # Sidebar with a slider and selection inputs
                        sidebarPanel (
                            textInput (
                                inputId = "guardian_timeseries_breaks",
                                "frequency of breaks for aggregation",
                                value = "mins"
                            ),
                            uiOutput (outputId = "guardian_tags_inputUI"),
                            uiOutput (outputId = "guardian_authors_inputUI")
                        ),
                        mainPanel (
                            verbatimTextOutput ("guardian_filters"),
                            plotOutput ("guardian_timeseries_byfilter")
                        )
                    )
                )
            )
        )
    )



panel.analysis.kwic <-
    tabPanel (
        "KWIC",
        useShinyjs (),
        sidebarLayout ( # Sidebar with a slider and selection inputs
            div (
                id = "guardian_Sidebar2",
                sidebarPanel (
                    selectInput (
                        "guardian_kwic_variable",
                        div (h3 ("KWIC:"), h5 ("Variable")),
                        choices = c (
                            "Articles [text]" = "text",
                            "Comments [description]" = "comments"
                        ),
                        selected = "text"
                    ),
                    textInput (
                        inputId = "guardian_kwic_pattern",
                        h5 ("search keywords context in articles corpus")
                    ),
                    sliderInput (
                        inputId = "guardian_kwic_window",
                        h3 ("(left/right) window size:"),
                        min = 1,
                        max = 20,
                        value = 10,
                        step = 1
                    ),
                    checkboxInput (
                        inputId =  "guardian_kwic_caseinsensitive",
                        "case insensitive pattern matching",
                        value = FALSE
                    )
                )
            ),
            mainPanel (
                navbarPage (
                    "KWIC Outputs:",
                    tabPanel (
                        "Context",
                        DT::DTOutput ("guardian_kwicTable")
                    ),
                    tabPanel (
                        "Most Frequent Preceeding Pattern",
                        DT::DTOutput ("guardian_kwicPreTable")
                    ),
                    tabPanel (
                        "Most Frequent Following Pattern",
                        DT::DTOutput ("guardian_kwicPostTable")
                    )
                )
            )
        )
    )


panel.analysis.terms <-
    tabPanel (
        "TERM FREQ & COOC",
        mainPanel (
            DT::DTOutput ("guardian_articles_top100_freq_DT"),
            br (),
            plotOutput ("guardian_articles_text_cooc_network"),
            br (),
            plotOutput ("guardian_articles_top100_freq_wordcloud")
        )
    )

panel.analysis.causes <-
    tabPanel (
        "CAUSAL RELATIONS",
        useShinyjs (),
        sidebarLayout ( # Sidebar with a slider and selection inputs
            sidebarPanel (
                textInput (
                    inputId = "article_number",
                    "Maximum number of articles:",
                    value = "10"
                ),
                actionButton (
                    "query_causal_relations",
                    "Show Causal Relations",
                    class = "btn-primary"
                )
            ),
            mainPanel (
                DT::DTOutput ("rendered_semantic_frames")
            )
        )
    )




panel.exploration <-
    tabPanel (
        "Exploration",
        fluidPage (
            navbarPage (
                "STATS",
                panel.exploration.byvariables,
                panel.exploration.plots
            )
        )
    )



panel.analysis <-
    tabPanel (
        "Analysis",
        fluidPage (
            navbarPage (
                "TOOLS",
                panel.analysis.kwic,
                panel.analysis.terms,
                panel.analysis.causes
            )
        )
    )


list (
    conditionalPanel (
        condition = "input.datasource == 'theguardian'",
        {
            navbarPage (
                title = div (
                    div (
                        id = "logo_theguardian",
                        img (src = "The_Guardian.svg.png", height = "20px")
                    ),
                    ""
                ),
                id = "guardian_tabs",
                panel.data,
                panel.exploration,
                panel.analysis,
            )
        }
    )
)
