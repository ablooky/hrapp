# HR App
# Author: Alex Looky
# https://github.com/ablooky/hrapp


# Load libraries on shiny server ----
if (FALSE) {
    library(shiny)
    library(shinyWidgets)
    library(shinydashboard)
    library(shinydashboardPlus)
    library(patchwork)
    library(plotly)
    library(xlsx)
    library(config)
    library(png)
    library(shiny)
    library(dplyr)
    library(DT)
    library(data.table)
    library(ggplot2)
    library(tidyr)
    library(rsconnect)
}
for (packages in c(
    'shiny',
    'dplyr',
    'DT',
    'data.table',
    'shinydashboard',
    'ggplot2',
    'tidyr',
    'patchwork',
    'plotly',
    "xlsx"
))
for (package in packages) {
    if (!require(package, character.only = T, quietly = T)) {
        #install.packages(package, repos = "http://cran.us.r-project.org")
        library(package, character.only = T)
    }
}
## load functions
source('helper_functions.R', local = TRUE)

# Custom themes ----
library(fresh)
# Create the theme
mytheme <- create_theme(
    adminlte_color(
        light_blue = color_palette2[1]
    ),
    adminlte_sidebar(
        width = "220px",
        dark_bg = color_palette[3],
        dark_hover_bg = color_palette[1],
        dark_color = color_palette[5]
    ),
    adminlte_global(
       # content_bg = color_palette[1],
        #box_bg = "#D8DEE9", 
        info_box_bg = color_palette[1]
            #"#D8DEE9"
    )
)
# Loading datasets & Global variables ----
color_palette<-get_color_palette()
clients_list <-get_clients_list()
raw_results<-get_raw_data()
profile_dataset<-get_profiles()
participants_list<-profile_dataset[,'Participants']
colnames(raw_results)<-c('Question_number', 'Question_category','Question_desc', participants_list)
participants_sorted<-sort(participants_list,decreasing = FALSE)
profile_revised<-modify_profile(profile_dataset)

questions_table<-raw_results[,1:3]
questions_list<-questions_table$Question_desc
questions_table$question<-paste0('Question_',questions_table$Question_number)
new_questions_list<-c('All Questions',questions_list)
questions_category<-unique(questions_table$Question_category)
#participants_list<-rownames(profile_revised)
#raw_results_complete<-modify_raw_results(raw_results)
new_question_category<-c('All Questions', questions_category)

categories_df<-get_categories_df()
#categories<-get_elements_categories()
#headers_list<-get_headers(categories_df)
categories_desc<-categories_df[,2]
categories<-unique(categories_df[,1])

department_list<-na.omit(dplyr::distinct(data.frame(profile_revised),Department))
department_list<-department_list[,1]

formatted_results<-generate_formatted_results(raw_results)

## Demographics Data and Downloads
demographics_table<-generate_demographics_ds(profile_dataset, formatted_results)
summary_demographics_table<-generate_summary_demographics_ds(demographics_table)
download_demographics_table<-generate_demographics_ds_scored(demographics_table)
summary_demographics_table_scored<-generate_summary_demographics_scored(summary_demographics_table)


# Building UI 
## header ----
header<-shinydashboardPlus::dashboardHeaderPlus(
    title='HR APP',
    enable_rightsidebar = FALSE, 
    tags$li(class = "dropdown",
            tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Libre+Baskerville:400,700|Open+Sans:400,700|Montserrat:400,700');")
            )),
    disable = FALSE, titleWidth  = '220'
)
header$children[[3]]$children[[3]] <-
    tags$h1('SURVEY RESULTS & ANALYSIS',
            align = 'left',
            style = paste0("color: ",color_palette[2],'!important;',
                    #   'background-color: ', color_palette[3],"!important; ",
                           "font-family: 'Open Sans','Libre Baskerville',Montserrat, serif;font-size: 19px;"
                           ))

## sidebar ---- 
sidebar<-dashboardSidebar(width = 220,
                          sidebarMenu(id = 'sidebar',
                                      # style = "position: relative; overflow: visible;",
                                      menuItem(
                                      "Summary Analysis",
                                      tabName = 'summary',
                                      icon = icon('dashboard')
                                          ),
                                      menuItem(
                                          "Detailed Analysis",
                                          tabName = 'detailed_analysis',
                                          icon = icon('chart-bar')
                                              ),
                                      menuItem(
                                          "Demographic Analysis",
                                          tabName = 'demographic_analysis',
                                          icon = icon('th-large')
                                      ),
                                      menuItem(
                                          "Profiles",
                                          tabName = "profiles",
                                          icon = icon('users')
                                      )
                                     # menuItem("Downloads",tabName = "downloads",icon =  icon("download"))
                                      
                          )
)
## body ---- 
downloads_tab<-tabItem(tabName='downloads',
                   tags$div(class='download_tables',
                            DT::dataTableOutput('demographic_analysis_table'),
                            hr(), 
                            DT::dataTableOutput('summary_demographics_analysis_table')
                   )
)
body<-dashboardBody(width = 900, #----
                    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      ),
                    use_theme(mytheme), # <-- use the theme
                      tabItems(
                          tabItem (tabName = "summary",
                                   fluidRow(box(width=6,
                                                title='OVERALL FAVORABILITY',
                                                status='primary',
                                                solidHEADER=TRUE,
                                                plotlyOutput('summarize_favorability_plot',
                                                             height=280)),
                                            box(width=6,
                                                title='SURVEY ENGAGEMENT',
                                                status='primary',
                                                solidHEADER=TRUE,
                                                plotlyOutput('summarize_participation_plot',
                                                           height=280))
                                   ),
                                   fluidRow(box(width=6,
                                                title='LOWEST SCORED QUESTIONS',
                                                status='primary',
                                                solidHeader=TRUE,
                                                #tableOutput('lowest_scores_table')
                                                dataTableOutput('lowest_scores_table')
                                                ),
                                            box(width=6,
                                                title='HIGHEST SCORED QUESTIONS',
                                                status='primary',
                                                solidHeader=TRUE,
                                                dataTableOutput('highest_scores_table')
                                                )
                                    )
                                   ), 
                          tabItem (tabName = "detailed_analysis",
                                   selectInput("selected_question_category", label = h4("View Results by Question Category"), 
                                               choices = new_question_category, 
                                               selected = 1), hr(),
                                   conditionalPanel(condition="input.selected_question_category == 'All Questions'",
                                                    reactableOutput('detailed_questions_table')),
                                   conditionalPanel(condition="input.selected_question_category != 'All Questions'",
                                                    box(width=4,
                                                        plotOutput('detailed_questions_plot', height=200)),
                                                    box(width=8,
                                                        tableOutput('questions_filtered_table'),height=200)
                                                    )
                                   
                                   ), 
                          tabItem (tabName = "demographic_analysis",
                                   selectInput("selected_attribute", label = h4("Select Demographic Attribute"), 
                                               #choices = categories_list, 
                                               choices = categories, 
                                               selected = 1), hr(),
                                   fluidRow(box(width=8, 
                                                title = 'Average Favorability Score Per Question Category and Demographic Attribute', plotOutput('attribute_category_plot',height='100%')),
                                   box(width=4, plotlyOutput('demographic_breakdown_pie'))
                                   #br()
                                   )
                                   #fluidRow(box(width=12, plotOutput('attribute_plot')))
                                   ), 
                          tabItem (tabName = "profiles",
                                   selectizeInput("selected_profile", 
                                                  label = h4("View Participant's Profile"), 
                                                  choices = department_list, 
                                                  selected = 0),
                                  fluidRow(column(11,
                                                  plotOutput('departmental_plot')),
                                           column(1)
                                           )                              
                                  )
                          
                      )
)
                          
## ui ---- 
ui<-shinydashboardPlus::dashboardPagePlus(title='Survey Results', 
                  #controlbar = dashboardControlbar(),
                  skin='blue',
                  header,
                  sidebar,
                  body)

# Server ----
server <- function(input, output, session) {
## Reactive Values ----
selectedQuestionCategory<-reactive({input$selected_question_category})
selectedParticipant<-reactive({input$selected_profile})
selectedDemographicAttribute<-reactive({input$selected_attribute})

## Profile analysis ----
output$participant_name<-renderText(selectedParticipant())
output$departmental_plot<-renderPlot(department_analysis(formatted_results,selectedParticipant(),profile_revised))
output$indivdual_profile_table<-renderTable(profile_revised[selectedParticipant(),],
                                                          colnames=FALSE,
                                                          rownames=TRUE,
                                            border=1
                                                  )
output$participant_category_plot<-renderPlot(participant_category_analysis_chart(formatted_results, selectedParticipant()))
output$participant_category_table<-renderDataTable(participant_category_analysis(formatted_results, selectedParticipant()),
                                                   caption='Favorability Scores Breakdown',
                                                   options=list(
                                                       pageLength = 10,
                                                       dom='t'
                                                   ))
output$participant_detailed_chart<-renderPlot(participant_detailed_favorability(formatted_results, selectedParticipant()))

## Detailed analysis ----
#output$detailed_questions_plot<-renderPlot(get_detailed_analysis(convert_scores(formatted_results),selectedQuestionCategory()))
output$detailed_questions_table<-renderReactable(get_detailed_analysis_2(formatted_results,selectedQuestionCategory()))
output$detailed_questions_plot<-renderPlot(get_detailed_analysis_2(formatted_results,selectedQuestionCategory()))
output$questions_filtered_table<-renderTable(get_questions(selectedQuestionCategory()),
                                             rownames=FALSE, colnames=FALSE)

## Summary Analysis ----
results_scored_by_questions<-generate_scores_per_question(formatted_results)
scored_results<-generate_scored_results(formatted_results)
lowest_scores<-summarize_score(formatted_results,'lowest') 
highest_scores<-summarize_score(formatted_results,'highest')

#output$lowest_scores_table<-renderTable(lowest_scores,colnames=TRUE,spacing='xs')
output$lowest_scores_table<-renderDT(lowest_scores,
                                     rownames = FALSE,
                                     options=list(dom='t'),
                                     style = 'bootstrap', class = 'table-bordered table-condensed')
output$highest_scores_table<-renderDT(highest_scores,
                                     rownames = FALSE,
                                     options=list(dom='t'),
                                     style = 'bootstrap', class = 'table-bordered table-condensed')
#output$highest_scores_table<-renderTable(highest_scores,colnames=TRUE,spacing='xs')
output$summarize_favorability_plot<-renderPlotly(summarize_favorability(formatted_results))
output$summarize_participation_plot<-renderPlotly(summarize_participation(formatted_results))

## Demographic Analysis ----
plotheight<-reactive({get_number_of_cat_elements(selectedDemographicAttribute())})
output$demographic_breakdown_pie<-renderPlotly(demographics_participant_stats(profile_revised,
                                                                              selectedDemographicAttribute()))
observe({ output$attribute_plot<-renderPlot(analyze_demographics(demographics_table,
                                                        selectedDemographicAttribute(),
                                                       'question'), height= plotheight()*500)})
#output$plot_height<-renderText(get_number_of_cat_elements(selectedDemographicAttribute()))
observe({ output$attribute_category_plot<-renderPlot({analyze_demographics(summary_demographics_table,
                                     selectedDemographicAttribute(),
                                     'category')}, height= plotheight()*500)})
#output$display_questions_table<-renderTable(questions_table[,3], rownames=TRUE,colnames=FALSE,spacing='xs')
## Downloads ----
output$demographic_analysis_table<-DT::renderDT(download_demographics_table,
                                                caption='Survey Analysis',
                                                extensions='Buttons',
                                                rownames = TRUE,
                                                options=list(
                                                    pageLength = 5,
                                                    dom='Bfrtip',
                                                    buttons = c('copy','excel', 'pdf', 'print'),
                                                    scrollX=TRUE))
output$summary_demographics_analysis_table<-DT::renderDT(summary_demographics_table_scored,
                                                         caption='Survey Analysis Per Question Category',
                                                         extensions='Buttons',
                                                         options=list(
                                                             pageLength = 15,
                                                             dom='Bfrtip',
                                                             buttons = c('copy','excel', 'pdf', 'print'),
                                                             scrollX=TRUE))
}
# Run the application  ----
shinyApp(ui = ui, server = server)
