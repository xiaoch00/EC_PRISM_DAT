


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(ggplot2)
library(googlesheets4)
library(DT)
library(dplyr)
library(shinydashboard)
library(shinythemes)
library(nortest)
#library(tseries)
#library(RcmdrMisc)
#library(lmtest)

fieldsMandatory <- c("ID","Child_race","Gender")



labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


appCSS <- ".mandatory_star { color: red; }
   #error { color: red; }"



fieldsAll <- c("ID", "Complete", "Dropout","Dropout_reason","Outcome1_Pre", "Outcome1_Post", "ACE_Total_Score","Mod_1_Score","Child_age_months",
               "Gender","Child_race","Caregiver_education", "Caregiver_race", "Caregiver_employment", "annualIncome")


loadData <- function () {
  data <- read.csv(
    url("https://docs.google.com/spreadsheets/d/e/2PACX-1vTCncbzNfJinl-4NzOwD6MiYrp238BEZqbLcXhnI-js5tbu2-v8-7XArXqL3pzhRyE-sprkdjdIJAnJ/pub?gid=0&single=true&output=csv"),
    strip.white = TRUE
  )
  data
  
}

newData <- loadData()

ui <- fluidPage(theme = shinytheme("cerulean"),
                  
                  titlePanel("UO Suggested Analyses IMPACT Evaluation Tool"),
                  navbarPage("Get started",
                             tabPanel(icon("home"),
                                      
                                      fluidRow(column(tags$img(src="Antioquia.png",width="200px",height="260px"),width=4),
                                               column(
                                                 
                                                 br(),
                                                 p("This tool was created in order to assist projects in analyzing data after a round of implementation and evaluation. The goal is for teams to begin to answer their Theory of Impact-based research questions during feasibility and early stage pilot tests. As such, the following tabs will aid in answering questions such as: Is this program feasible and acceptable? Were you able to recruit and retain participants? Did you reach the population you were hoping to reach? Is there evidence to suggest that your targets are changing in the anticipated direction? This template was built with a focus on the EC PRISM Demographics Form, BRIEF A, ACEs-Adult, and EC PRISM Participant Tracking Form). There is also space for inclusion of up to 10 project-specific targets and/or outcomes of interest, based on your Theory of IMPACT.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                 br(),
                                                 
                                                 p("In addition to the reflection questions outlined throughout this form, we encourage your team to consider lessons learned related to the data collection and evaluation process. For example, What was the doability of the data collection process? Did your interventionists and participants find the completion of assessment doable? What, if anything, did you learn about the collection of sensitive information, such as the ACEs? How did you handle any situations that arose from asking these questionnaires? How did you support data collectors throughout this project? Whas this successful? Is there anything you would change about your data collection processes moving forward? These questions are critical for informing your work in future iterations.",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                                                 br(),
                                                 p("On the following tabs you'll find both data entry templates, as well as graphs automatically generated from the entered data. The data entry templates are color-coded to indicate which columns are being used in the generation of graphs on subsequent tabs (Green=Generated Graphs, Blue=Additional Information). You'll also notice that the tabs are broken up by category, including demographics, Target/Outcome Change Graphs, as well as participant tracking. On the Target/Outcome Change Graphs, there are also reflection questions related to that topic that are designed to guide the reflection process of the generated information.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                                 br(),
                                                 p("We hope that this tool is helpful toward guiding your team in answering the following question: What did you learn in order to propel the work forward in future rounds? Please reach out to Anna Wright at awright6@uoregon.edu or Tyson Barker tysonb@uoregon.edu if you have any questions, or would like to share feedback about this tool.",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                                                 br(),
                                                 
                                                 width=8),
                                               ),
                                      
        
                                      hr(),
      
                                      p(em("Developed by"), br("Stress Neurobiology and Prevention (SNAP) Research Laboratory"),
          
                                        a(href="https://snaplab.uoregon.edu", "Website",target="_blank"),style="text-align:center;color:black")
                             
                             ),
                             tabPanel("Data Entry",
                                      shinyjs::useShinyjs(),
                                      shinyjs::inlineCSS(appCSS),
                                      fluidRow(column(width=2),
                                               column(
                                                 h4(p("Please select your program",style="color:black;text-align:center")),
                                                 width=8,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      hr(),
                                      
                                      sidebarLayout(
                                        sidebarPanel(
                                          div(
                                            id = "form",
                                            textInput("ID", labelMandatory("Participant ID Number"), ""),
                                            checkboxInput("Complete", "Completed Intervention", FALSE),
                                            selectInput("Dropout", "Opt-Out or Drop-Out?",
                                                        c("",  "Opt-Out", "Drop Out")),
                                            
                                            selectInput("Dropout_reason", "Reasons for Opt-Out and Drop-Out",
                                                        c("",  "Time Commitment", "Didn't Need the Help", "Already Involved in too many other things", "Too stressed","Transportation to Site", "Not a good fit for the family", "Cultural or language concerns", "Privacy concerns", "Out of comfort zone", "Other")),
                                            #       textInput("Dropout_other", "If other, please specify:", ""),
                                            #   dateInput("Dropout_date", "Date of Opt-Out or Drop-Out", value = NULL, min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", width = NULL),            
                                            textInput("Outcome1_Pre", "Target/Outcome 1 Pre Test Score", ""),
                                            textInput("Outcome1_Post", "Target/Outcome 1 Post Test Score", ""),
                                            textInput("ACE_Total_Score", "ACEs Total Score", ""),
                                            textInput("Mod_1_Score", "Moderator 1 Score", ""),
                                            sliderInput("Child_age_months", "Child's Age in Months", 0, 72, 1, ticks = FALSE),
                                            selectInput("Gender", labelMandatory("Child's Gender"),
                                                        c("",  "Female", "Male", "Other")),
                                            selectInput("Child_race", labelMandatory("Child's Race"),
                                                        c("",  "Caucasian/White", "African American", "Asian", "Hawaiian/Pacific Islander","Other")),
                                            selectInput("Caregiver_education", "Caregiver's Highest Education",
                                                        c("",  "Less than Highschool", "Highschool Diploma/GED", "Community College", "Associates Degree","Bachelors Degree", "Graduate Degree", "Other")),
                                            selectInput("Caregiver_race", "Caregiver's Race/Ethnicity",
                                                        c("",  "Caucasian/White", "African American", "Asian", "Hawaiian/Pacific Islander","Other")),
                                            selectInput("Caregiver_employment", "Which of the following best describes the caregiver's current employment status?",
                                                        c("",  "Working full time", "Working part time", "Unemployed or laid off, seeking employment", "Unemployed or laid off, not seeking employment","Keeping house or raising children full-time", "Retired", "Full-time student", "Other")),
                                            selectInput("annualIncome", "To your knowledge, what is your total annual household income from all sources?",
                                                        c("", "No Income",  "Less than $5,000", "$5,000 to $14,999", "$15,000 to $29,999", "$30,000 to $44,999","$45,000 to $59,999", "$60,000 to $74,999", "Greater than $75,000")),
                                            
                                            actionButton("submit", "Submit", class = "btn-primary"),
                                            shinyjs::hidden(
                                              span(id = "submit_msg", "Submitting..."),
                                              div(id = "error",
                                                  div(br(), tags$b("Error: "), span(id = "error_msg"))
                                              )
                                            )
                                          ),
                                          
                                          shinyjs::hidden(
                                            div(
                                              id = "thankyou_msg",
                                              h3("Thanks, your response was submitted successfully!"),
                                              actionLink("submit_another", "Submit another response")
                                            )
                                          ),
                                          width = 3
                                          
                                        ),
                                        mainPanel("",
                                                  tags$style(".glyphicon-folder-open {color:#E87722}"),
                                                  h3(p(em("Previous Records  "),icon("folder-open",lib = "glyphicon"),style="color:black;text-align:center")),
                                                  br(),
                                                  fluidRow(
                            
                                                    column(DT::dataTableOutput("responsesTable"),
                                                                  width = 12)),
                                                  
                                                  width = 9
                                        )),
                                      
                             ),
                             
                             tabPanel("Data Presentation",
                                      
                                      fluidRow(column(width=2),
                                               column(
                                                 h4(p("Normality",style="color:black;text-align:center")),
                                                 width=8,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                                               column(
                                                 p("In order to make inferences about the results of this modeling process, it is necessary to establish 
                                     distributional assumptions, and to make things easier we are going to assume that the response (dependent) 
                                     variable is normally distributed; following this assumption, we will try to achieve this:",style="color:black;text-align:justify"),
                                                 withMathJax(),
                                                 p('$$H_0:~Y ~ \\sim ~ Normal( ~\\mu ~,~ \\sigma~ )$$',style="color:black;border:1px solid black;background-color:white"),
                                                 p("In our case we will take as a response variable", strong(em("Personal injuries")), "since it represents a big 
                                    safety problem where the physical integrity of the people is threatened. We will try to explain this 
                                    variable through education issues, others related to sports and even through other safety problems. All of these,
                                    represented through the other variables in the dataset",style="color:black;text-align:justify"),
                                                 width=8,style="background-color:lavender;border-radius: 10px")
                                      ),
                                      br(),
                                      fluidRow(column(width=2),
                                               column(
                                                 p("Let's do it. You are going to find some graphical and analytical tests in order to conclude about the previous hypothesis",style="color:black;text-align:center"),
                                                 width=8,style="background-color:papayawhip;border-radius: 10px")
                                      ),
                                      hr(),
                                      tags$style(".fa-chart-pie {color:#E87722}"),
                                      h3(p(em("Graphical tests "),icon("chart-pie",lib = "font-awesome"),style="color:black;text-align:center")),
                                      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: coral; border-top: 1px coral; border-bottom: 1px coral;border-left: 1px coral}")),
                                      tags$style(HTML(".js-irs-0 .irs-max, .js-irs-0 .irs-min {background:papayawhip}")),
                                      
                                      br(),
                                      sidebarLayout(
                                        sidebarPanel(
                                          
                                          sliderInput("Transformacion",p("Try power transformations to achieve the normality of",em("Personal injuries"),style="color:black;text-align:center"),
                                                      value=1,
                                                      min=-3,
                                                      max=3,
                                                      step=0.01),
                                          br(),
                                          
                                          p("Remember we are looking for this (click on the next image to view it in a new tab)",style="color:black;text-align:center"),
                                          br(),
                                          a(href="https://drive.google.com/file/d/1eXf5FHKwMIt5aW--64eaB0_UArB_N-v4/view?usp=sharing", tags$img(src="collage.png",width="380px",height="130px",style="border:1px solid black"),
                                            target="_blank"),
                                          br(),
                                          br(),
                                          tags$style(".fa-wikipedia-w {color:black}"),
                                          p("Read more about normal distribution here → ", a(href="https://en.wikipedia.org/wiki/Normal_distribution", icon("wikipedia-w"),target="_blank"),style="color:black;text-align:center")
                                          
                                          
                                          
                                        ),
                                        mainPanel(
                                          
                                          fluidRow(
                                            column(br(),plotOutput("Histograma"),br(),width=4,style="border:1px solid black"),
                                            column(br(),plotOutput("Boxplot"),br(),width=4,style="border: 1px solid black;border-left: none"),
                                            column(br(),plotOutput("qqPlot"),br(),width=4,style="border:1px solid black;border-left:none")
                                            
                                          )
                                        )),
                                      hr(),
                                      tags$style(".glyphicon-folder-open {color:#E87722}"),
                                      h3(p(em("Analytical tests  "),icon("folder-open",lib = "glyphicon"),style="color:black;text-align:center")),
                                      br(),
                                      sidebarLayout(
                                        
                                        sidebarPanel(
                                          
                                          selectInput("PruebaAnalitica",p("Please select the test you want to try:",style="color:black; text-align:center"),choices=c("Shapiro-Wilk"=1,"Anderson-Darling"=2,"Cramér-von Mises"=3,"Kolmogorov-Smirnov"=4,"Jarque-Bera"=5)),
                                          uiOutput("ReadMore")
                                        ),
                                        mainPanel(
                                          
                                          fluidRow(
                                            
                                            tags$head(tags$style("#Conclusion1{color: navy;
                                                      font-size: 15px;
                                                             font-style: italic;
                                                             font-weight: bold;
                                                             text-align: center
                                                             }")),
                                            tags$head(tags$style("#Prueba{height: 155px; border: 1px solid black; background-color: lavender}")),
                                            column(verbatimTextOutput("Prueba"),
                                                   br(),width = 6),
                                            column(br(),
                                                   p("Remember we are looking for a p-value greater than 0.05 (for a confidence level of 95%), so:",style="color:black"),
                                                   br(),
                                                   textOutput("Conclusion1"),
                                                   br(),width = 6,style="background-color:lavender;border-left:8px solid blue")
                                            
                                          )
                                        ))
                             )
                             
                  )
)

                                      
server <- function(input, output, session) {
  output$responsesTable <- DT::renderDataTable({
    newData
  },

  options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=50,scrollX=TRUE,scrollCollapse=TRUE,
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                   "}"),
                 columnDefs=list(list(className='dt-center',targets="_all"))
  ),
  filter = "top",
  selection = 'multiple',
  style = 'bootstrap',
  class = 'cell-border stripe',
  rownames = FALSE,
  colnames = c("ID",	"Complete",	"Dropout",	"Dropout_reason",	"Outcome1_Pre",	"Outcome1_Post",	"ACE_Total_Score",	"Mod_1_Score",	"Child_age_months",	"Gender",	"Child_race",	"Caregiver_education",	"Caregiver_race",	"Caregiver_employment",	"annualIncome")
  ) 
  
}      


# Run the application 
shinyApp(ui = ui, server = server)
