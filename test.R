


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
library(googlesheets)
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


#loaddata from Googlesheet
loadData <- function () {
  data <- read.csv(url("https://docs.google.com/spreadsheets/d/e/2PACX-1vTCncbzNfJinl-4NzOwD6MiYrp238BEZqbLcXhnI-js5tbu2-v8-7XArXqL3pzhRyE-sprkdjdIJAnJ/pub?gid=0&single=true&output=csv"),
    strip.white = TRUE
  )
  data
  
#  data <- sheets_speedread("1J-Z5GJIz1rh1P3XSQZywXqZJ_bOVvhc7kFwGPDhKmoo", sheet = "Test")
  
}

newData <- loadData()

ui <- fluidPage(theme = shinytheme("cerulean"),
                  
                  titlePanel("UO Suggested Analyses IMPACT Evaluation Tool"),
                  navbarPage("Menu",
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
                                      #         htmlOutput("googleSheet")
                                      
                                      h1("Participant Tracking Graphs", align="center",
                                         style ="font-family: 'arial'; font-size: 16pt; color:blue "),
                                      h2("Did participants complete program?  &  Reasons for Opt-Out and Drop-Out", align="center",
                                         style ="font-family: 'arial'; font-size: 12pt; color:blue "),
                                      
                                      fluidRow(  
                                        splitLayout(cellWidths = c("50%", "50%"),plotOutput("pie_completedintervention"),plotOutput("OutReason"))
                                      ),
                                      
                                      h1("Child Characteristics", align="center",
                                         style ="font-family: 'arial'; font-size: 16pt; color:blue "),
                                      h2("Child Race  &  Child Gender", align="center",
                                         style ="font-family: 'arial'; font-size: 12pt; color:blue "),
                                      
                                      fluidRow(  
                                        splitLayout(cellWidths = c("50%", "50%"),plotOutput("pie_childRace"),plotOutput("pie_childGender"))
                                      ),  
                                      h1("Caregiver and Household Characteristics", align="center",
                                         style ="font-family: 'arial'; font-size: 16pt; color:blue "),
                                      h2("Caregiver Race  & Caregiver Education Level", align="center",
                                         style ="font-family: 'arial'; font-size: 12pt; color:blue "),
                                      
                                      #   fluidRow(
                                      #   column(3,
                                      #          tableOutput(s_table)
                                      #        )
                                      # ),
                                      
                                      fluidRow(  
                                        splitLayout(cellWidths = c("50%", "50%"),plotOutput("pie_cgRace"),plotOutput("pie_cgEd"))
                                      ),  
                                      #  fluidRow(  
                                      #       splitLayout(cellWidths = c("50%", "50%"),plotOutput("pie_cgEmployment"),plotOutput("pie_income"))
                                      # ),  
                                      h1("Evaluating Changes in Targets and Outcomes", align="center",
                                         style ="font-family: 'arial'; font-size: 16pt; color:blue "),
                                      h2("Individual Change in Outcome1  &  Average Change in Outcome1", align="center",
                                         style ="font-family: 'arial'; font-size: 12pt; color:blue "),
                                      
                                    #  selectInput("OutcomeSelection", "Please select outcome:",
                                     #             c("Outcome 1",  "Outcome 2", "Outcome 3")),
                                      fluidRow(  
                                        splitLayout(cellWidths = c("50%", "50%"),plotOutput("outcome1_indv"),plotOutput("outcome1_avr"))
                                      ),  
                                      
                                      h2("Change in Outomce1 by ACEs Score  &  Change in Outcome1 by Moderator1", align="center",
                                         style ="font-family: 'arial'; font-size: 12pt; color:blue "),
                                      
                                      fluidRow(  
                                        splitLayout(cellWidths = c("50%", "50%"),plotOutput("outcome1byACEs"),plotOutput("outcome1byMod1"))
                                      ),  
                                      
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
  
  
  
  # Enable the submit button when all mandatory fields are completed
  observe({
    
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  # Gather all the form inputs
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
  })
  
  
  # Save data to Googlesheet
  saveData <- function(data) {
#    sheets_append(data, "1J-Z5GJIz1rh1P3XSQZywXqZJ_bOVvhc7kFwGPDhKmoo", sheet = "Test")
    gs_add_row(ss = gs_key("1J-Z5GJIz1rh1P3XSQZywXqZJ_bOVvhc7kFwGPDhKmoo"), 
               ws = "Test",
               input = data
    )
  }
  
  
  

  
  
  
  observeEvent(input$submit, {
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
      
    })
  })
  
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
    #  update table?
  })  
  
  
  output$pie_completedintervention <- renderPlot({
    D2 <- newData %>% 
      group_by(Complete) %>%
      summarise(n())
    D2$lbs <- NA
    D2$lbs <- ifelse(D2$Complete, "YES", "NO")
    pct <- round(D2$`n()`/sum(D2$`n()`)*100)
    lbls <- paste(D2$lbs, pct, "%")
    pie(D2$`n()`, labels = lbls)
    
  })
  
  output$OutReason <- renderPlot({
    D3 <- newData %>% 
      filter(Dropout != "") 
    
    ggplot(D3, aes(Dropout_reason)) + 
      geom_bar(alpha = 0.7, fill = "#FF6666", width = 0.3) + theme(aspect.ratio = 2/1, axis.title.x = element_blank(), axis.title.y  = element_blank()) + coord_flip() 
  })
  
  output$pie_childRace <- renderPlot({
    D4 <- newData %>% 
      group_by(Child_race) %>%
      summarise(n())
    pct4 <- round(D4$`n()`/sum(D4$`n()`)*100)
    lbls4 <- paste(D4$Child_race, pct4, "%")
    pie(D4$`n()`, labels = lbls4, main = "Child Race")
    
  })
  
  output$pie_childGender <- renderPlot({
    D5 <- newData %>% 
      group_by(Gender) %>%
      summarise(n())
    pct5 <- round(D5$`n()`/sum(D5$`n()`)*100)
    lbls5 <- paste(D5$Gender, pct5, "%")
    pie(D5$`n()`, labels = lbls5, main = "Child Gender")     
  })
  
  output$pie_cgRace <- renderPlot({
    D6 <- newData %>% 
      group_by(Caregiver_race) %>%
      summarise(n())
    ggplot(D6, aes(x="", y=`n()`, fill = factor(Caregiver_race)))+ geom_bar(width = 1, stat = "identity") + 
      geom_text(aes(label = paste(round(`n()`/sum(`n()`)*100,1),"%")), position = position_stack(vjust = 0.5)) +
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())+
      labs(fill = "Category", x = NULL, y=NULL) + coord_polar("y")
    
  })
  
  output$pie_cgEd <- renderPlot({
    D7 <- newData %>% 
      group_by(Caregiver_education) %>%
      summarise(n())
    ggplot(D7, aes(x="", y=`n()`, fill = factor(Caregiver_education)))+ geom_bar(width = 1, stat = "identity") + 
      geom_text(aes(label = paste(round(`n()`/sum(`n()`)*100,1),"%")), position = position_stack(vjust = 0.5)) +
      theme_classic()+
      theme(plot.title = element_text(hjust = 0.5),
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())+
      labs(fill = "Category", x = NULL, y=NULL) + coord_polar("y")
    
  })
  output$outcome1_indv <- renderPlot({
    D8 <- newData %>% 
      filter(Complete) 
    ggplot(D8) + geom_segment( aes(x=1, xend = 2,  y=Outcome1_Pre, yend = Outcome1_Post, color = as.character(ID))) +
      theme_bw()+
      scale_x_discrete(
        breaks = c("1", "2"),
        labels = c("Pre", "Post"),
        limits = c(1,2)
      ) + labs(y = "Outcome1")
    
    
  })
  output$outcome1_avr <- renderPlot({
    D9 <- newData %>% 
      filter(Complete) %>%
      summarise(meanOut1pre = mean(Outcome1_Pre), meanOut1post = mean(Outcome1_Post))
    ggplot(D9) + geom_segment( aes(x=1, xend = 2,  y=meanOut1pre, yend = meanOut1post, color = "FF6666")) +
      theme_bw()+
      scale_x_discrete(
        breaks = c("1", "2"),
        labels = c("Pre", "Post"),
        limits = c(1,2)
      ) + labs(y = "Outcome1")
  })
  
  output$outcome1byACEs <- renderPlot({
    D10_1 <- newData %>% 
      filter(Complete) %>%
      filter(ACE_Total_Score > mean(ACE_Total_Score)) %>%
      summarise(meanOut1prebyACE = mean(Outcome1_Pre), meanOut1postbyACE = mean(Outcome1_Post))
    D10_2 <- newData %>% 
      filter(Complete) %>%
      filter(ACE_Total_Score <= mean(ACE_Total_Score)) %>%
      summarise(meanOut1prebyACE = mean(Outcome1_Pre), meanOut1postbyACE = mean(Outcome1_Post))
    
    D10 <- rbind(D10_2, D10_1)
    D10$ACEs <- c("Above Average", "Below Average")
    ggplot(D10) + geom_segment( aes(x=1, xend = 2,  y=meanOut1prebyACE, yend = meanOut1postbyACE, color = ACEs)) +
      theme_bw()+
      scale_x_discrete(
        breaks = c("1", "2"),
        labels = c("Pre Average", "Post Average"),
        limits = c(1,2)
      ) + labs(y = "Outcome1")
    
  })
  output$outcome1byMod1 <- renderPlot({
    D11 <- newData %>% 
      filter(Complete) %>%
      group_by(Mod_1_Score) %>%
      summarise(meanOut1prebyMod1 = mean(Outcome1_Pre), meanOut1postbyMod1 = mean(Outcome1_Post))
    ggplot(D11) + geom_segment( aes(x=1, xend = 2,  y=meanOut1prebyMod1, yend = meanOut1postbyMod1, color = as.character( Mod_1_Score))) +
      theme_bw()+
      scale_x_discrete(
        breaks = c("1", "2"),
        labels = c("Pre Average", "Post Average"),
        limits = c(1,2)
      ) + labs(y = "Outcome1")
  })
  

  
}      


# Run the application 
shinyApp(ui = ui, server = server)
