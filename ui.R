library(shiny)
library(jsonlite)
library(dplyr)
library(curl)
library(shinythemes)
library(markdown)
source("functions.R")

shinyUI(navbarPage(strong("School Idol Tomodachi Team Builder"),
                   theme = "bootstrap.min.css",
                   windowTitle = "School Idol Tomodachi Team Builder",
                   collapsible = TRUE,
                   position = "fixed-top",
                   footer = includeHTML('donations.html'),
  
  tabPanel("Home", fluidRow(
    tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
    # Left Sidebar of width 3
    column(3,
           wellPanel(
             textInput("username", "School Idol Tomodachi Username:", value = ""),
             actionButton("fetchAccounts", "Fetch Accounts"),
             width = 3
             ),
           
           conditionalPanel(
             condition = "input.fetchAccounts != 0",
             wellPanel(
               uiOutput("accountid"),
               selectInput("songAttribute", "Song Attribute", c("Smile", "Pure", "Cool")),
               selectInput("songDifficulty", "Song Difficulty", c("Easy", "Normal", "Hard", "Expert", "Master"), selected = "Expert"),
               uiOutput("songlist"),
               sliderInput("percentPerfect", "% PERFECTs", 0, 100, value = 80, step = 1),
               selectInput("teamType", "Type of Team", c("Scorer", "Perfect-Lock (Maximize Score)", "Perfect-Lock (Maximize PLock)", "Healer (Maximize Score)", "Healer (Maximize Healing)")),
               actionButton("createTeam", strong("☆ BUILD TEAM ☆")))
             )
           ),
    
    # Right Main Display of width 9
    column(9,
           tabsetPanel(type = "tabs",
                       selected = "Team Builder",
                       tabPanel(div(strong("Important:"), " Regarding 4.0"), includeMarkdown("regarding4.md")),
                       tabPanel("Team Builder", uiOutput("final"), textOutput("guestCenter")),
                       tabPanel("Team Stats", dataTableOutput("rawdata")),
                       tabPanel("All Stats", dataTableOutput("rawcards"))),
           HTML('<br><br><br><br>')
           )
    
    
    
    )),
  
  tabPanel("Usage Guide", includeHTML("usageguide.html")),
  
  tabPanel("About", includeHTML("about.html")),
  
  tabPanel("Contact", includeHTML("contact.html")),
  
  tabPanel("Change Log", pre(includeText("changelog.txt")))
  
  
))