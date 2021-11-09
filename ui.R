library(shiny)
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)


# Define UI 
shinyUI(fluidPage(theme = shinytheme("cosmo"),
                  
                  navbarPage("Hockey 101",
                             tabPanel("About",
                                navlistPanel(widths = c(2, 10),
                                     tabPanel("What?",        
                                      fluidRow(
                                        column(12, h4(strong("What is Hockey 101?"))),
                                        column(12, h5("This app provides visualisations on analysing", strong("strategy"), "in", strong("field hockey."),
                                                      "It is intended to be used by any", strong("coach, player, analyst"), "or", strong("fan"), "with a basic to in depth knowledge of hockey.
                                                      Layers of information are provided so that the inquiring mind may stop when the desired level of detail is gained.
                                                      Insight is provided into", strong("how"), "and", strong("where"), "a team moves the ball, the", strong("attacking intent"), "and", strong("opportunities"), "that arise so that each teams", strong("strengths"), "and", strong("weaknesses"), "may be identified.
                                                      This app illustrates", strong("how each team attempts to be successful"), "and gives you the tools to", strong("generate effective strategies"), "to be successful."))),
                                      fluidRow(
                                        br(),
                                        column(2, offset = 1, img(src = "Strategy1.jpeg", style="display: block; margin-left: auto; margin-right: auto;", height = 269.5, width = 180)),
                                        column(2, offset = 1, img(src = "Strategy2.jpeg", style="display: block; margin-left: auto; margin-right: auto;", height = 269.5, width = 180)),
                                        column(2, offset = 1, img(src = "Strategy3.jpeg", style="display: block; margin-left: auto; margin-right: auto;", height = 269.5, width = 180)),
                                      )),
                                    tabPanel("Categories",  
                                     fluidRow(
                                        column(12, h4(strong("Categories of Performance"))),
                                        column(12, h5("Insight is provided into the",  strong("on ball actions, events"), "and", strong("outcomes"), "in relation to the", strong("opposition"),  "and", strong("how"), "and", strong("where"), "a team moves the ball from a", strong("spatial-temporal"), "perspective.
                                                      The code windows used to capture the data are shown below illustrating the variables recorded.")),
                                        column(12, img(src = "GS Code Window.png", style="display: block; margin-left: auto; margin-right: auto;", height = 344.8, width = 522.8),
                                        br(),
                                        br()),
                                        column(12, img(src = "BM Code Window.png", style="display: block; margin-left: auto; margin-right: auto;", height = 207.2, width = 488)),
                                        column(12, h5("Strategy is divided into three categories of performance; game styles, ball movement patterns and in game events:")),
                                        column(12, h5(strong("-Game styles"), "condense the highly detailed in game events into practical identities which reflect the main on ball components of performance that can be altered to create a strategy.
                                                      A game style reflects the consistent strategy used by a team in terms of strength in", strong("attack types, game actions"), "used in different attack types and", strong("tempo"), "of ball movement.
                                                      For example, do teams prefer to pass or dribble, do they create goal scoring opportunities from open play or set pieces, do they play controlled or attacking hockey?")),
                                        column(12, h5(strong("-Ball movement patterns"), "condense the highly detailed ball tracking data into practical identities which reflect the main ways ball movement can be altered to create a strategy.
                                                      These patterns reflect the consistent strategy used by a team in terms of the", strong("spatial distribution"), "of ball possession due to the", strong("unpredictability"), "and", strong("direction"), "of ball movement and the attacking opportunities that arise.
                                                      For example, do teams play fast or slow, do they play forward, backwards or sideways, are their movements predictable?")),
                                        column(12, h5(strong("-In game events"), "reflect game events recorded in relation to", strong("time, space"), "and", strong("opposition"), "to provide specific detail on particular moments within a game.
                                                      For example, do teams use dribbling or passing to gain ground, do they want to engage the defence or stick to space, how close to goal do a team need to recover possession to create a goal scoring opportunity?")))),
                                    tabPanel("How?",  
                                      fluidRow(
                                        column(12, h4(strong("How to use"))),
                                        column(12, h5("This app is split into 3 tabs across the top of the page reflecting the different categories of performance."),
                                               br()),
                                        column(12, img(src = "Tabs.png", style="display: block; margin-left: auto; margin-right: auto;", height = 78, width = 475),
                                               br()),
                                        column(12, h5("By clicking on a tab, a list of sub-categories are presented on the left reflecting the areas analysed."),
                                               br()),
                                        column(12, img(src = "Lists.png", style="display: block; margin-left: auto; margin-right: auto;", height = 210, width = 410),
                                               br()),
                                        column(12, h5("Clicking on a sub-category opens a drop down menu of figures available to be viewed. Click on the desired figure title to open it."),
                                               br()),
                                        column(12, img(src = "Drop down lists.png", style="display: block; margin-left: auto; margin-right: auto;", height = 210, width = 415),
                                               br()),
                                        column(12, h5("For each visualisation, drop down menus are presented to allow an analyst to select the team or context to be analysed."),
                                               br()),
                                        column(12, img(src = "Filter.png", style="display: block; margin-left: auto; margin-right: auto;", height = 319.5, width = 419.5),
                                               br()))),
                                    tabPanel("Interpret",  
                                      fluidRow(
                                        column(12, h4(strong("How to interpret"))),
                                        column(12, h5("This app is built to provide layers of analysis:")), 
                                        column(12, h5("1. The", strong("first layer"), "of analysis is the", strong("holistic game plan"), "which should reflect how you", strong("observe"), "and", strong("communicate strategy."), 
                                                      "Game style and ball movement profiles are presented as practical identities reflecting the consistent strategies teams implement.
                                                      To access Game Style profiles select the Game Style tab and Team Profiles sub-category."),
                                               br()),
                                        column(12, img(src = "GS Profile.png", style="display: block; margin-left: auto; margin-right: auto;", height = 313, width = 550),
                                               br()),
                                        column(12, h5("To access Ball Movement profiles select the Ball Movement tab and Team Profiles sub-category."),
                                               br()),
                                        column(12, img(src = "BM Profile.png", style="display: block; margin-left: auto; margin-right: auto;", height = 366.4, width = 497.6),
                                               br()),
                                        column(12, h5("To identify", strong("common strategies,"), "analyse the", strong("interaction"), "between", strong("game style"), "or", strong("ball movement variables"), "and", strong("attacking"), "and", strong("defensive profiles."),
                                                      "For example, strength in established attack and defence indicates field position and attacking opportunities, and strength in established attack and tempo reflects attacking intent.")),
                                        column(12, h5("2. The", strong("second layer"), "of analysis looks at", strong("differentiating teams"), "with", strong("similar profiles"), "by identifying a teams", strong("strengths"), "and", strong("weaknesses."), 
                                                      "Each game style type and ball movement variable is broken down into its components to illustrate the important facets of each game style type or locations in a movement pattern for each team.
                                                      To access Game Style variables select the Game Style tab and Variables sub-category."),
                                               br()),
                                        column(12, img(src = "GS Variable.png", style="display: block; margin-left: auto; margin-right: auto;", height = 319.67, width = 557.34),
                                               br()),
                                        column(12, h5("To access Ball Movement variables select the Ball Movement tab and Heat Maps sub-category."),
                                               br()),
                                        column(12, img(src = "Heat Maps.png", style="display: block; margin-left: auto; margin-right: auto;", height = 262.4, width = 719.2),
                                               br()),
                                        column(12, h5("3. The", strong("third layer"), "of analysis is the", strong("techno-tactical indicators"), "that provide insight into", strong("how to inhibit"), "or", strong("exploit"), "a teams strengths and weaknesses.
                                                      In game events provide more specific detail on game actions and events so", strong("individual team nuances"), "can be identified.
                                                      Select the In Game Events tab and the sub-category you want to analyse."),
                                               br()),
                                        column(12, img(src = "In Game Events.png", style="display: block; margin-left: auto; margin-right: auto;", height = 296.34, width = 503.34),
                                               br())))
                                   
                                    )),
                             
                             tabPanel("Game Styles", 
                                      navlistPanel(widths = c(2, 10),
                                                   
                                                   tabPanel("Definitions",
                                                            fluidRow(
                                                              column(12, h4(strong("Game Style Types"))),
                                                              column(12, h5(strong("Established and Counter Attack Success:"))),
                                                              column(4, h6("1. Strong"),
                                                                     h6("Greater possession, stoppages and turnovers in attacking half and lower attacks per goal shot")),
                                                              column(4, h6("2. Poor"),
                                                                     h6("Greater possession, stoppages and turnovers in defensive half and greater attacks per goal shot"))),
                                                            
                                                            fluidRow(
                                                              column(12, h5(strong("Established and Counter Attack Game Actions:"))),
                                                              column(4, h6("1. Pass"),
                                                                     h6("Higher proportion of game actions as passes")),
                                                              column(4, h6("2. Dribble"),
                                                                     h6("Higher proportion of game actions as dribbling"))),
                                                            
                                                            fluidRow(
                                                              column(12, h5(strong("Set Piece Occurrence:"))),
                                                              column(4, h6("1. Low"),
                                                                     h6("Low set pieces per game")),
                                                              column(4, h6("2. High"),
                                                                     h6("High set pieces per game"))),
                                                            
                                                            
                                                            fluidRow(
                                                              column(12, h5(strong("Tempo:"))),
                                                              column(4, h6("1. Direct"),
                                                                     h6("Lower game actions per attack and greater number of attacks per game")),
                                                              column(4, h6("2. Possession"),
                                                                     h6("High game actions per attack and lower number of attacks per game")))
                                                   ),
                                                   navbarMenu("Team Profiles",
                                                   tabPanel("Attack Game Styles",
                                                            fluidRow(
                                                              column(12, h4("Attack Game Style Team Profiles"),
                                                                     h5("What attacking strategies does a team use consistently?"),
                                                                     br())),
                                                              
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team1",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))), 
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("agsPlot")))),
                                                   
                                                   tabPanel("Defence Game Styles",
                                                            fluidRow(
                                                              column(12, h4("Defence Game Style Team Profiles"),
                                                                     h5("What defensive strategies does a team use consistently?"),
                                                                     br())),
                                                             
                                                            
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team2",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))), 
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("dgsPlot"))))),
                                                   navbarMenu("Outcomes",
                                                   tabPanel("Match Outcomes",
                                                            fluidRow(
                                                              column(12, h4("Game Styles and Match Outcomes"),
                                                                     h5("What strategies did a team use in each match and were they successful?"),
                                                                     br())),
                                                              
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team3",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected")))),
                                                              column(4, h6(selectInput(inputId = "ms3",
                                                                                       label = "Choose the match status you want to analyse:",
                                                                                       choices = c("Winning" = "Winning", 
                                                                                                   "Losing" = "Losing",
                                                                                                   "Drawing" = "Drawing"), 
                                                                                       selected = "Drawing")))),
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("moPlot")))),
                                                   
                                                   tabPanel("Goals",
                                                            fluidRow(
                                                              column(12, h4("Goals For and Against"),
                                                                     h5("Do teams play balanced or unbalanced games?"),
                                                                     br())),
                                                              
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team4",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))),
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("goalsPlot"))))),
                                                   navbarMenu("Variables", 
                                                   tabPanel("Established Attacks",
                                                            fluidRow(
                                                              column(12, h4("Established Attack Success Game Styles"),
                                                                     h5("How does a team perform in each game variable and which game style category does it belong to"),
                                                                     br())),
                                                              
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team5",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))), 
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("ecPlot"),
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
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br())),
                                                            
                                                            fluidRow(
                                                              column(12,  h4("League Average"),
                                                                     tableOutput("eTable")))),
                                                   
                                                   tabPanel("Established Attack Game Actions",
                                                            fluidRow(
                                                              column(12, h4("Established Attack Game Actions Game Styles"),
                                                                     h5("How does a team perform in each game variable and which game style category does it belong to"),
                                                                     br())),
                                                              
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team6",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))), 
                                                            
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("gaecPlot"),
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
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br())),
                                                            
                                                            fluidRow(
                                                              column(12, h4("League Average"),
                                                                     tableOutput("gaeTable")))),
                                                   
                                                   tabPanel("Counter Attacks",
                                                            fluidRow(
                                                              column(12, h4("Counter Attack Success Game Styles"),
                                                                     h5("How does a team perform in each game variable and which game style category does it belong to"),
                                                                     br())),
                                                             
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team7",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))),
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("ccPlot"),
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
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br())),
                                                            
                                                            fluidRow(
                                                              column(12, h4("League Average"),
                                                                     tableOutput("cTable")))),
                                                   
                                                   tabPanel("Counter Attack Game Actions",
                                                            fluidRow(
                                                              column(12, h4("Counter Attack Game Actions Game Styles"),
                                                                     h5("How does a team perform in each game variable and which game style category does it belong to"),
                                                                     br())),
                                                              
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team8",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))),
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("gaccPlot"),
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
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br())),
                                                            
                                                            fluidRow(
                                                              column(12, h4("League Average"),
                                                                     tableOutput("gacTable")))),
                                                   
                                                   tabPanel("Set Pieces",
                                                            fluidRow(
                                                              column(12, h4("Set Piece Occurrence Game Styles"),
                                                                     h5("How does a team perform in each game variable and which game style category does it belong to"),
                                                                     br())),
                                                              
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team9",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))),
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("spcPlot"))),
                                                            
                                                            fluidRow(
                                                              column(12, h4("League Average"),
                                                                     tableOutput("spTable")))),
                                                   
                                                   tabPanel("Tempo",
                                                            fluidRow(
                                                              column(12, h4("Tempo Game Styles"),
                                                                     h5("How does a team perform in each game variable and which game style category does it belong to"),
                                                                     br())),
                                                              
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team10",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))),
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("tcPlot"),
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
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br(),
                                                                     br())),
                                                            
                                                            fluidRow(
                                                              column(12,  h4("League Average"),
                                                                     tableOutput("tTable")))))
                                      )),
                             
                             tabPanel("Ball Movement",
                                      navlistPanel(widths = c(2, 10),
                                                   
                                                   tabPanel("Definitions",
                                                            fluidRow(
                                                              column(12, h4(strong("Ball Movement Variables:")),
                                                                     h5("Entropy = How unpredictable a team is"),
                                                                     h5("Poss = Possession per Zone"),
                                                                     h5("Back = Move to zone behind"),
                                                                     h5("Stay = Move within current zone"),
                                                                     h5("Forward = Move to zone in front"),
                                                                     h5("Goal = Move > 2 zones in front or directly to goal")),
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
                                                              br()),
                                                            fluidRow(
                                                              column(4, img(src = "Attack_Zones.png", style="display: block; margin-left: auto; margin-right: auto;", height = 370, width = 280),
                                                              ))),
                                                   navbarMenu("Team Profiles",
                                                   tabPanel("Attack Ball Movement",
                                                            fluidRow(
                                                              column(12, h4("Offensive Ball Movement Team Profiles"),
                                                                     h5("What ball movement strategies does a team use consistently?"),
                                                                     br())),
                                                              
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team11",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))),
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("bmPlot")))),
                                                   
                                                   tabPanel("Defence Ball Movement",
                                                            fluidRow(
                                                              column(12, h4("Defensive Ball Movement Team Profiles"),
                                                                     h5("What ball movement strategies does a team use consistently?"),
                                                                     br())),
                                                              
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team12",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))),
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("obmPlot"))))),
                                                   navbarMenu("Heat Maps",
                                                   tabPanel("Attack",
                                                            fluidRow(
                                                              column(12, h4("Attack Heat Maps"),
                                                                     h5("Where does a team have possession, where are they unpredictable, in which direction do they move?"),
                                                                     br())),
                                                              
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team13",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected")))),
                                                              column(4, h6(selectInput(inputId = "ms13",
                                                                                       label = "Choose the match status you want to analyse:",
                                                                                       choices = c("Winning" = "Winning",
                                                                                                   "Losing" = "Losing",
                                                                                                   "Drawing" = "Drawing"),
                                                                                       selected = "Winning"))),
                                                            column(4, h6(selectInput(inputId = "zone13",
                                                                                     label = "Choose the attack zone you want to analyse:",
                                                                                     choices = c("Circle" = "Circle",
                                                                                                 "Corners" = "Corners",
                                                                                                 "Deep Attack" = "DeepAtt",
                                                                                                 "Build Attack" = "BuildAtt",
                                                                                                 "Build Defence" = "BuildDef",
                                                                                                 "Outlet" = "Outlet",
                                                                                                 "Deep Defence" = "DeepDef"),
                                                                                     selected = "BuildAtt")))),
                                                            
                                                            fluidRow(
                                                              column(4, align = "center", plotOutput("hmPlot")),
                                                              column(4, align = "center", plotOutput("entPlot")),
                                                              column(4, align = "center", plotOutput("progPlot")))),
                                                   
                                                   
                                                   
                                                   
                                                   
                                                   tabPanel("Defence",
                                                            fluidRow(
                                                              column(12, h4("Defence Heat Maps"),
                                                                     h5("Where does the opposition have possession, where are they unpredictable, in which direction do they move?"),
                                                                     br())),
                                                             
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team16",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected")))),
                                                              
                                                              column(4, h6(selectInput(inputId = "ms16",
                                                                                       label = "Choose the match status you want to analyse:",
                                                                                       choices = c("Winning" = "Winning",
                                                                                                   "Losing" = "Losing",
                                                                                                   "Drawing" = "Drawing"),
                                                                                       selected = "Winning"))),
                                                            
                                                            column(4, h6(selectInput(inputId = "zone16",
                                                                                     label = "Choose the attack zone you want to analyse:",
                                                                                     choices = c("Circle" = "Circle",
                                                                                                 "Corners" = "Corners",
                                                                                                 "Deep Attack" = "DeepAtt",
                                                                                                 "Build Attack" = "BuildAtt",
                                                                                                 "Build Defence" = "BuildDef",
                                                                                                 "Outlet" = "Outlet",
                                                                                                 "Deep Defence" = "DeepDef"),
                                                                                     selected = "BuildAtt")))),
                                                            
                                                            fluidRow(
                                                              column(4, align = "center",  plotOutput("ohmPlot")),
                                                              column(4, align = "center",  plotOutput("oentPlot")),
                                                              column(4, align = "center", plotOutput("oprogPlot"))
                                                            ))
                                                   
                                                   
                                                   
                                                   
                                      ))),
                             
                             tabPanel("In Game Events",
                                      navlistPanel(widths = c(2, 10),
                                                   navbarMenu("Goal Shots",
                                                   tabPanel("Open Play",
                                                            fluidRow(
                                                              column(12, h4("Goal Shot Locations"),
                                                                     h5("What areas of the circle does a team create goal shots from and score goals?"),
                                                                     br())),
                                                             
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team19",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected")))),
                                                              column(4, h6(selectInput(inputId = "outcome19",
                                                                                       label = "Choose the shot outcome you want to analyse:",
                                                                                       choices = c("Goal" = "Goal", 
                                                                                                   "Saved" = "Saved",
                                                                                                   "Smothered" = "Smothered",
                                                                                                   "Turnover" = "Turnover"), 
                                                                                       selected = "Goal")))),
                                                            
                                                            fluidRow(
                                                              column(8, align = "center", offset = 2,  plotOutput("gsnPlot"))
                                                            ),
                                                            fluidRow(
                                                              column(8, align = "center", offset = 2, plotOutput("gsoPlot"))
                                                            ),
                                                            fluidRow(
                                                              column(8, align = "center", offset = 2, plotOutput("gsePlot"))
                                                            )),
                                                   
                                                  
                                                   
                                                   tabPanel("Penalty Corners",
                                                            fluidRow(
                                                              column(12, h4("Penalty Corner Patterns"),
                                                                     h5("What penalty corner routines does a team attempt and how successful are they?"),
                                                                     br())),
                                                            
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team22",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))),
                                                            
                                                            fluidRow(
                                                              column(2, img(src = "SP_Straight.png", style = "display: block; margin-left: auto; margin-right: auto;", height = 157.5, width = 237.5)),
                                                              column(2, offset = 1, img(src = "SP_Layoff.png", style = "display: block; margin-left: auto; margin-right: auto;", height = 157.5, width = 237.5)),
                                                              column(2, offset = 1, img(src = "SP_Deflection.png", style = "display: block; margin-left: auto; margin-right: auto;", height = 157.5, width = 237.5))),
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("pcPlot"))))),
                                                   
                                                   tabPanel("Game Actions",
                                                            fluidRow(
                                                              column(12, h4("Game Action Types and Effectiveness"),
                                                                     h5("How effective is a team at dribbling and passing?"),
                                                                     br())),
                                                             
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team23",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected")))),
                                                              
                                                              column(4, h6(selectInput(inputId = "attack23",
                                                                                       label = "Choose the attack type you want to analyse:",
                                                                                       choices = c("Established" = "E",
                                                                                                   "Counter" = "C"), 
                                                                                       selected = "E")))),
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("gaPlot"))
                                                            )),
                                                   
                                                   tabPanel("Stoppages",
                                                            fluidRow(
                                                              column(12, h4("Stoppage Types"),
                                                                     h5("What types of stoppages do they win?"),
                                                                     br())),
                                                            
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team24",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected")))),
                                                              column(4, h6(selectInput(inputId = "attack24",
                                                                                       label = "Choose the attack type you want to analyse:",
                                                                                       choices = c("Established" = "E",
                                                                                                   "Counter" = "C"), 
                                                                                       selected = "E")))),
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("sPlot")))),
                                                   
                                                   tabPanel("Turnovers", 
                                                            fluidRow(
                                                              column(12, h4("Turnover Types"),
                                                                     h5("How does a team lose possession?"),
                                                                     br())),
                                                              
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team25",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected")))),
                                                              column(4, h6(selectInput(inputId = "attack25",
                                                                                       label = "Choose the attack type you want to analyse:",
                                                                                       choices = c("Established" = "E",
                                                                                                   "Counter" = "C"), 
                                                                                       selected = "E")))),
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("tPlot")))),
                                                   navbarMenu("Possessions",
                                                   tabPanel("Possession Time", 
                                                            fluidRow(
                                                              column(12, h4("Possession Time"),
                                                                     h5("How long does a team control possession?"),
                                                                     br())),
                                                             
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team26",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))), 
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("timePlot"))
                                                            )),
                                                   
                                                   tabPanel("Possession Length",
                                                            fluidRow(
                                                              column(12, h4("Possession Length"),
                                                                     h5("How many game events occur per possession?"),
                                                                     br())),
                                                             
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team27",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))), 
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("lengthPlot")))),
                                                   
                                                   tabPanel("Possession Rate",
                                                            fluidRow(
                                                              column(12, h4("Possession Rate"),
                                                                     h5("How quickly does a team move the ball?"),
                                                                     br())),
                                                              
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team28",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))), 
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("ratePlot"))))),
                                                   navbarMenu("Field Progress",
                                                   tabPanel("Start Locations",
                                                            fluidRow(
                                                              column(12, h4("Start Locations"),
                                                                     h5("Where does a team start their possessions"),
                                                                     br())),
                                                              
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team29",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))), 
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("startPlot")))),
                                                   
                                                   tabPanel("End Locations",
                                                            fluidRow(
                                                              column(12, h4("End Locations"),
                                                                     h5("Where does a team end their possession and how much ground do they gain?"),
                                                                     br())),
                                                              
                                                            
                                                            fluidRow(
                                                              column(4, h6(selectInput(inputId = "team30",
                                                                                       label = "Choose the team you want to analyse:",
                                                                                       choices = c("Not Selected"))))),
                                                            
                                                            fluidRow(
                                                              column(12, align = "center", plotOutput("endPlot")))))
                                                   
                                      ))
                             
                  )
))