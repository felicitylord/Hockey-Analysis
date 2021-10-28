
# load packages #
library(shiny)
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

# import data #
# GAME STYLES #
MO <- reactive({
  read_excel("Data/Match_Outcome_Women.xlsx")})

Goals <- reactive({
  read_excel("Data/Goals_Women.xlsx")})

Att_GS <- reactive({
  read_excel("Data/Att_Game_Styles_Women.xlsx")})

Def_GS <- reactive({
  read_excel("Data/Def_Game_Styles_Women.xlsx")})

var <- read_excel("Data/GS_Variables_Women2.xlsx")

GAE_var <- reactive({
  filter(var, Category == "GAE")})
E_var <- reactive({
  filter(var, Category == "E")})
GAC_var <- reactive({
  filter(var, Category == "GAC")})
C_var <- reactive({
  filter(var, Category == "C")})
SP_var <- reactive({
  filter(var, Category == "SP")})
T_var <- reactive({
  filter(var, Category == "T")})

Clusters <- read_excel("Data/Cluster_Averages_Women.xlsx")

GAE_cluster <- reactive({
  filter(Clusters, Category == "GAE")})
E_cluster <- reactive({
  filter(Clusters, Category == "E")})
GAC_cluster <- reactive({
  filter(Clusters, Category == "GAC")})
C_cluster <- reactive({
  filter(Clusters, Category == "C")})
SP_cluster <- reactive({
  filter(Clusters, Category == "SP")})
T_cluster <- reactive({
  filter(Clusters, Category == "T")})

e_avg <- reactive({
  read_excel("Data/Established_Average_Women.xlsx")})

gae_avg <- reactive({
  read_excel("Data/GAE_Average_Women.xlsx")})

c_avg <- reactive({
  read_excel("Data/Counter_Average_Women.xlsx")})

gac_avg <- reactive({
  read_excel("Data/GAC_Average_Women.xlsx")})

sp_avg <- reactive({
  read_excel("Data/Set_Piece_Average_Women.xlsx")})

t_avg <- reactive({
  read_excel("Data/Tempo_Average_Women.xlsx")})


# BALL MOVEMENT #
BM <- reactive({
  read_excel("Data/Ball_Move_MS_Women.xlsx")})

OBM <- reactive({
  read_excel("Data/Opp_Ball_Move_MS_Women.xlsx")})

HM <- reactive({
  read_excel("Data/Game_Heat_Maps_Women.xlsx")})

OHM <- reactive({
  read_excel("Data/Opp_Game_Heat_Maps_Women.xlsx")})

E <- reactive({
  read_excel("Data/Game_Entropy_Women.xlsx")})

OE <- reactive({
  read_excel("Data/Opp_Game_Entropy_Women.xlsx")})

Prog <- reactive({
  read_excel("Data/Game_Progress_Zones_Women.xlsx")})

OProg <- reactive({
  read_excel("Data/Opp_Game_Progress_Zones_Women.xlsx")})


# GOAL SHOTS #
GSN <- reactive({
  read_excel("Data/GS_Number_Women.xlsx")})

GSO <- reactive({
  read_excel("Data/GS_Outcomes_Women.xlsx")})

GSE <- reactive({
  read_excel("Data/GS_Efficiency_Women.xlsx")})

SP <- reactive({
  read_excel("Data/SP_Patterns_Women.xlsx")})

# GAME ACTIONS #
GA <- reactive({
  read_excel("Data/Game_Actions_Women.xlsx")})

GAA <- reactive({
  read_excel("Data/Game_Actions_Avg_Women.xlsx")})

# STOPPAGES #
S <- reactive({
  read_excel("Data/Stoppages_Women.xlsx")})

SA <- reactive({
  read_excel("Data/Stoppages_Avg_Women.xlsx")})

# TURNOVERS #
T <- reactive({
  read_excel("Data/Turnovers_Women.xlsx")})

TA <- reactive({
  read_excel("Data/Turnovers_Avg_Women.xlsx")})

# POSSESSIONS #
Time <- reactive({
  read_excel("Data/Time_Women.xlsx")})

TimeA <- reactive({
  read_excel("Data/Time_Avg_Women.xlsx")})

Length <- reactive({
  read_excel("Data/Length_Women.xlsx")})

LengthA <- reactive({
  read_excel("Data/Length_Avg_Women.xlsx")})

Rate <- reactive({
  read_excel("Data/Rate_Women.xlsx")})

RateA <- reactive({
  read_excel("Data/Rate_Avg_Women.xlsx")})


# START AND END #
Start <- reactive({
  read_excel("Data/Start_Women.xlsx")})

StartA <- reactive({
  read_excel("Data/Start_Avg_Women.xlsx")})

End <- reactive({
  read_excel("Data/End_Women.xlsx")})

EndA <- reactive({
  read_excel("Data/End_Avg_Women.xlsx")})


# Define server logic required to create visualisations #
shinyServer(function(input, output) {
  
  ## PLOT 1 ## 
  
  # identify teams from uploaded file #
  choices1 <- reactive({
    Att_GS() %>%
      select(Team) %>%
      unique()
  })
  
  # update choices to reflect team list from uploaded file #
  observeEvent(Att_GS(), {
    updateSelectInput(inputId = "team1", choices = choices1())
  })
  
  # create plot based on team selected from uploaded file #
  output$agsPlot <- renderPlot({
    
    att_subset <- reactive({
      subset(Att_GS(), Team == input$team1)
    })
    
    ggplot(data = att_subset(), aes(x = factor(Category, levels = c("GAE_Dribble", "GAE_Pass", "E_Strong", "GAC_Dribble", "GAC_Pass", "C_Strong", "SP_High", "Direct", "Poss")), 
                                    y = factor(MS, levels = c("Drawing", "Losing", "Winning")), fill = Prop)) +
      geom_tile() +
      theme_bw() +
      scale_fill_gradient2(name = "Percentage\nof matches", low = "red", mid = "white", high = "deepskyblue", midpoint = 50) +
      geom_text(aes(label = Prop), size = 8) +
      ggtitle("ATTACK") +
      theme(plot.title = element_text(hjust = 0.5, size = 14), 
            axis.text.x = element_text(size = 14, angle = 360), 
            axis.text.y = element_text(size = 14), 
            strip.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12)) +
      scale_x_discrete(name = "", labels = c("GAE_Dribble" = "Dribble", 
                                             "GAE_Pass" = "Pass", 
                                             "E_Strong" = "Strong", 
                                             "GAC_Dribble" = "Dribble", 
                                             "GAC_Pass" = "Pass", 
                                             "C_Strong" = "Strong", 
                                             "SP_High" = "High", 
                                             "Direct" = "Direct", 
                                             "Poss" = "Possession")) +
      ylab("") +
      facet_grid(. ~ factor(MOP, levels = c("EA", "GAE", "CA", "GAC", "SP", "Tempo"), 
                            labels = c("Established Attack", "Established Game Actions", "Counter Attack","Counter Game Actions", "Set Piece", "Tempo")), 
                 scales = "free", space = "free", 
                 labeller = label_wrap_gen(width = 3, multi_line = TRUE))
    
  }, height = 500, width = 937.5)
  
  ## PLOT 2 ## 
  
  choices2 <- reactive({
    Def_GS() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(Def_GS(), {
    updateSelectInput(inputId = "team2", choices = choices2())
  })
  
  output$dgsPlot <- renderPlot({
    
    def_subset <- reactive({
      subset(Def_GS(), Team == input$team2)
    })
    
    ggplot(def_subset(), aes(x = factor(Category, levels = c("GAE_Dribble", "GAE_Pass", "ED", "GAC_Dribble", "GAC_Pass", "CD", "SP_High", "Direct", "Poss")), 
                             y = factor(MS, levels = c("Drawing","Losing", "Winning")), fill = Prop)) +
      geom_tile() +
      theme_bw() +
      scale_fill_gradient2(name = "Percentage\nof matches", low = "red", mid = "white", high = "deepskyblue", midpoint = 50) +
      geom_text(aes(label = Prop), size = 8) +
      ggtitle("DEFENCE (Opposition Attack)") +
      theme(plot.title = element_text(hjust = 0.5, size = 14), 
            axis.text.x = element_text(size = 14, angle = 3600),     
            axis.text.y = element_text(size = 14), 
            strip.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12)) +
      scale_x_discrete(name = "", labels = c("GAE_Dribble" = "Opp\nDribble", 
                                             "GAE_Pass" = "Opp\nPass", 
                                             "ED" = "Strong", 
                                             "GAC_Dribble" = "Opp\nDribble", 
                                             "GAC_Pass" = "Opp\nPass", 
                                             "CD" = "Strong", 
                                             "SP_High" = "Opp\nHigh", 
                                             "Direct" = "Opp\nDirect", 
                                             "Poss" = "Opp\nPossession")) +
      ylab("") +
      facet_grid(. ~ factor(MOP, levels = c("ED", "GAE", "CD", "GAC", "SP", "Tempo"), 
                            labels = c("Established Defence", "Established Game Actions", "Counter Defence", "Counter Game Actions", "Set Piece", "Tempo")), 
                 scales = "free", space = "free", 
                 labeller = label_wrap_gen(width = 3, multi_line = TRUE))
    
  }, height = 500, width = 937.5)
  
  ## PLOT 3## 

  choices3 <- reactive({
    MO() %>%
      select(Team.x) %>%
      unique()
  })
  
  observeEvent(MO(), {
    updateSelectInput(inputId = "team3", choices = choices3())
  })
  
  output$moPlot <- renderPlot({
    
    mo_subset <- reactive({
      subset(MO(),  Team.x == input$team3 
             & Match.Status == input$ms3)
    })
    
    ggplot() +
      geom_tile(data = mo_subset(), aes(x = Match_Opp, y = Category, fill = as.factor(Result))) +
      geom_point(data = mo_subset(), aes(x = Match_Opp, y = Category, colour = as.factor(Id)), show.legend = FALSE) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size= 14), 
            axis.text.y = element_text(size = 14)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("") +
      ylab("") +
      labs(fill = "Match Outcome") +
      scale_colour_manual(values = c(NA, "black")) +
      scale_fill_manual(values = alpha(c("Draw" = "dodger blue", "Loss" = "red", "Win" = "lime green"), 0.75)) +
      facet_grid(MOP ~., scales = "free", space = "free", labeller = label_wrap_gen(width = 3, multi_line = TRUE)) +
      scale_y_discrete(name = "", labels = c("E_Dribble" = "Dribble", 
                                             "E_Pass" = "Pass", 
                                             "E_Strong" = "Strong", 
                                             "E_Poor" = "Poor", 
                                             "C_Dribble" = "Dribble", 
                                             "C_Pass" = "Pass", 
                                             "C_Strong" = "Strong", 
                                             "C_Poor" = "Poor", 
                                             "SP_High" = "High",  
                                             "SP_Low" = "Low", 
                                             "Direct" = "Direct", 
                                             "Poss" = "Possession")) +
      theme(panel.border = element_rect(colour = "black", fill = NA),  
            strip.text.y = element_text(size = 14, angle = 360),  
            axis.text.y = element_text(size = 14), 
            axis.text.x = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14))
    
  }, height = 625, width = 750)
  
  ## PLOT 4 ## 
  
  choices4 <- reactive({
    Goals() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(Goals(), {
    updateSelectInput(inputId = "team4", choices = choices4())
  })
  
  output$goalsPlot <- renderPlot({
    
    goals_subset <- reactive({
      subset(Goals(), Team == input$team4)
    })
    
    ggplot() +
      geom_col(data = goals_subset(), aes(x = Match_Opp, y = Goals, 
                                          fill = factor(Score, levels = c("Score_Team", "Score_Opp")))) +
      theme_bw() +
      xlab("") +
      theme(axis.text.x = element_text(angle = 90, size = 14),  
            legend.text = element_text(size = 14), 
            axis.title.y = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      scale_fill_manual(name = "", labels = c(input$team4, "Opposition"), 
                        values = c("lime green", "black")) 
  }, height = 500, width = 750)
  
  ## TABLE 1 ## 
  
  output$eTable <- renderTable(e_avg())
  
  ## PLOT 5 ## 

  choices5 <- reactive({
    E_var() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(E_var(), {
    updateSelectInput(inputId = "team5", choices = choices5())
  })
  
  output$ecPlot <- renderPlot({
    
    var_subset <- reactive({
      subset(E_var(), Team == input$team5)
    })
    
    ggplot() +
      geom_col(data = E_cluster(), aes(x = factor(Variable, levels = c("E_GSN", "E_SPN", "E_TN", "AC_Time_E", "A25_Time_E", "A50_Time_E", "D50_Time_E", "D25_Time_E",  "A25E_Main_Gain", "A25E_Retain_Turnover", "A50E_Retain_Turnover", "D50E_Retain_Turnover", "D25E_Retain_Turnover", "S_ACE", "S_A25E",  "S_A50E", "S_D50E", "S_D25E", "A25E_S_F", "A50E_S_F", "D50E_S_F", "D25E_S_F",  "TE_AC", "TE_A25", "TE_A50", "TE_D50", "TE_D25", "TE_Play", "TE_Stoppages", "TE_High",  "TE_Medium",  "TE_Low", "E_GS", "E_Goal")), 
                                       y = zscore, fill = as.factor(E_Cluster))) +
      geom_point(data = var_subset(), aes(x = Variable, y = zscore, shape = Match.Status)) +
      facet_grid(Match.Status ~ ., scales = "free", space = "free") +
      theme_bw() +
      ylab("Comparison to League Average (zscore)") +
      scale_x_discrete(name = "", labels = c("E_GSN" = "GS EA (%)", 
                                             "E_SPN" = "Set Piece EA (%)", 
                                             "E_TN" = "Turnovers EA (%)", 
                                             "AC_Time_E" = "Time in AC (%)", 
                                             "A25_Time_E" = "Time in A25 (%)", 
                                             "A50_Time_E" = "Time in A50 (%)", 
                                             "D50_Time_E" = "Time in D50 (%)", 
                                             "D25_Time_E" = "Time in D25 (%)", 
                                             "A25E_Main_Gain" = "Game Actions Main:Gain A25", 
                                             "A25E_Retain_Turnover" = "Game Actions Retain:Turnover A25", 
                                             "A50E_Retain_Turnover" = "Game Actions Retain:Turnover A50", 
                                             "D50E_Retain_Turnover" = "Game Actions Retain:Turnover D50", 
                                             "D25E_Retain_Turnover" = "Game Actions Retain:Turnover D25", 
                                             "S_ACE" =  "Stoppages AC (%)", 
                                             "S_A25E" =  "Stoppages A25 (%)", 
                                             "S_A50E" =  "Stoppages A50 (%)", 
                                             "S_D50E" = "Stoppages D50 (%)", 
                                             "S_D25E" = "Stoppages D25 (%)", 
                                             "A25E_S_F" = "Restart Speed Slow:Fast A25", 
                                             "A50E_S_F" = "Restart Speed Slow:Fast A50", 
                                             "D50E_S_F" = "Restart Speed Slow:Fast D50", 
                                             "D25E_S_F" = "Restart Speed Slow:Fast D25", 
                                             "TE_AC" = "Turnovers AC (%)", 
                                             "TE_A25" =  "Turnovers A25 (%)",  
                                             "TE_A50" = "Turnovers A50 (%)", 
                                             "TE_D50" = "Turnovers D50 (%)", 
                                             "TE_D25" = "Turnovers D25 (%)",  
                                             "TE_Play" = "Turnovers Open Play (%)", 
                                             "TE_Stoppages" = "Turnovers Stoppages (%)", 
                                             "TE_High" = "Turnovers High Pressure (%)", 
                                             "TE_Medium" = "Turnovers Medium Pressure (%)", 
                                             "TE_Low" = "Turnovers Low Pressure (%)", 
                                             "E_GS" =  "EA/GS", 
                                             "E_Goal" =  "EA/Goal")) +
      theme(axis.text.x = element_text(angle = 90, size = 14), 
            axis.title.y = element_text(size = 14, vjust = 2.5), 
            strip.text = element_text(size = 14),
            legend.text = element_text(size = 14), 
            legend.title = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      scale_fill_manual(name = "Game Style\nAverage", values = alpha(c("orange red", "dodger blue"), 0.75)) +
      scale_shape_manual(name = "Team Average", values = c(15, 16, 17)) +
      guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2))
    
  }, height = 750, width = 1000)
  
  ## TABLE 2 ## 
  
  output$gaeTable <- renderTable(gae_avg())
  
  ## PLOT 6 ## 
  
  choices6 <- reactive({
    GAE_var() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(GAE_var(), {
    updateSelectInput(inputId = "team6", choices = choices6())
  })
  
  output$gaecPlot <- renderPlot({
    
    var_subset <- reactive({
      subset(GAE_var(), Team == input$team6)
    })
    
    ggplot() +
      geom_col(data = GAE_cluster(), aes(x = factor(Variable, levels = c("ACE_Cross", "ACE_Dribble", "ACE_Pass", "A25E_Cross", "A25E_Dribble", "A25E_Pass", "A50E_Dribble", "A50E_Pass", "D50E_Dribble", "D50E_Overhead", "D50E_Pass",  "D25E_Dribble",  "D25E_Overhead", "D25E_Pass" )), 
                                         y = zscore, fill = as.factor(GAE_Cluster))) +
      geom_point(data = var_subset(), aes(x = Variable, y = zscore, shape = Match.Status)) +
      facet_grid(Match.Status ~ ., scales = "free", space = "free") +
      theme_bw() +
      ylab("Comparison to League Average (zscore)") +
      scale_x_discrete(name = "", labels = c("D25E_Pass" = "D25 Pass (%)", 
                                             "D25E_Overhead" = "D25 Overhead (%)", 
                                             "D25E_Dribble" = "D25 Dribble (%)", 
                                             "D50E_Pass" = "D50 Pass (%)", 
                                             "D50E_Overhead" = "D50 Overhead (%)", 
                                             "D50E_Dribble" = "D50 Dribble (%)", 
                                             "A50E_Pass" = "A50 Pass (%)", 
                                             "A50E_Dribble" = "A50 Dribble (%)", 
                                             "A25E_Pass" = "A25 Pass (%)", 
                                             "A25E_Dribble" = "A25 Dribble (%)", 
                                             "A25E_Cross" = "A25 Cross (%)", 
                                             "ACE_Pass" = "AC Pass (%)", 
                                             "ACE_Dribble" = "AC Dribble (%)", 
                                             "ACE_Cross" = "AC Cross (%)")) +
      theme(axis.text.x = element_text(angle = 90, size = 14), 
            axis.title.y = element_text(size = 12, vjust = 2.5), 
            strip.text = element_text(size = 14),
            legend.text = element_text(size = 14), 
            legend.title = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      scale_fill_manual(name = "Game Style\nAverage",  values = alpha(c("darkorange", "blue"), 0.75)) +
      scale_shape_manual(name = "Team Average", values = c(15, 16, 17)) +
      guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2))
    
  }, height = 750, width = 1000)
  
  ## TABLE 3 ## 
 
  output$cTable <- renderTable(c_avg())
  
  ## PLOT 7 ## 
  
  choices7 <- reactive({
    C_var() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(C_var(), {
    updateSelectInput(inputId = "team7", choices = choices7())
  })
  
 
  output$ccPlot <- renderPlot({
    
    var_subset <- reactive({
      subset(C_var(), Team == input$team7)
    })
    
    ggplot() +
      geom_col(data = C_cluster(), aes(x = factor(Variable, levels = c("C_GSN", "C_SPN", "C_EN", "C_TN", "AC_Time_C", "A25_Time_C", "A50_Time_C", "D50_Time_C", "D25_Time_C",  "D25C_Main_Gain", "A50C_Retain_Turnover",  "D25C_Retain_Turnover", "S_ACC", "S_A25C",  "S_A50C", "S_D50C", "S_D25C",   "TC_AC", "TC_A25", "TC_A50", "TC_D50", "TC_D25", "TC_Play", "TC_Stoppages", "TC_High",  "TC_Medium",  "TC_Low", "C_GS", "C_Goal")), 
                                       y = zscore, fill = as.factor(C_Cluster))) +
      geom_point(data = var_subset(), aes(x = Variable, y = zscore, shape = Match.Status)) +
      facet_grid(Match.Status ~ ., scales = "free", space = "free") +
      theme_bw() +
      ylab("Comparison to League Average (zscore)") +
      scale_x_discrete(name = "", labels = c("C_GSN" = "GS CA (%)", 
                                             "C_SPN" = "Set Piece CA (%)", 
                                             "C_EN" = "CA to EA (%)", 
                                             "C_TN" = "Turnovers CA (%)", 
                                             "AC_Time_C" = "Time in AC (%)", 
                                             "A25_Time_C" = "Time in A25 (%)", 
                                             "A50_Time_C" = "Time in A50 (%)", 
                                             "D50_Time_C" = "Time in D50 (%)", 
                                             "D25_Time_C" = "Time in D25 (%)", 
                                             "D25C_Main_Gain" = "Game Actions Main:Gain D25", 
                                             "A50C_Retain_Turnover" = "Game Actions Retain:Turnover A50",  
                                             "D25C_Retain_Turnover" = "Game Actions Retain:Turnover D25", 
                                             "S_ACC" =  "Stoppages AC (%)", 
                                             "S_A25C" =  "Stoppages A25 (%)", 
                                             "S_A50C" =  "Stoppages A50 (%)", 
                                             "S_D50C" = "Stoppages D50 (%)", 
                                             "S_D25C" = "Stoppages D25 (%)", 
                                             "TC_AC" = "Turnovers AC (%)", 
                                             "TC_A25" =  "Turnovers A25 (%)",  
                                             "TC_A50" = "Turnovers A50 (%)", 
                                             "TC_D50" = "Turnovers D50 (%)", 
                                             "TC_D25" = "Turnovers D25 (%)",  
                                             "TC_Play" = "Turnovers Open Play (%)", 
                                             "TC_Stoppages" = "Turnovers Stoppages (%)", 
                                             "TC_High" = "Turnovers High Pressure (%)", 
                                             "TC_Medium" = "Turnovers Medium Pressure (%)", 
                                             "TC_Low" = "Turnovers Low Pressure (%)", 
                                             "C_GS" =  "CA/GS", 
                                             "C_Goal" =  "CA/Goal")) +
      theme(axis.text.x = element_text(angle = 90, size = 14), 
            axis.title.y = element_text(size = 14, vjust = 2.5), 
            strip.text = element_text(size = 14),
            legend.text = element_text(size = 14), 
            legend.title = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      scale_fill_manual(name = "Game Style\nAverage",  values = alpha(c("orange red", "dodger blue"), 0.75)) +
      scale_shape_manual(name = "Team Average", values = c(15, 16, 17)) +
      guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2))
    
  }, height = 750, width = 1000) 
  
  ## TABLE 4 ## 
  
  output$gacTable <- renderTable(gac_avg())
  
  ## PLOT 8 ## 
  
  choices8 <- reactive({
    GAC_var() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(GAC_var(), {
    updateSelectInput(inputId = "team8", choices = choices8())
  })
  
 
  output$gaccPlot <- renderPlot({
    
    var_subset <- reactive({
      subset(GAC_var(), Team == input$team8)
    })
    
    ggplot() +
      geom_col(data = GAC_cluster(), aes(x = factor(Variable, levels = c("ACC_Cross", "ACC_Dribble", "ACC_Pass", "A25C_Cross", "A25C_Dribble", "A25C_Pass", "A50C_Through Ball", "A50C_Dribble", "A50C_Pass", "D50C_Dribble",  "D50C_Pass", "D25C_Clearance", "D25C_Dribble",  "D25C_Pass")), 
                                         y = zscore, fill = as.factor(GAC_Cluster))) +
      geom_point(data = var_subset(), aes(x = Variable, y = zscore, shape = Match.Status)) +
      facet_grid(Match.Status ~ ., scales = "free", space = "free") +
      theme_bw() +
      ylab("Comparison to League Average (zscore)") +
      scale_x_discrete(name = "", labels = c("D25C_Pass" = "D25 Pass (%)", 
                                             "D25C_Dribble" = "D25 Dribble (%)", 
                                             "D25C_Clearance" = "D25 Clearance (%)", 
                                             "D50C_Pass" = "D50 Pass (%)", 
                                             "D50C_Dribble" = "D50 Dribble (%)", 
                                             "A50C_Through Ball" = "A50 Through Ball (%)", 
                                             "A50C_Pass" = "A50 Pass (%)", 
                                             "A50C_Dribble" = "A50 Dribble (%)", 
                                             "A25C_Pass" = "A25 Pass (%)", 
                                             "A25C_Dribble" = "A25 Dribble (%)", 
                                             "A25C_Cross" = "A25 Cross (%)", 
                                             "ACC_Pass" = "AC Pass (%)", 
                                             "ACC_Dribble" = "AC Dribble (%)", 
                                             "ACC_Cross" = "AC Cross (%)")) +
      theme(axis.text.x = element_text(angle = 90, size = 14), 
            axis.title.y = element_text(size = 14, vjust = 2.5), 
            strip.text = element_text(size = 14),
            legend.text = element_text(size = 14), 
            legend.title = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      scale_fill_manual(name = "Game Style\nAverage",  values = alpha(c("darkorange", "blue"), 0.75)) +
      scale_shape_manual(name = "Team Average", values = c(15, 16, 17)) +
      guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2))
    
  }, height = 750, width = 1000) 
  
  ## TABLE 5 ## 
  
  output$spTable <- renderTable(sp_avg())
  
  ## PLOT 9 ## 
  
  choices9 <- reactive({
    SP_var() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(SP_var(), {
    updateSelectInput(inputId = "team9", choices = choices9())
  })
  
  output$spcPlot <- renderPlot({
    
    var_subset <- reactive({
      subset(SP_var(), Team == input$team9)
    })
    
    ggplot() +
      geom_col(data = SP_cluster(), aes(x = factor(Variable, levels = c("SP_Total", "SP_Goal")), 
                                        y = zscore, fill = as.factor(SP_Cluster))) +
      geom_point(data = var_subset(), aes(x = Variable, y = zscore, shape = Match.Status) ) +
      facet_grid(Match.Status ~ ., scales = "free", space = "free") +
      theme_bw() +
      ylab("Comparison to League Average (zscore)") +
      scale_x_discrete(name = "", labels = c("SP_Total" = "SP/Game", "SP_Goal" = "SP/Goal")) +
      theme(axis.text.x = element_text(angle = 90, size = 14), 
            axis.title.y = element_text(size = 14, vjust = 2.5), 
            strip.text = element_text(size = 14),
            legend.text = element_text(size = 14), 
            legend.title = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      scale_fill_manual(name = "Game Style\nAverage",  values = alpha(c("orangered2", "mediumblue"), 0.75)) +
      scale_shape_manual(name = "Team Average", values = c(15, 16, 17)) +
      guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2))
    
  }, height = 375, width = 500) 
  
  ## TABLE 6 ## 
  
  output$tTable <- renderTable(t_avg())
  
  ## PLOT 10 ## 
  
  choices10 <- reactive({
    T_var() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(T_var(), {
    updateSelectInput(inputId = "team10", choices = choices10())
  })
  
 
  output$tcPlot <- renderPlot({
    
    var_subset <- reactive({
      subset(T_var(), Team == input$team10)
    })
    
    ggplot() +
      geom_col(data = T_cluster(), aes(x = factor(Variable, levels = c("GA_E", "GA_C", "GA_S", "GA_T",  "GA_GS", "GA_Total", "E_Total", "C_Total", "Penetrating")), 
                                       y = zscore, fill = as.factor(Tempo_Cluster))) +
      geom_point(data = var_subset(), aes(x = Variable, y = zscore, shape = Match.Status)) +
      facet_grid(Match.Status ~ ., scales = "free", space = "free") +
      theme_bw() +
      ylab("Comparison to League Average (zscore)") +
      scale_x_discrete(name = "", labels = c("GA_E" = "Game Actions/EA", 
                                             "GA_C" = "Game Actions/CA", 
                                             "GA_S" = "Game Actions/Stoppage", 
                                             "GA_T" = "Game Actions/Turnover",  
                                             "GA_GS" = "Game Actions/Goal Shot", 
                                             "GA_Total" = "Game Actions/Game", 
                                             "E_Total" = "EA/Game", 
                                             "C_Total" = "CA/Game", 
                                             "Penetrating" = "Penetrating Game Actions/Game")) +
      theme(axis.text.x = element_text(angle = 90, size = 14), 
            axis.title.y = element_text(size = 14, vjust = 2.5), 
            strip.text = element_text(size = 14),
            legend.text = element_text(size = 14), 
            legend.title = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      scale_fill_manual(name = "Game Style\nAverage", 
                        values = alpha(c("darkorange2", "navy"), 0.75)) +
      scale_shape_manual(name = "Team Average", values = c(15, 16, 17)) +
      guides(shape = guide_legend(order = 1), fill = guide_legend(order = 2))
    
  }, height = 750, width = 1000) 
  
  ## PLOT 11 ## 
  
  choices11 <- reactive({
    BM() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(BM(), {
    updateSelectInput(inputId = "team11", choices = choices11())
  })
  
  output$bmPlot <- renderPlot({
    
    bm_subset <- reactive({
      subset(BM(), Team == input$team11
      )
    })
    
    ggplot(bm_subset(), aes(x = factor(Variable, levels = c("Game_Poss", "Entropy", "Poss", "Back", "Stay", "Forward", "Goal")), 
                            y = MS, fill = zscore)) +
      geom_tile() +
      theme_bw() +
      scale_fill_gradient2(name = "Comparison to\nleague average", low = "purple", mid = "white", high = "green3") +
      geom_text(aes(label = value), size = 4.5) +
      ggtitle("ATTACK") +
      theme(plot.title = element_text(hjust = 0.5, size =14), 
            axis.text.x = element_text(size = 14), 
            axis.text.y = element_text(size = 14), 
            strip.text = element_text(size = 14), 
            axis.title.y.left = element_blank(),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12)) +
      facet_grid(factor(Start, levels = c("Circle", "Corners", "DeepAtt", "BuildAtt", "BuildDef", "Outlet", "DeepDef"), 
                        labels = c("Circle", "Corners", "Deep Att", "Build Att", "Build Def", "Outlet", "Deep Def")) ~ .) +
      scale_x_discrete(name = "", labels = c("Game_Poss" = "Game Poss (%)", 
                                             "Entropy" = "Entropy", 
                                             "Poss" = "Poss (%)", 
                                             "Back" = "Back (%)", 
                                             "Stay" = "Stay (%)", 
                                             "Forward" = "Forward (%)", 
                                             "Goal" = "Goal (%)"))
    
  }, height = 687.5, width = 825)
  
  ## PLOT 12 ## 
  
  choices12 <- reactive({
    OBM() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(OBM(), {
    updateSelectInput(inputId = "team12", choices = choices12())
  })
  
  output$obmPlot <- renderPlot({
    
    obm_subset <- reactive({
      subset(OBM(),  Team == input$team12)
    })
    
    ggplot(obm_subset(), aes(x = factor(Variable, levels = c("Game_Poss", "Entropy", "Poss", "Back", "Stay", "Forward", "Goal")), 
                             y = MS, fill = zscore)) +
      geom_tile() +
      theme_bw() +
      scale_fill_gradient2(name = "Comparison to\nleague average", low = "purple", mid = "white", high = "green3") +
      geom_text(aes(label = value), size = 4.5) +
      ggtitle("DEFENCE (Opposition Attack)") +
      theme(plot.title = element_text(hjust = 0.5, size = 14), 
            axis.text.x = element_text(size = 14), 
            axis.text.y = element_text(size = 14), 
            strip.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12), 
            axis.title.y.left = element_blank()) +
      facet_grid(factor(Start, levels = c("Circle", "Corners", "DeepAtt", "BuildAtt", "BuildDef", "Outlet", "DeepDef"), 
                        labels = c("Circle", "Corners", "Deep Att", "Build Att", "Build Def", "Outlet", "Deep Def")) ~ .) +
      scale_x_discrete(name = "", labels = c("Game_Poss" = "Opp\nGame Poss (%)", 
                                             "Entropy" = "Opp\nEntropy", 
                                             "Poss" = "Opp\nPoss (%)", 
                                             "Back" = "Opp\nBack (%)", 
                                             "Stay" = "Opp\nStay (%)", 
                                             "Forward" = "Opp\nForward (%)", 
                                             "Goal" = "Opp\nGoal (%)"))
    
  }, height = 687.5, width = 825)
  
  ## PLOT 13 ##
  
  choices13 <- reactive({
    HM() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(HM(), {
    updateSelectInput(inputId = "team13", choices = choices13())
  })
  
  output$hmPlot <- renderPlot({
    
    hm_subset <- reactive({
      subset(HM(), Team == input$team13
             & MS == input$ms13)
    })
    
    ggplot(hm_subset(), aes(x = as.factor(Start.x), y = as.factor(Start.y), fill = Percent)) +
      geom_tile() + 
      theme(axis.text.x = element_text(angle = 360, size = 10)) +
      ggtitle("Possession")+
      theme(plot.title = element_text(hjust = 0.5, size = 14),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.y = element_text(size = 10)) +
      scale_fill_distiller(palette = "Reds", direction = 1) +
      xlab("x") +
      ylab("y") +
      geom_curve(aes(x = 1.5, y = 0.5, xend = 4.5, yend = 0.5), curvature = -0.8) +
      geom_curve(aes(x = 1.5, y = 8.5, xend = 4.5, yend = 8.5), curvature = 0.8) +
      geom_hline(yintercept = 4.5) +
      geom_hline(yintercept = 6.5) +
      geom_hline(yintercept = 2.5)
    
  }, height = 412.5, width = 343.75)
  
  ## PLOT 14 ## 
 
  choices14 <- reactive({
    E() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(E(), {
    updateSelectInput(inputId = "team13", choices = choices14())
  })
  
  output$entPlot <- renderPlot({
    
    e_subset <- reactive({
      subset(E(),Team == input$team13
             & MS == input$ms13)
    })
    
    ggplot(e_subset(), aes(x = as.factor(Start.x), y = as.factor(Start.y), fill = Entropy)) +
      geom_tile() + 
      theme(axis.text.x = element_text(angle = 360, size = 10),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.y = element_text(size = 10)) +
      ggtitle("Entropy") +
      theme(plot.title = element_text(hjust = 0.5, size = 14)) +
      scale_fill_distiller(palette = "Greens", direction = 1, na.value = "white") +
      xlab("x") +
      ylab("y") +
      geom_curve(aes(x = 1.5, y = 0.5, xend = 4.5, yend = 0.5), curvature = -0.8) +
      geom_curve(aes(x = 1.5, y = 8.5, xend = 4.5, yend = 8.5), curvature = 0.8) +
      geom_hline(yintercept = 4.5) +
      geom_hline(yintercept = 6.5) +
      geom_hline(yintercept = 2.5)
    
  }, height = 412.5, width = 343.75)
  
  ## PLOT 15 ## 
 
  choices15 <- reactive({
    Prog() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(Prog(), {
    updateSelectInput(inputId = "team13", choices = choices15())
  })
  
  output$progPlot <- renderPlot({
    
    
    prog_subset <- reactive({
      subset(Prog(), Team == input$team13
             & MS == input$ms13
             & Start == input$zone13)
    })
    
    ggplot(prog_subset(), aes(x = End.x, y = End.y, fill = Percent)) +
      geom_tile() + 
      theme(axis.text.x = element_text(angle = 360, size = 10)) +
      ggtitle("Progression Rates") +
      theme(plot.title = element_text(hjust = 0.5, size = 14),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.y = element_text(size = 10)) +
      scale_fill_distiller(palette = "Blues", direction = 1) +
      xlab("x") +
      ylab("y") +
      geom_curve(aes(x = 1.5, y = 0.5, xend = 4.5, yend = 0.5), curvature = -0.8) +
      geom_curve(aes(x = 1.5, y = 8.5, xend = 4.5, yend = 8.5), curvature = 0.8) +
      geom_hline(yintercept = 4.5) +
      geom_hline(yintercept = 6.5) +
      geom_hline(yintercept = 2.5)
  }, height = 412.5, width = 343.75)
  
  ## PLOT 16 ## 
  
  choices16 <- reactive({
    OHM() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(OHM(), {
    updateSelectInput(inputId = "team16", choices = choices16())
  })
  
  output$ohmPlot <- renderPlot({
    
    ohm_subset <- reactive({
      subset(OHM(), Team == input$team16
             & MS == input$ms16)
    })
    
    ggplot(ohm_subset(), aes(x = as.factor(Start.x), y = as.factor(Start.y), fill = Percent)) +
      geom_tile() + 
      theme(axis.text.x = element_text(angle = 360, size = 10)) +
      ggtitle("Opposition Possession") +
      theme(plot.title = element_text(hjust = 0.5, size = 14),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.y = element_text(size = 10)) +
      scale_fill_distiller(palette = "Reds", direction = 1) +
      xlab("x") +
      ylab("y") +
      geom_curve(aes(x = 1.5, y = 0.5, xend = 4.5, yend = 0.5), curvature = -0.8) +
      geom_curve(aes(x = 1.5, y = 8.5, xend = 4.5, yend = 8.5), curvature = 0.8) +
      geom_hline(yintercept = 4.5) +
      geom_hline(yintercept = 6.5) +
      geom_hline(yintercept = 2.5)
  }, height = 412.5, width = 343.75)
  
  ## PLOT 16 ## 
  
  choices17 <- reactive({
    OE() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(OE(), {
    updateSelectInput(inputId = "team16", choices = choices17())
  })
  
  output$oentPlot <- renderPlot({
    
    oe_subset <- reactive({
      subset(OE(), Team == input$team16
             & MS == input$ms16)
    })
    
    ggplot(oe_subset(), aes(x = as.factor(Start.x), y = as.factor(Start.y), fill = Entropy)) +
      geom_tile() + 
      theme(axis.text.x = element_text(angle = 360, size = 10)) +
      ggtitle("Opposition Entropy") +
      theme(plot.title = element_text(hjust = 0.5, size = 14),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.y = element_text(size = 10)) +
      scale_fill_distiller(palette = "Greens", direction = 1, na.value = "white") +
      xlab("x") +
      ylab("y") +
      geom_curve(aes(x = 1.5, y = 0.5, xend = 4.5, yend = 0.5), curvature = -0.8) +
      geom_curve(aes(x = 1.5, y = 8.5, xend = 4.5, yend = 8.5), curvature = 0.8) +
      geom_hline(yintercept = 4.5) +
      geom_hline(yintercept = 6.5) +
      geom_hline(yintercept = 2.5)
    
  }, height = 412.5, width = 343.75)
  

  ## PLOT 18 ## 
  
  choices18 <- reactive({
    OProg() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(OProg(), {
    updateSelectInput(inputId = "team16", choices = choices18())
  })
  
  output$oprogPlot <- renderPlot({
    
    oprog_subset <- reactive({
      subset(OProg(),Team == input$team16
             & MS == input$ms16
             & Start == input$zone16)
    })
    
    ggplot(oprog_subset(), aes(x = End.x, y = End.y, fill = Percent)) +
      geom_tile() + 
      theme(axis.text.x = element_text(angle = 360, size = 10)) +
      ggtitle("Opposition Progression Rates") +
      theme(plot.title = element_text(hjust = 0.5, size = 14),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.text.y = element_text(size = 10)) +
      scale_fill_distiller(palette = "Blues", direction = 1) +
      xlab("x") +
      ylab("y") +
      geom_curve(aes(x = 1.5, y = 0.5, xend = 4.5, yend = 0.5), curvature = -0.8) +
      geom_curve(aes(x = 1.5, y = 8.5, xend = 4.5, yend = 8.5), curvature = 0.8) +
      geom_hline(yintercept = 4.5) +
      geom_hline(yintercept = 6.5) +
      geom_hline(yintercept = 2.5)
    
  }, height = 412.5, width = 343.75)
  
  ## PLOT 19 ## 
  
  choices19 <- reactive({
    GSN() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(GSN(), {
    updateSelectInput(inputId = "team19", choices = choices19())
  })
  
  output$gsnPlot <- renderPlot({
    
    gsn_subset <- reactive({
      subset(GSN(), Team == input$team19 )
      
    })
    
    ggplot(gsn_subset(), aes(x = Loc.x, y = Loc.y)) +
      geom_point(aes(size = Percent, fill = zscore), shape = 21, colour = "black") +
      xlim(0, 6) +
      ylim(-0.27, 3) +
      geom_curve(aes(x = 0, y = 0, xend = 6, yend = 0), curvature = -0.9) +
      geom_curve(aes(x = 1, y = 0, xend = 5, yend = 0),colour = "Grey", curvature = -0.9) +
      geom_curve(aes(x = 2, y = 0, xend = 4, yend = 0), colour = "Grey", curvature = -0.9) +
      geom_hline(yintercept = 0) +
      xlab("") +
      ylab("") +
      scale_fill_gradient2(name = "Comparison to\nleague average", low = "purple", mid = "white", high = "green3", midpoint = 0) +
      geom_segment(aes(x = 2.6, y = 0, xend = 2.6, yend = -0.25)) +
      geom_segment(aes(x = 3.4, y = 0, xend = 3.4, yend = -0.25)) +
      geom_segment(aes(x = 2.6, y = -0.25, xend = 3.4, yend = -0.25)) +
      theme(panel.background = element_blank(), 
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            axis.ticks = element_blank(), 
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(), 
            plot.title = element_text(size = 24, vjust = -10), 
            plot.subtitle = element_text(size = 14, vjust = -18)) +
      labs(title = "Attempts", subtitle = "(Shots/Location)") +
      scale_size(range = c(4, 12)) +
      guides(size = guide_legend(order = 1), fill = guide_colourbar(order = 2))
    
  }, height = 400, width = 610)
  
  ## PLOT 20 ## 
  
  choices20 <- reactive({
    GSO() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(GSO(), {
    updateSelectInput(inputId = "team19", choices = choices20())
  })
  
  output$gsoPlot <- renderPlot({
    
    gso_subset <- reactive({
      subset(GSO(), Team == input$team19 
             & Outcome == input$outcome19)
    })
    
    ggplot(gso_subset(), aes(x = Loc.x, y = Loc.y)) +
      geom_point(aes(size = Percent, fill = zscore), shape = 21, colour = "black") +
      xlim(0, 6) +
      ylim(-0.27, 3) +
      geom_curve(aes(x = 0, y = 0, xend = 6, yend = 0),  curvature = -0.9) +
      geom_curve(aes(x = 1, y = 0, xend = 5, yend = 0),colour = "Grey", curvature = -0.9) +
      geom_curve(aes(x = 2, y = 0, xend = 4, yend = 0), colour = "Grey", curvature = -0.9) +
      geom_hline(yintercept = 0) +
      xlab("") +
      ylab("") +
      scale_fill_gradient2(name = "Comparison to\nleague average", low = "purple", mid = "white", high = "green3", midpoint = 0) +
      geom_segment(aes(x = 2.6, y = 0, xend = 2.6, yend = -0.25)) +
      geom_segment(aes(x = 3.4, y = 0, xend = 3.4, yend = -0.25)) +
      geom_segment(aes(x = 2.6, y = -0.25, xend = 3.4, yend = -0.25)) +
      theme(panel.background = element_blank(),  
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            axis.ticks = element_blank(), 
            axis.text.x = element_blank(), 
            axis.text.y = element_blank(), 
            plot.title = element_text(size = 24, vjust =-10), 
            plot.subtitle = element_text(size = 14, vjust =-18)) +
      labs(title = input$outcome19, subtitle = "(Outcome/Shots per Location)") +
      scale_size(range = c(4, 12)) +
      guides(size = guide_legend(order = 1), fill = guide_colourbar(order = 2))
    
  }, height = 400, width = 610)
  
  ## PLOT 21 ##
 
  choices21 <- reactive({
    GSE() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(GSE(), {
    updateSelectInput(inputId = "team19", choices = choices21())
  })
  
  output$gsePlot <- renderPlot({
    
    gse_subset <- reactive({
      subset(GSE(), Team == input$team19)
    })
    
    ggplot(gse_subset(), aes(x = Loc.x, y = Loc.y)) +
      geom_point(aes(size = Efficiency, fill = zscore), shape = 21, 
                 colour = "black") +
      xlim(0, 6) +
      ylim(-0.27, 3) +
      geom_curve(aes(x = 0, y = 0, xend = 6, yend = 0),  curvature = -0.9) +
      geom_curve(aes(x = 1, y = 0, xend = 5, yend = 0), colour = "Grey", curvature = -0.9) +
      geom_curve(aes(x = 2, y = 0, xend = 4, yend = 0), colour = "Grey", curvature = -0.9) +
      geom_hline(yintercept = 0) +
      xlab("") +
      ylab("") +
      scale_fill_gradient2(name = "Comparison to\nleague average", low = "purple", mid = "white", high = "green3", midpoint = 0) +
      geom_segment(aes(x = 2.6, y = 0, xend = 2.6, yend = -0.25)) +
      geom_segment(aes(x = 3.4, y = 0, xend = 3.4, yend = -0.25)) +
      geom_segment(aes(x = 2.6, y = -0.25, xend = 3.4, yend = -0.25)) +
      theme(panel.background = element_blank(),  
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12),
            axis.ticks = element_blank(), 
            axis.text.x = element_blank(), axis.text.y = element_blank(), 
            plot.title = element_text(size = 24, vjust = -10), 
            plot.subtitle = element_text(size =14, vjust = -18)) +
      labs(title = "Efficiency", subtitle = "(Goals/Total Shots all Locations)") +
      scale_size(range = c(4, 12)) +
      guides(size = guide_legend(order = 1), fill = guide_colourbar(order = 2))
    
  }, height = 400, width = 610)
  
  ## PLOT 22 ## 
 
  choices22 <- reactive({
    SP() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(SP(), {
    updateSelectInput(inputId = "team22", choices = choices22())
  })
  
  output$pcPlot <- renderPlot({
    
    sp_subset <- reactive({
      subset(SP(), Team == input$team22)
    })
    
    ggplot(sp_subset(), aes(Pattern, Percent, group = factor(Outcome, levels = c("P","G", "Sa", "Sm", "T")))) + 
      geom_col(position = position_dodge(width = 1),  aes(fill = factor(Outcome, levels = c("P", "G", "Sa", "Sm", "T")))) +
      scale_x_discrete(labels = c("")) +
      ylab("Goal Shot Outcome (%)") +
      scale_fill_manual(name = "Outcome", values = c("black", "purple4", "lightslateblue", "navy blue", "red"),  
                        labels = c( "Total Shots", "Goal", "Saved", "Smothered", "Turnover")) +
      facet_grid(. ~ factor(Pattern, levels = c("SS", "LD", "DefA"), 
                            labels = c("Straight", "Layoff", "Deflection")), 
                 space = "free", scales = "free", switch = "x") +
      theme_bw() +
      theme(axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(vjust = 2.5, size = 14),
            strip.text = element_text(size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            axis.text.y = element_text(size = 12))
    
  }, height = 375, width = 625)
  
  ## PLOT 23 ##
  
  choices23 <- reactive({
    GA() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(GA(), {
    updateSelectInput(inputId = "team23", choices = choices23())
  })
  
  output$gaPlot <- renderPlot({
    
    
    ga_subset <- reactive({
      subset(GA(), Team == input$team23 
             & Attack == input$attack23)
    })
    
    gaa_subset <- reactive({
      subset(GAA(), Attack == input$attack23)
    })
    
    ggplot() +
      geom_col(data = ga_subset(), aes(x = Effect, y = Percent, fill = Effect), position = position_dodge(width = 1)) +
      geom_point(data = gaa_subset(), aes(Effect, y = Percent, shape = Effect)) +
      facet_grid(factor(Location, levels = c("AC", "A25", "A50", "D50", "D25")) ~ Action , space = "free", scales = "free") +
      theme_bw() +
      ylab("Movement Effect (%)") +
      theme(axis.ticks.x = element_blank(), 
            axis.title.x = element_blank(), 
            axis.text.x = element_blank(),
            axis.title.y = element_text(vjust = 2.5, size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            strip.text = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      scale_fill_manual(values = alpha(c("springgreen3", "gold1", "darkorchid"), 0.75)) +
      scale_shape_manual(name = "League Average", values = c(15, 16, 17)) +
      guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1))
    
  }, height = 625, width = 500)
  
  ## PLOT 24 ## 
  
  choices24 <- reactive({
    S() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(S(), {
    updateSelectInput(inputId = "team24", choices = choices24())
  })
  
  output$sPlot <- renderPlot({
    
    s_subset <- reactive({
      subset(S(), Team == input$team24 
             & Attack == input$attack24)
    })
    
    sa_subset <- reactive({
      subset(SA(), Attack == input$attack24)    
    })
    
    ggplot() +
      geom_col(data = s_subset(), aes(x = Stoppage, y = Percent, fill = Stoppage), position = position_dodge(width = 1)) +
      geom_point(data = sa_subset(), aes(Stoppage, y = Percent, shape = Stoppage)) +
      facet_grid(. ~ factor(Location, levels = c("AC", "A25", "A50", "D50", "D25")), space = "free", scales = "free") +
      theme_bw() +
      ylab("Stoppage Type (%)") +
      theme(axis.ticks.x = element_blank(), 
            axis.title.x = element_blank(), 
            axis.text.x = element_blank(),
            axis.title.y = element_text(vjust = 2.5, size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            strip.text = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      scale_fill_manual(values = alpha(c("lightgreen", "navy blue", "dodger blue", "darkturquoise", "lime green"), 0.75)) +
      scale_shape_manual(name = "League Average", values = c(0, 1, 15, 16, 17)) +
      guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1))
    
  }, height = 500, width = 625)
  
  ## PLOT 25 ##
  
  choices25 <- reactive({
    T() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(T(), {
    updateSelectInput(inputId = "team25", choices = choices25())
  })
  
  output$tPlot <- renderPlot({
    
    t_subset <- reactive({
      subset(T(), Team == input$team25 
             & Attack == input$attack25)
    })
    
    ta_subset <- reactive({
      subset(TA(), Attack == input$attack25)
    })
    
    ggplot() +
      geom_col(data = t_subset(), aes(x = Turnover, y = Percent, fill = Turnover), position = position_dodge(width = 1)) +
      geom_point(data = ta_subset(), aes(Turnover, y = Percent, shape = Turnover)) +
      facet_grid(. ~ factor(Location, levels = c("AC", "A25", "A50", "D50", "D25")), space = "free", scales = "free") +
      theme_bw() +
      ylab("Turnover Type (%)") +
      scale_fill_manual(values = alpha(c("red",  "gold", "purple3", "firebrick2",  "goldenrod1", "mediumorchid", "orange red", "yellow1", "mediumpurple1"), 0.75)) +
      scale_shape_manual(name = "League Average", values = c(0, 1, 2, 5, 6, 15, 16, 17, 18)) +
      theme(axis.ticks.x = element_blank(), 
            axis.title.x = element_blank(), 
            axis.text.x = element_blank(),
            axis.title.y = element_text(vjust = 2.5, size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            strip.text = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1))
    
  }, height = 500, width = 775)
  
  ## PLOT 26 ## 
 
  choices26 <- reactive({
    Time() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(Time(), {
    updateSelectInput(inputId = "team26", choices = choices26())
  })
  
  
  output$timePlot <- renderPlot({
    
    time_subset <- reactive({
      subset(Time(), Team == input$team26)
    })
    
    ggplot() +
      geom_col(data = time_subset(), position = position_dodge(width = 1), aes(x = factor(Match.Status, levels = c("D", "L", "W")), 
                                         y = Percent, fill = factor(Match.Status, levels = c("D", "L", "W"), labels = c("Drawing", "Losing", "Winning")))) +
      geom_point(data = TimeA(), aes(x = factor(Match.Status, levels = c("D", "L", "W")), 
                                     y = Percent, shape = Match.Status)) +
      facet_grid(. ~ factor(Time, levels = c("Less_Ten", "Ten_Twenty", "Twenty_Thirty", "Thirty_Forty", "Forty_Fifty", "Fifty_Sixty", "Sixty_Hundred", "Greater_Hundred"), 
                            labels = c("< 10", "10 - 20", "20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 100", "> 100")), 
                 scales = "free", space = "free", switch = "x") +
      theme_bw() +
      xlab("Possession Time (seconds)") +
      ylab("Number of Possessions (%)") +
      scale_fill_manual(name = "Match Status", values = alpha(c("dodger blue", "red", "lime green"), 0.75)) +
      scale_shape_manual(name = "League Average", values = c(15, 16, 17), labels = c("Drawing", "Losing", "Winning")) +
      theme(axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(vjust = 2.5, size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            strip.text = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1))
    
  }, height = 500, width = 750)
  
  ## PLOT 27 ## 
  
  choices27 <- reactive({
    Length() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(Length(), {
    updateSelectInput(inputId = "team27", choices = choices27())
  })
  
  
  output$lengthPlot <- renderPlot({
    
    length_subset <- reactive({
      subset(Length(),  Team == input$team27)  
    })
    
    ggplot() +
      geom_col(data = length_subset(), position = position_dodge(width = 1), aes(x = factor(Match.Status, levels = c("D", "L", "W")), 
                                           y = Percent, fill = factor(Match.Status, levels = c("D", "L", "W"), labels = c("Drawing", "Losing", "Winning")))) +
      geom_point(data = LengthA(), aes(x = factor(Match.Status, levels = c("D", "L", "W")), 
                                       y = Percent, shape = Match.Status)) +                                     
      facet_grid(. ~ factor(Length, levels = c("Less_Two", "Three_Five", "Six_Nine", "Ten_Fifteen", "Sixteen_Twenty", "TwentyOne_Thirty", "Greater_Thirty"), 
                            labels = c("< 2", "3 - 5", "6 - 9", "10 - 15", "16 - 20", "21 - 30", "> 30")), 
                 space = "free", scales = "free", switch = "x" ) +
      theme_bw() +
      xlab("Game Events per Possession") +
      ylab("Number of Possessions (%)") +
      scale_fill_manual(name = "Match Status", values = alpha(c("dodger blue", "red", "lime green"), 0.75)) +
      scale_shape_manual(name = "League Average", values = c(15, 16, 17), labels = c("Drawing", "Losing", "Winning")) +
      theme(axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(vjust = 2.5, size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            strip.text = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1))
    
  }, height = 500, width = 750)
  
  ## PLOT 28 ## 
  
  choices28 <- reactive({
    Rate() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(Rate(), {
    updateSelectInput(inputId = "team28", choices = choices28())
  })
  
  output$ratePlot <- renderPlot({
    
    rate_subset <- reactive({
      subset(Rate(),  Team == input$team28) 
      })
    
    ggplot() +
      geom_col(data = rate_subset(), position = position_dodge(width = 1), aes(x = factor(Match.Status, levels = c("D", "L", "W")), 
                                         y = Percent, fill = factor(Match.Status, levels = c("D", "L", "W"), labels = c("Drawing", "Losing", "Winning")))) +
      geom_point(data = RateA(), aes(x = factor(Match.Status, levels = c("D", "L", "W")), 
                                     y = Percent, shape = Match.Status)) +
      facet_grid(. ~ factor(Rate, levels = c("Less_Two", "Two_Three", "Three_Four", "Five_Eight", "Nine_Twelve", "Greater_Twelve"), 
                            labels = c("< 2", "2 - 3", "3 - 4", "5 - 8", "9 - 12", "> 12")), 
                 scales = "free", space = "free", switch = "x") +
      theme_bw() +
      xlab("Time per Game Event (seconds)") +
      ylab("Number of Possessions (%)") +
      scale_fill_manual(name = "Match Status", values = alpha(c("dodger blue", "red", "lime green"), 0.75)) +
      scale_shape_manual(name = "League Average", values = c(15, 16, 17), labels = c("Drawing", "Losing", "Winning")) +
      theme(axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(vjust = 2.5, size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            strip.text = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1))
    
  }, height = 500, width = 750)
  
  ## PLOT 29 ## 
  
  choices29 <- reactive({
    Start() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(Start(), {
    updateSelectInput(inputId = "team29", choices = choices29())
  })
  
  output$startPlot <- renderPlot({
    
    start_subset <- reactive({
      subset(Start(),Team == input$team29)
    })
    
    ggplot() +
      geom_col(data = start_subset(), position = position_dodge(width = 1), aes(x = factor(Match.Status, levels = c("D", "L", "W")), 
                                          y = Percent, fill = factor(Match.Status, levels = c("D", "L", "W"), labels = c("Drawing", "Losing", "Winning")))) +
      geom_point(data = StartA(), aes(x = factor(Match.Status, levels = c("D", "L", "W")), 
                                      y = Percent, shape = Match.Status)) +
      facet_grid(. ~ factor(Location, levels = c("AC", "A25", "A50", "D50", "D25")), scales = "free", space = "free", switch = "x") +
      xlab("Start Location") +
      ylab("Number of Possessions (%)") +
      theme_bw() +
      scale_fill_manual(name = "Match Status", values = alpha(c("dodger blue", "red", "lime green"), 0.75)) +
      scale_shape_manual(name = "League Average", values = c(15, 16, 17), labels = c("Drawing", "Losing", "Winning")) +
      theme(axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(vjust = 2.5, size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            strip.text = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1))
    
  }, height = 500, width = 750)
  
  ## PLOT 30 ## 
 
  choices30 <- reactive({
    End() %>%
      select(Team) %>%
      unique()
  })
  
  observeEvent(End(), {
    updateSelectInput(inputId = "team30", choices = choices30())
  })
  
 
  output$endPlot <- renderPlot({
    
    end_subset <- reactive({
      subset(End(), Team == input$team30)
    })
    
    ggplot() +
      geom_col(data = end_subset(), position = position_dodge(width = 1), aes(x = factor(End, levels = c("GS", "AC", "A25", "A50", "D50", "D25")), 
                                        y = Percent, fill = factor(End, levels = c("GS", "AC", "A25", "A50", "D50", "D25")))) +
      geom_point(data = EndA(), aes(x = factor(End, levels = c("GS", "AC", "A25", "A50", "D50", "D25")), 
                                    y = Percent, shape = Match.Status)) +
      facet_grid(factor(Match.Status, levels = c("D", "L", "W"), labels = c("Drawing", "Losing", "Winning")) ~ factor(Start, levels = c("AC", "A25", "A50", "D50", "D25")), 
                 scales = "free", space = "free", switch = "x") +
      xlab("Start Location") +
      ylab("Number of Possessions (%)") +
      theme_bw() +
      scale_fill_manual(name = "End Location", values = alpha(c("yellow1", "steelblue1",  "blueviolet", "gold1",  "navy blue", "purple4" ), 0.75)) +
      scale_shape_manual(name = "League Average", values = c(15, 16, 17), labels = c("Drawing", "Losing", "Winning")) +
      theme(axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(),
            axis.title.y = element_text(vjust = 2.5, size = 14),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            strip.text = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.text.y = element_text(size = 12)) +
      guides(shape = guide_legend(order = 2), fill = guide_legend(order = 1))
    
  }, height = 625, width = 775)
  
})