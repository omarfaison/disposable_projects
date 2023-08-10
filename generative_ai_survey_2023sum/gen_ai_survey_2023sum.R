library(tidyverse)
library(googlesheets4)
library(shiny)
library(ggpubr)
library(scales)
library(shinydashboard) 

#read in data
gs4_deauth()
colleges<-read_sheet("1dyVdnanuJrXMN8f5SGxd88Hr5zQeBWpt35N6zoLda1g")
raw<-read_sheet("17lOrKyxUMuFOicVXIeGVHY_8UdhCsIHp48VXz6igrEQ") %>%
  select(2:17)

#assign column headers
names(raw) = c("gender","age","rank","admin","dept","familiar","ever_used","attitude","learning","pedagogy","workload","engagement","equity","challenges","benefits","feedback")

#organize factor levels
reformat<-select(raw, gender:equity) %>%
  mutate(gender=factor(gender, levels=c("Male", "Female", "Non-binary", "Prefer not to say")))%>%
  mutate(age=factor(age, levels=c("Under 30","30-39", "40-49", "50-59", "60 or above")))%>%
  mutate(rank=factor(rank, levels=c("Adjunct/instructor","Assistant professor","Associate professor","Professor","Administrative/other role"))) %>%
  mutate(admin=factor(admin))%>%
  mutate(familiar=factor(familiar, levels=c("Not familiar at all","Not very familiar","Somewhat familiar","Very familiar"))) %>%
  mutate(ever_used=factor(ever_used))%>%
  mutate(attitude=factor(attitude, levels=c("Extremely negative","Negative","Neutral","Positive","Extremely positive"))) %>%
  left_join(colleges, by="dept") %>%
  mutate(college=factor(case_when(!is.na(dept) ~ college,
                                  TRUE ~ "None stated")))

ui <- fluidPage(
  titlePanel("Results from the College Faculty Survey on ChatGPT Usage"),
  fluidRow(column(12, "What is your overall attitude towards using generative AI in educational settings?")),
  fluidRow(
    box(width=6, plotOutput('attitudepie_all')),
    box(width=6, plotOutput('attitudeplot_all'))),
  fluidRow(column(12, "How would you rate your familiarity with generative AI?")),
  fluidRow(
    box(width=6, plotOutput('familiarpie_all')),
    box(width=6, plotOutput('familiarplot_all'))),
  fluidRow(column(12, "Have you ever used generative AI in any capacity (teaching, research, personal use, etc)?")),
  fluidRow(
    box(width=6, plotOutput('everpie_all')),
    box(width=6, plotOutput('everplot_all'))),
  fluidRow(column(12, "How do you perceive generative AI's potential in each of the following (1=very low impact, 5=very high impact)")),
  fluidRow(
    box(width=6, plotOutput('learningplot_all')),
    box(width=6, plotOutput('pedagogyplot_all'))),
  fluidRow(
    box(width=6, plotOutput('workloadplot_all')),
    box(width=6, plotOutput('engagementplot_all'))),
  fluidRow(
    box(width=3, ""),
    box(width=6, plotOutput('equityplot_all')),
    box(width=3, "")),
  fluidRow(column(12,dataTableOutput("challengesTable"))),
  fluidRow(column(12,dataTableOutput("benefitsTable"))),
  fluidRow(column(12,dataTableOutput("feedbackTable")))
)


server<-function(input, output) {
  source("gen_ai_survey_2023sum_singles.R")
  output$attitudepie_all<-renderPlot(attitudepie)
  output$attitudeplot_all<-renderPlot(attitudeplot)
  output$familiarpie_all<-renderPlot(familiarpie)
  output$familiarplot_all<-renderPlot(familiarplot)
  output$everpie_all<-renderPlot(everpie)
  output$everplot_all<-renderPlot(everplot)
  
  output$learningplot_all<-renderPlot(learningplot)
  output$pedagogyplot_all<-renderPlot(pedagogyplot)
  output$workloadplot_all<-renderPlot(workloadplot)
  output$engagementplot_all<-renderPlot(engagementplot)
  output$equityplot_all<-renderPlot(equityplot)
  
  output$challengesTable<-renderDataTable(as.data.frame(raw$challenges)) 
  output$benefitsTable<-renderDataTable(as.data.frame(raw$benefits)) 
  output$feedbackTable<-renderDataTable(as.data.frame(raw$feedback)) 
}

shinyApp(ui, server) 
