library(tidyverse)
library(googlesheets4)
gs4_deauth()


gs<-"https://docs.google.com/spreadsheets/d/1_ubdpCZufsnNzJeyEY5M4yi9y3pfWkkPtFy-PnWO4Ws/edit?usp=sharing"
data<-read_sheet(gs)
logi<-read_sheet(gs, sheet="logi")
kevin<-read_sheet(gs, sheet="kevin")

non_overlap<-setdiff(data$logi, data$kevin)

logi_diff<- logi %>%
  filter(Index %in% non_overlap)

kevin_diff<- kevin %>%
  filter(FUND %in% non_overlap)