library(tidyverse)
library(googlesheets4)
library(shiny)
library(ggpubr)
library(scales)
library(shinydashboard) 

#read in data
gs4_deauth()
colleges<-read_sheet("1dyVdnanuJrXMN8f5SGxd88Hr5zQeBWpt35N6zoLda1g")
data<-read_sheet("17lOrKyxUMuFOicVXIeGVHY_8UdhCsIHp48VXz6igrEQ") %>%
  select(2:17)

#assign column headers
names(data) = c("gender","age","rank","admin","dept","familiar","ever_used","attitude","learning","pedagogy","workload","engagement","equity","challenges","benefits","feedback")

#organize factor levels
data<-data %>%
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

proptable<- function(v){
  data %>%
    group_by({{v}}) %>%
    summarize(num=n(),
              prop=n()/nrow(data)*100,
              pct=percent(prop*100, scale=0.01, accuracy=0.01))
}

#draw and format gender graphs
gender_props<-data %>%
  group_by(gender) %>%
  summarize(num=n(),
            prop=n()/nrow(data)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))


genderpie<-ggpie(gender_props,x="prop", label="pct", fill="gender",color=NULL, lab.pos="in", lab.font=c("bold","black"), lab.adjust=-2)+
  scale_fill_brewer(palette="Set1")+
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

genderplot<-ggplot(gender_props, aes(x=gender, y=num, fill=gender))+
  geom_col()+
  geom_text(aes(label=num), vjust=1, color="black", fontface="bold", size=6)+
  theme_classic()+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position = "none") +
  theme(text = element_text(size = 14))+
  labs(x=NULL,
       y="Count")