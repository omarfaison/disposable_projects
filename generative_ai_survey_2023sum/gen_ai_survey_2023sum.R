#setwd("D:/code/disposable_projects/generative_ai_survey_2023sum")
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

#draw and format gender graphs
gender_props<-reformat %>%
  group_by(gender) %>%
  summarize(num=n(),
    prop=n()/nrow(reformat)*100,
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

#draw and format age graphs
age_props<-reformat %>%
  group_by(age) %>%
  summarize(num=n(),
            prop=n()/nrow(reformat)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))


agepie<-ggpie(age_props,x="prop", label="pct", fill="age",color=NULL, lab.pos="in", lab.font=c("bold","black"), lab.adjust=-2)+
  scale_fill_brewer(palette="Set1")+
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

ageplot<-ggplot(age_props, aes(x=age, y=num, fill=age))+
  geom_col()+
  geom_text(aes(label=num), vjust=1, color="black", fontface="bold", size=6)+
  theme_classic()+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position = "none")+
  theme(text = element_text(size = 14))+
  labs(x=NULL,
       y="Count")

#draw and format rank graphs
rank_props<-reformat %>%
  group_by(rank) %>%
  summarize(num=n(),
            prop=n()/nrow(reformat)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))


rankpie<-ggpie(rank_props,x="prop", label="pct", fill="rank",color=NULL, lab.pos="in", lab.font=c("bold","black"), lab.adjust=-2)+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="right",legend.box="vertical")+
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

rankplot<-ggplot(rank_props, aes(x=rank, y=num, fill=rank))+
  geom_col()+
  geom_text(aes(label=num), vjust=1, color="black", fontface="bold", size=6)+
  theme_classic()+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.25))+
  theme(legend.position = "none")+
  theme(text = element_text(size = 14))+
  labs(x=NULL,
       y="Count")

#draw and format college graphs
college_props<-reformat %>%
  group_by(college) %>%
  summarize(num=n(),
            prop=n()/nrow(reformat)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))


collegepie<-ggpie(college_props,x="prop", label="pct", fill="college",color=NULL, lab.pos="in", lab.font=c("bold","black"), lab.adjust=-2)+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="right",legend.box="vertical") +
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

collegeplot<-ggplot(college_props, aes(x=college, y=num, fill=college))+
  geom_col()+
  geom_text(aes(label=num), vjust=1, color="black", fontface="bold", size=6)+
  theme_classic()+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.25))+
  theme(legend.position = "none")+
  theme(text = element_text(size = 14))+
  labs(x=NULL,
       y="Count")

#draw and format familiar graphs
familiar_props<-reformat %>%
  group_by(familiar) %>%
  summarize(num=n(),
            prop=n()/nrow(reformat)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))


familiarpie<-ggpie(familiar_props,x="prop", label="pct", fill="familiar",color=NULL, lab.pos="out", lab.font=c("bold","black"), lab.adjust=-2)+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="right",legend.box="vertical")+
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

familiarplot<-ggplot(familiar_props, aes(x=familiar, y=num, fill=familiar))+
  geom_col()+
  geom_text(aes(label=num), vjust=1, color="black", fontface="bold", size=6)+
  theme_classic()+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.25))+
  theme(legend.position = "none")+
  theme(text = element_text(size = 14))+
  labs(x=NULL,
       y="Count")

#draw and format ever_used graphs
ever_used_props<-reformat %>%
  group_by(ever_used) %>%
  summarize(num=n(),
            prop=n()/nrow(reformat)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))


everpie<-ggpie(ever_used_props,x="prop", label="pct", fill="ever_used",color=NULL, lab.pos="out", lab.font=c("bold","black"), lab.adjust=2)+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="right",legend.box="vertical")+
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

everplot<-ggplot(ever_used_props, aes(x=ever_used, y=num, fill=ever_used))+
  geom_col()+
  geom_text(aes(label=num), vjust=1, color="black", fontface="bold", size=6)+
  theme_classic()+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.position = "none")+
  theme(text = element_text(size = 14))+
  labs(x=NULL,
       y="Count")

#draw and format attitude graphs
attitude_props<-reformat %>%
  group_by(attitude) %>%
  summarize(num=n(),
            prop=n()/nrow(reformat)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))

attitudepie<-ggpie(attitude_props,x="prop", label="pct", fill="attitude",color=NULL, lab.pos="out", lab.font=c("bold","black"), lab.adjust=-2)+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="right",legend.box="vertical")+
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

attitudeplot<-ggplot(attitude_props, aes(x=attitude, y=num, fill=attitude))+
  geom_col()+
  geom_text(aes(label=num), vjust=1, color="black", fontface="bold",size=6)+
  theme_classic()+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.25))+
  theme(legend.position = "none")+
  theme(text = element_text(size = 14))+
  labs(x=NULL,
       y="Count")

#draw and format learning graphs
learning_props<-reformat %>%
  group_by(learning) %>%
  summarize(num=n(),
            prop=n()/nrow(reformat)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))

learningplot<-ggplot(learning_props, aes(x=learning, y=num, fill=as.factor(learning)))+
  geom_col()+
  geom_text(aes(label=num), vjust=1, color="black", fontface="bold", size=6)+
  theme_classic()+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand=c(0,0), breaks=pretty_breaks())+
  theme(legend.position = "none")+
  labs(title="Student Learning Outcomes",
       x=NULL,
       y="Count")+
  theme(text = element_text(size = 14))

#draw and format pedagogy graphs
pedagogy_props<-reformat %>%
  group_by(pedagogy) %>%
  summarize(num=n(),
            prop=n()/nrow(reformat)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))

pedagogyplot<-ggplot(pedagogy_props, aes(x=pedagogy, y=num, fill=as.factor(pedagogy)))+
  geom_col()+
  geom_text(aes(label=num), vjust=1, color="black", fontface="bold", size=6)+
  theme_classic()+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand=c(0,0), breaks=pretty_breaks())+
  theme(legend.position = "none")+
  labs(title="Pedagogical approaches",
       x=NULL,
       y="Count")+
  theme(text = element_text(size = 14))


#draw and format workload graphs
workload_props<-reformat %>%
  group_by(workload) %>%
  summarize(num=n(),
            prop=n()/nrow(reformat)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))

workloadplot<-ggplot(workload_props, aes(x=workload, y=num, fill=as.factor(workload)))+
  geom_col()+
  geom_text(aes(label=num), vjust=1, color="black", fontface="bold", size=6)+
  theme_classic()+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand=c(0,0), breaks=pretty_breaks())+
  theme(legend.position = "none")+
  labs(title="Faculty workload",
       x=NULL,
       y="Count")+
  theme(text = element_text(size = 14))


#draw and format engagement graphs
engagement_props<-reformat %>%
  group_by(engagement) %>%
  summarize(num=n(),
            prop=n()/nrow(reformat)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))

engagementplot<-ggplot(engagement_props, aes(x=engagement, y=num, fill=as.factor(engagement)))+
  geom_col()+
  geom_text(aes(label=num), vjust=1, color="black", fontface="bold", size=6)+
  theme_classic()+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand=c(0,0), breaks=pretty_breaks())+
  theme(legend.position = "none")+
  labs(title="Student engagement",
       x=NULL,
       y="Count")+
  theme(text = element_text(size = 14))


#draw and format equity graphs
equity_props<-reformat %>%
  group_by(equity) %>%
  summarize(num=n(),
            prop=n()/nrow(reformat)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))

equityplot<-ggplot(equity_props, aes(x=equity, y=num, fill=as.factor(equity)))+
  geom_col()+
  geom_text(aes(label=num), vjust=1, color="black", fontface="bold", size=6)+
  theme_classic()+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(expand=c(0,0), breaks=pretty_breaks())+
  theme(legend.position = "none")+
  labs(title="Equity and inclusivity in education",
       x=NULL,
       y="Count")+
  theme(text = element_text(size = 14))
