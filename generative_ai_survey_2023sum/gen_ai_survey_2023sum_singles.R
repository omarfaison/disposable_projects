#draw and format gender graphs
gender_props<-raw %>%
  group_by(gender) %>%
  summarize(num=n(),
            prop=n()/nrow(raw)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))


gender_pie<-ggpie(gender_props,x="prop", label="pct", fill="gender",color=NULL, lab.pos="in", lab.font=c("bold","black"), lab.adjust=-2)+
  scale_fill_brewer(palette="Set1")+
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

gender_plot<-ggplot(gender_props, aes(x=gender, y=num, fill=gender))+
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
age_props<-raw %>%
  group_by(age) %>%
  summarize(num=n(),
            prop=n()/nrow(raw)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))


age_pie<-ggpie(age_props,x="prop", label="pct", fill="age",color=NULL, lab.pos="in", lab.font=c("bold","black"), lab.adjust=-2)+
  scale_fill_brewer(palette="Set1")+
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

age_plot<-ggplot(age_props, aes(x=age, y=num, fill=age))+
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
rank_props<-raw %>%
  group_by(rank) %>%
  summarize(num=n(),
            prop=n()/nrow(raw)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))


rank_pie<-ggpie(rank_props,x="prop", label="pct", fill="rank",color=NULL, lab.pos="in", lab.font=c("bold","black"), lab.adjust=-2)+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="right",legend.box="vertical")+
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

rank_plot<-ggplot(rank_props, aes(x=rank, y=num, fill=rank))+
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
college_props<-raw %>%
  group_by(college) %>%
  summarize(num=n(),
            prop=n()/nrow(raw)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))


college_pie<-ggpie(college_props,x="prop", label="pct", fill="college",color=NULL, lab.pos="in", lab.font=c("bold","black"), lab.adjust=-2)+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="right",legend.box="vertical") +
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

college_plot<-ggplot(college_props, aes(x=college, y=num, fill=college))+
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
familiar_props<-raw %>%
  group_by(familiar) %>%
  summarize(num=n(),
            prop=n()/nrow(raw)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))


familiar_pie<-ggpie(familiar_props,x="prop", label="pct", fill="familiar",color=NULL, lab.pos="out", lab.font=c("bold","black"), lab.adjust=-2)+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="right",legend.box="vertical")+
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

familiar_plot<-ggplot(familiar_props, aes(x=familiar, y=num, fill=familiar))+
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
ever_used_props<-raw %>%
  group_by(ever_used) %>%
  summarize(num=n(),
            prop=n()/nrow(raw)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))


ever_pie<-ggpie(ever_used_props,x="prop", label="pct", fill="ever_used",color=NULL, lab.pos="out", lab.font=c("bold","black"), lab.adjust=2)+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="right",legend.box="vertical")+
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

ever_plot<-ggplot(ever_used_props, aes(x=ever_used, y=num, fill=ever_used))+
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
attitude_props<-raw %>%
  group_by(attitude) %>%
  summarize(num=n(),
            prop=n()/nrow(raw)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))

attitude_pie<-ggpie(attitude_props,x="prop", label="pct", fill="attitude",color=NULL, lab.pos="out", lab.font=c("bold","black"), lab.adjust=-2)+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="right",legend.box="vertical")+
  theme(text = element_text(size = 14)) +
  labs(fill=NULL)

attitude_plot<-ggplot(attitude_props, aes(x=attitude, y=num, fill=attitude))+
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
learning_props<-raw %>%
  group_by(learning) %>%
  summarize(num=n(),
            prop=n()/nrow(raw)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))

learning_plot<-ggplot(learning_props, aes(x=learning, y=num, fill=as.factor(learning)))+
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
pedagogy_props<-raw %>%
  group_by(pedagogy) %>%
  summarize(num=n(),
            prop=n()/nrow(raw)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))

pedagogy_plot<-ggplot(pedagogy_props, aes(x=pedagogy, y=num, fill=as.factor(pedagogy)))+
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
workload_props<-raw %>%
  group_by(workload) %>%
  summarize(num=n(),
            prop=n()/nrow(raw)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))

workload_plot<-ggplot(workload_props, aes(x=workload, y=num, fill=as.factor(workload)))+
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
engagement_props<-raw %>%
  group_by(engagement) %>%
  summarize(num=n(),
            prop=n()/nrow(raw)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))

engagement_plot<-ggplot(engagement_props, aes(x=engagement, y=num, fill=as.factor(engagement)))+
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
equity_props<-raw %>%
  group_by(equity) %>%
  summarize(num=n(),
            prop=n()/nrow(raw)*100,
            pct=percent(prop*100, scale=0.01, accuracy=0.01))

equity_plot<-ggplot(equity_props, aes(x=equity, y=num, fill=as.factor(equity)))+
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