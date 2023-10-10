library(tidyverse)
library(openxlsx)

data<-read.xlsx("CEPSalesInExcel.xlsx", detectDates = T)
gl <- data %>% filter(str_detect(Description, "Glowing Lamp"))
inv<-unique(gl$Invoice)
final<-data %>% filter(Invoice %in% inv)
write.xlsx(final, "glowinglamp.xlsx")