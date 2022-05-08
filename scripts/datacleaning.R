
data<- load("/Users/nicholasparker/Dropbox/JPSM/MWDS/Assignment4/WV6_Data_R_v20201117.rdata")

library(dplyr)
library(tidyr)
library(tidyverse)
library(psych)
library(purrr)

dir.create("scripts")

data<- WV6_Data_R_v20201117 %>%
  select(C_COW_ALPHA,V2,V228A:V228I,V217:V224,V192:V197)

write.csv(data,"/Users/nicholasparker/Dropbox/Brian/data.csv")

describe(data)

democracy <- data %>%
  group_by(C_COW_ALPHA) %>%
  summarise_each(funs(mean(., na.rm=TRUE))) %>%
  select(C_COW_ALPHA,V228A:V228I) %>%
  rename("fair_vote"=V228A) %>%
  rename("opposition_candidate"=V228B) %>%
  rename("new_favors_governing"=V228C) %>%
  rename("voters_bribed"=V228D) %>%
  rename("coverage_fair"=V228E) %>%
  rename("officials_fair"=V228F) %>%
  rename("rich_buy_elections"=V228G) %>%
  rename("voters_threatened"=V228H) %>%
  rename("voters_given_choice"=V228I) %>%
  filter(fair_vote!="NaN") %>%
  as.data.frame() %>%
  pivot_longer(
    cols = "fair_vote":"voters_given_choice",
    names_to = "var", 
    values_to = "value"
  )



science_tech <- data %>%
  group_by(C_COW_ALPHA) %>%
  summarise_each(funs(mean(., na.rm=TRUE))) %>%
  select(C_COW_ALPHA,V192:V197) %>%
  rename("science_life_easier"=V192) %>%
  rename("science_more_opportunities"=V193) %>%
  rename("depend_too_much"=V194) %>%
  rename("breaks_down_ideas"=V195) %>%
  rename("not_important_dailylife"=V196) %>%
  rename("better_worse_off"=V197) %>%
  pivot_longer(
    cols = "science_life_easier":"better_worse_off",
    names_to = "var", 
    values_to = "value"
  )

information <- data %>%
   group_by(C_COW_ALPHA) %>%
   count() %>%
   mutate(freq = n / sum(n))
   select(C_COW_ALPHA,V217:V224) 


# information_all <- data %>%
#   select(-C_COW_ALPHA) %>%
#   summarise(n = n()) %>%
#   mutate(freq = n / sum(n)) %>%
#   select(V217:V224) 

democracy_all <- data %>%
  select(-C_COW_ALPHA) %>%
  summarise_each(funs(mean(., na.rm=TRUE))) %>%
  select(V228A:V228I) %>%
  rename("fair_vote"=V228A) %>%
  rename("opposition_candidate"=V228B) %>%
  rename("new_favors_governing"=V228C) %>%
  rename("voters_bribed"=V228D) %>%
  rename("coverage_fair"=V228E) %>%
  rename("officials_fair"=V228F) %>%
  rename("rich_buy_elections"=V228G) %>%
  rename("voters_threatened"=V228H) %>%
  rename("voters_given_choice"=V228I)

democracy_all_df <- as.data.frame(democracy_all) %>%
  pivot_longer(
    cols = V228A:V228I,
    names_to = "var", 
    values_to = "value")

democracy_all_df$var <- c("fair_vote","opposition_candidate","new_favors_governing","voters_bribed","coverage_fair","officials_fair","rich_buy_elections","voters_threatened","voters_given_choice")

science_tech_all <- data %>%
  select(-C_COW_ALPHA) %>%
  summarise_each(funs(mean(., na.rm=TRUE))) %>%
  select(V192:V197) %>%
  rename("science_life_easier"=V192) %>%
  rename("science_more_opportunities"=V193) %>%
  rename("depend_too_much"=V194) %>%
  rename("breaks_down_ideas"=V195) %>%
  rename("not_important_dailylife"=V196) %>%
  rename("better_worse_off"=V197)
  
science_tech_all_df <- as.data.frame(science_tech_all) %>%
  pivot_longer(
    cols = V192:V197,
    names_to = "var", 
    values_to = "value"
  )

science_tech_all_df$var<- c("science_life_easier","science_more_opportunities","depend_too_much","breaks_down_ideas","not_important_dailylife","better_worse_off")

table1<- as.data.frame(round(table(data$V217)/length(data$V217),2))
table2<- as.data.frame(round(table(data$V218)/length(data$V218),2))
table3<- as.data.frame(round(table(data$V219)/length(data$V219),2))
table4<- as.data.frame(round(table(data$V220)/length(data$V220),2))
table5<- as.data.frame(round(table(data$V221)/length(data$V221),2))
table6<- as.data.frame(round(table(data$V222)/length(data$V222),2))
table7<- as.data.frame(round(table(data$V223)/length(data$V223),2))
table8<- as.data.frame(round(table(data$V224)/length(data$V224),2))

information_all<- rbind(c(table1,table2,table3,table4))

# map over each variable - do aggregate and group by each country 

country<- as.data.frame(science_tech$C_COW_ALPHA)%>%
  distinct()

library(tidyverse)
install.packages("shinythemes")

#filter for country

democracy_filtered<-democracy %>%
  filter(C_COW_ALPHA== "ALG") 

ggplot(democracy, aes_string(x="value",y="var"))+
  stat_summary(fun.y = sum, geom = "bar",colour="#56B4E9",fill="#56B4E9") +
  geom_bar(stat="identity") +
  theme_minimal()

ggplot(science_tech_all_df, aes_string(x="value",y="var"))  +
  stat_summary(fun.y = sum, geom = "bar",colour="#56B4E9",fill="#56B4E9") +
  geom_bar(stat="identity") +
  theme_minimal()

sessionInfo()
