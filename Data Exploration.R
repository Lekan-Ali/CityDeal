# load libraries
library(gplots)
library(tidyverse)
library(readxl)

#install.packages("effects")
#install.packages("HH")
library(HH)
library(effects)
library(rlang) # to create a function for ggplot 


# read in demographic dataset
df_demographic <- read_excel("Demographic.xlsx")

# read in activities dataset
df_activities <- read_excel("Activities.xlsx")

# read in outcome dataset
df_outcomes <- read_excel("Outcomes.xlsx")





# count the number of clients in each dataset


# 1.1 All three datasets have a common feature name (which is the ID of the client) but apparently this is names 'Unique ID' in the demographic and outcomes dataset and names 'ClientRef' in the Activities dataset. Hence we need to rename 'ClientRef' to 'Unique ID' so that it matches with the same name in the Demographic and outcomes sheet

names(df_activities)[1] <- "Unique ID"

# 1.2 create a function that picks only unique client and then add the name of the dataset to the final table
number_of_clients <- function(df, group, dataset) {
  df %>% group_by({{group}}) %>% 
    mutate(instances = row_number()) %>% # this code assigns a number to each instance or if a client exists 5 times in the datasets, each client will be given a number from 1 to 5
    filter(instances == 1) %>% # this picks only the first instance such that duplicates are excluded
    #dplyr::select({{group}}) %>% 
    mutate(dataset = {{dataset}}) %>% 
    dplyr::select({{group}}, dataset) # create a new column, the new column should have the name of the dataset
}

# 1.3 Demographic clients (unique clients)
df_demographic_clients <- number_of_clients(df_demographic,`Unique ID`,"Demographic")

# 1.4 Activities clients (unique clients)
df_activities_clients <- number_of_clients(df_activities,`Unique ID`, "Activities")


# 1.5 Outcomes clients (unique clients)
df_outcomes_clients <- number_of_clients(df_outcomes,`Unique ID`, "Outcomes")


# 2.1 combine the three tables such that they are ontop of each other
client_id_and_dataset <- rbind(df_demographic_clients, df_activities_clients, df_outcomes_clients)

# 2.2 
client_id_and_dataset$dataset <- factor(client_id_and_dataset$dataset
                                        , levels = c("Demographic", "Activities", "Outcomes"))

#3 count the number of clients in each dataset
clients_per_dataset <- client_id_and_dataset %>% 
  group_by(dataset) %>% 
  summarise(count = n())



# 4 plot
clients_per_dataset %>% 
  ggplot(aes(fct_rev(dataset), count)) +
  geom_col(fill = "grey70") + 
  geom_col(data = clients_per_dataset %>% filter(dataset == "Demographic")
           , fill = "#FF9999") + 
  coord_flip() +
  geom_text(data = clients_per_dataset %>% 
              filter(count > 5000), aes(label = scales::comma(count), hjust = 1.1)) +
  geom_text(data = clients_per_dataset %>% 
              filter(count < 5000), aes(label = scales::comma(count), hjust = -0.1)) +
  theme(panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.major.x = element_line(linetype = 2, color = "black", linewidth = 0.1)
        , axis.text = element_text(size = 12)) + 
  scale_y_continuous(expand = c(0,100)
                     , labels = scales::comma) +
  labs(y = "number of clients in each dataset"
       , x = "")




## Demographic Data Exploration ###
df_demographic_unique <- df_demographic %>% 
  group_by(`Unique ID`) %>% 
  mutate(instances = row_number()) %>% 
  filter(instances == 1)


# explore age distribution
ggplot(df_demographic_unique, aes(Age)) +
  geom_histogram(binwidth = 1, color = "white") +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120)) +
  theme(panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , panel.grid.major = element_line(linetype = 2, color = "black"
                                          , linewidth = 0.08)
        , axis.text = element_text(size = 12)
        , plot.caption = element_text(size=12, hjust=1, color="black", family = "mono")) +
  labs(y = "", caption = "Source: Demographic dataset") +
  annotate("text", x = 35, y = 450, label = "18 years Old", family = "mono") +
  annotate("segment", x = 26, xend = 19, y = 450, yend = 450,
           colour = "#FF9999", size = 1, arrow = arrow(length = unit(0.2, "inches")))


# explore age distribution by sex
# change sex from character column to a factor column
df_demographic_unique$Sex <- factor(df_demographic_unique$Sex
                                        , levels = c("Male", "Female", "Other Gender"
                                                     , "Prefer not to say", "Transgender"
                                                     , "Unknown"))

# plot age distribution using box plot
ggplot(df_demographic_unique, aes(Age, Sex)) +
  geom_boxplot() +
  coord_flip() +
  theme(panel.grid.minor.x = element_blank()
        , panel.grid.minor.y = element_blank()
        , panel.grid.major = element_line(linetype = 2, color = "black"
                                          , linewidth = 0.08)
        , axis.text = element_text(size = 12)
        , plot.caption = element_text(size=12, hjust=1, color="black", family = "mono")) +
  annotate("text", x = 46, y = 2.8, label = "Average (median)", family = "mono") +
  annotate("segment", x = 44, xend = 34, y = 2.4, yend = 2,
           colour = "#FF9999", size = 1, arrow = arrow(length = unit(0.2, "inches"))) +
  labs(y = "", caption = "Source: Demographic dataset")





## Exploring Outcomes

df_outcomes_unique <- df_outcomes %>% 
  group_by(`Unique ID`) %>% 
  mutate(instances = row_number()) %>% 
  filter(instances == 1)


df3 <- df_outcomes_unique %>% 
  mutate("Subsidy start date" = case_when(`Subsidy start date` == "NULL" ~ 0, TRUE ~ 1)
         , "Subsidy end date" = case_when(`Subsidy end date` == "NULL" ~ 0, TRUE ~ 1)
         , "Employment start date" = case_when(`Employment start date` == "NULL" ~ 0, TRUE ~ 1)
         , "Self-employment start date" = case_when(`Self-employment start date` == "NULL" ~ 0, TRUE ~ 1)
         , "Modern Apprenticeship start date" = case_when(`Modern Apprenticeship start date` == "NULL" ~ 0, TRUE ~ 1)
         , "Work experience start date" = case_when(`Work experience start date` == "NULL" ~ 0, TRUE ~ 1)
         , "Work experience completion date" = case_when(`Work experience completion date` == "NULL" ~ 0, TRUE ~ 1)
         , "Volunteering start date" = case_when(`Volunteering start date` == "NULL" ~ 0, TRUE ~ 1)
         , "Volunteering completion date" = case_when(`Volunteering completion date` == "NULL" ~ 0, TRUE ~ 1)
         , "LTU Accredited Training start date" = case_when(`LTU Accredited Training start date` == "NULL" ~ 0, TRUE ~ 1)
         , "Date Accredited Training qualification achieved (all participants)" = case_when(`Date Accredited Training qualification achieved (all participants)` == "NULL" ~ 0, TRUE ~ 1)
         , "Further / Higher Education start date" = case_when(`Further / Higher Education start date` == "NULL" ~ 0, TRUE ~ 1)
         , "Further / Higher Education completion date" = case_when(`Further / Higher Education completion date` == "NULL" ~ 0, TRUE ~ 1)
         , "Date FE/HE qualification achieved" = case_when(`Date FE/HE qualification achieved` == "NULL" ~ 0, TRUE ~ 1)
         , "School start date" = case_when(`School start date` == "NULL" ~ 0, TRUE ~ 1))


# select specific columns (that can be regarded as positive outcomes, mostly features with start date)
df4 <- df3 %>% 
  dplyr::select(
    1, 3, 5, 6, 7, 40, 42, 44, 48, 56)

# using pivot longer, transpose df4 such that we have lesser columns
df5 <- df4 %>% 
  pivot_longer(cols = -1, names_to = "outcomes") # this means pivot/transpose all columns except the first feature
  
# create a new column, add group the outcomes into three different groups of Education/training, employment and others  
df6 <- df5 %>% 
  mutate(Outcome_type = case_when(outcomes == "Subsidy start date" | outcomes == "Subsidy end date" | outcomes == "Work experience start date" | outcomes == "Work experience completion date" | outcomes == "Volunteering start date" | outcomes == "Volunteering completion date" ~ "Others"
                                  , outcomes == "Employment start date"  |  outcomes == "Self-employment start date" | outcomes == "Modern Apprenticeship start date" ~ "Employment"
                                  , TRUE ~ "Education & Training")) # group the outcomes to either employment, education/training or others
  

# sum each outcomes
df7 <- df6 %>% 
  group_by(outcomes, Outcome_type) %>% 
  summarise(total_outcome = sum(value))
  
# create a facet plot 
df7 %>% 
  ggplot(aes(reorder(outcomes,total_outcome), total_outcome)) +
  geom_col(fill = "grey70") +
  geom_text(data = df7 %>% filter(total_outcome < 650), aes(label = total_outcome), hjust = -0.1) +
  geom_text(data = df7 %>% filter(total_outcome > 650), aes(label = scales::comma(total_outcome), hjust = 1.2)) +
  coord_flip() +
  theme(strip.text.y = element_text(angle = 0, face = "bold")
        , panel.grid.minor.x = element_blank()
        , panel.grid.major.y = element_blank()
        , panel.grid.major = element_line(linetype = 2, color = "black"
                                          , linewidth = 0.08)
        , axis.text = element_text(size = 10)
        , plot.caption = element_text(size=12, hjust=1.2, color="black", family = "mono")) +
  facet_grid(Outcome_type~ ., scales = "free_y") +
  labs(x="", y = "number of clients", caption = "Source: Outcomes dataset") +
  scale_y_continuous(expand = c(0,10))


