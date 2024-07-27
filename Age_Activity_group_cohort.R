# load libraries
library(gplots)
library(tidyverse)
library(readxl)

#install.packages("effects")
#install.packages("HH")
library(HH)
library(effects)
library(rlang) # to create a function for ggplot 

# 1.1 read in outcome dataset
df_outcomes <- read_excel("Outcomes.xlsx")

# 1.2 read in demographic dataset
df_demographic <- read_excel("Demographic.xlsx")

# 1.3 read in activities dataset
df_activities <- read_excel("Activities.xlsx")



# facet theme_plot
theme_facet <- theme(panel.grid = element_blank()
      , panel.grid.major.x = element_line(linetype = 2, linewidth = 0.09)
      , axis.text = element_text(size = 11)
      , strip.text = element_text(size = 10, face = "bold"))




# 2.1 select specific activities (ClientRef, Activity group name, ClientActivityID) columns
df2_activities <- df_activities %>% 
  dplyr::select(1, 3, 8) 


# renaming entry
df2_activities$nvhActivityGroupName[df2_activities$nvhActivityGroupName == "Finanical/Debt"] <- "Financial/Debt"


# Convert nvhActivityGroupName from character to factors data type
df2_activities$nvhActivityGroupName <- factor(df2_activities$nvhActivityGroupName
                                              , levels = c("Registration", "Contact"
                                                           , "Stage", "Client interaction", "Financial/Debt",
                                                           "IAG","Events","Employability development"
                                                           ,"Support", "Referrals"
                                                           , "Training","Specialist Advice","Job Related"
                                                           ,"Job search and job matching"
                                                           ,"Work Experience","Outcomes"
                                                           ,"Sustainment", "Exit"))


# 2.2 select unique activities using ActivityID as the indicator
df_unique_activities <- df2_activities %>% 
  group_by(ClientActivityID) %>% 
  mutate(flag = row_number()) %>% 
  filter(flag == 1) %>% 
  mutate(`Unique ID` = ClientRef) %>% # need to rename the ClientRef to Unique ID (this is for merging reasons. the demographic data has the Unique ID variable as clientID whereas the Activity data has ClientRef as Client ID, we might as well standardize it so both tables have the same column name)
  dplyr::select(-1,-4) # remove ClientRef (the entries are now saved under Unique ID) and the flag column (not needed)
  
  
# 3.1 select only Unique ID, Age and sex in the demographic data
df_demo_age_sex_id <- df_demographic %>% dplyr::select(3,12,13)


# 3.2 select only unique clients (remove duplicates, however some clients are tagged as male and female, hence use row number to pick only 1 instance for clients that have more than one sex entry)
df_age_sex_unique <- df_demo_age_sex_id %>% 
  mutate(Age_cohort = case_when(Age > 26 ~ "Adult"
                                , TRUE ~ "Young")) %>%  # create a new column Age_cohort that assigned clients older than 26 as Adult and young (less or equal to 26 as Young)
  group_by(`Unique ID`) %>% 
  mutate(instances = row_number()) %>% 
  filter(instances == 1) %>% # filter for the first occurences only
  dplyr::select(-5) # remove the instances column from the dataframe


# 4. count the total number of Clients based on their age group
df_age_cohort_count <- df_age_sex_unique %>% 
  group_by(Age_cohort) %>% 
  summarise(total_clients = n()) %>% 
  mutate(Age_cohort = factor(Age_cohort, levels = c("Young", "Adult"))) # convert the Age cohort to a factor variable


# plot the(might delete these lines of code - not needed) - DELETE LATER
df_age_cohort_count_plot <- df_age_cohort_count %>% 
  ggplot(aes(Age_cohort, count)) +
  geom_col(fill = "grey70") +
  geom_text(aes(label = scales::comma(count), vjust = -0.3)) +
  theme(panel.grid.major.x  = element_blank()
        , panel.grid.minor.y = element_blank()
        , panel.grid.major.y = element_line(linetype = 2, colour = "grey70")
        , axis.text = element_text(size = 10)) +
  #scale_y_continuous(expand = c(0,80)) +
  labs(y = "number of clients"
       , x = "")


# 5.1 merge the demographic data with the activities data (outcomes data should be on the left)
df_demographic_activity <- df_age_sex_unique %>% 
  left_join(df_unique_activities, by = "Unique ID"
            , relationship = "many-to-many")


  
# 5.2 since the merged dataset is a many-to-many relationship, we have duplicates. We need to remove duplicates to pin-point the number of activities done by each clients
df_demo_activity_2 <- df_demographic_activity %>% 
  group_by(ClientActivityID) %>% 
  mutate(instances = row_number()) %>% 
  filter(instances == 1) # select the first instance
 

# 6. total number of activities by Age cohort (this will be combined with the variable in - 4 - to create a plot)
df_total_activities_age_cohort <- df_demo_activity_2 %>% 
  group_by(Age_cohort) %>% 
  summarise(total_activities = n())



# 6.2 merge Code 6 and Code 4 + create a new column that calculates the average activities
df_total_activities_total_clients <- df_total_activities_age_cohort %>% 
  left_join(df_age_cohort_count, by = "Age_cohort") %>% 
  mutate(average_activities = round(total_activities/total_clients,0)) # create a new column that calculates the average by age cohort


# 6.3 
df_total_activities_total_clients_2 <- df_total_activities_total_clients %>% 
  pivot_longer(cols = c(-1)) # pivot longer the plot for facet plot


# 6.4 recode the name column from character to a factor
df_total_activities_total_clients_2$name <-  factor(df_total_activities_total_clients_2$name, 
                                                    levels = c("total_clients"
                                                            , "total_activities"
                                                            , "average_activities"))

# 6.5 create plot canvas
w1 <- df_total_activities_total_clients_2 %>%  ggplot(aes(Age_cohort, value))

# 6.6 Add plot type , should be a bar chart or column chart
w2 <- w1 + geom_col(fill = "grey80") + coord_flip()

# 6.7 Add facet, text and theme
w3 <- w2 + facet_wrap(.~name, scales = "free_x") +
  geom_text(aes(label = scales::comma(value)), hjust = 1.1) +
  theme_facet +
  labs(x = "", y = "")


# 6.8 View plot
w3




# we are interested in the number of unique activities performed by each client. Such that if a client performs the same activity 4 times, we say that client performed that activity at least once. Here we use the row_number again to count only the first instance
df_performed_at_least_once <- df_demo_activity_2 %>% 
  group_by(`Unique ID`, nvhActivityGroupName) %>% 
  mutate(flag = row_number()) %>% 
  filter(flag == 1) %>% 
  group_by(Age_cohort, nvhActivityGroupName) %>% 
  summarise(performed_act_at_least_once = n()) %>% 
  mutate(share = case_when(Age_cohort == "Adult" ~ round(performed_act_at_least_once/2018,2)
                           , TRUE ~ round(performed_act_at_least_once/2186,2)))



# create a new column that combines the actual value and the percentage
df_performed_at_least_once <- df_performed_at_least_once %>% 
  mutate(value_share_1 = paste0(share*100,"% (",performed_act_at_least_once,")")
         , value_share_2 = paste0(share*100,"% (",performed_act_at_least_once,")"))




# plot 

p1 <- df_performed_at_least_once %>% 
  ggplot(aes(fct_rev(nvhActivityGroupName), share, fill = Age_cohort))

# add plot type and rotate bars
p2 <- p1 + geom_col(position = "dodge") + 
  coord_flip()


# add text to plot (text for Adult only + less than 0.9 - text will be beside the bars)
p3 <- p2 + geom_text(data = df_performed_at_least_once %>% 
                     filter(Age_cohort == "Adult" , share < 0.9), aes(label = value_share_2)
               , hjust = -0.08, vjust = 1.5, size = 3.5)


# add text to plot (text for Adult only + greater than 0.9 - text will be inside the bars)
p4 <- p3 + geom_text(data = df_performed_at_least_once %>% 
                       filter(Age_cohort == "Adult" , share > 0.9), aes(label = value_share_2)
                     , hjust = 1.05, vjust = 1.5, size = 3.5)


# add text to plot (text for Young only + less than 0.9 - text will be beside the bars)
p5 <- p4 + geom_text(data = df_performed_at_least_once %>% 
                       filter(Age_cohort == "Young", share < 0.9), aes(label = value_share_2)
                     , hjust = -0.08, vjust = -0.5, size = 3.5)



# add text to plot (text for Young only + less than 0.9 - text will be beside the bars)
p6 <- p5 + geom_text(data = df_performed_at_least_once %>% 
                       filter(Age_cohort == "Young", share > 0.9), aes(label = value_share_2)
                     , hjust = 1.05, vjust = -0.5, size = 3.5)



# change scale to a percentage scale and adjust the plot background
p7 <- p6 + scale_y_continuous(labels = scales::percent
                              , expand = c(0,0.01)) + 
  theme(panel.grid.major.y = element_blank()
                 , panel.grid.minor.x = element_blank()
                 , panel.grid.major.x = element_line(linetype = 2, linewidth = 0.2, color = "grey")
                 , legend.position = "right"
        , legend.title = element_blank()
        , axis.text = element_text(size = 11))


# switch the legend, let Young come before Adult
p8 <- p7 + guides(fill = guide_legend(reverse = TRUE)) +
  labs(x= "",  y= "performed activity at least once")


# display plot
p8
