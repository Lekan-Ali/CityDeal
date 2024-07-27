# load libraries
library(gplots)
library(tidyverse)
library(readxl)
library(xlsx)

#install.packages("effects")
#install.packages("HH")
library(HH)
library(effects)
library(rlang) # to create a function for ggplot 


# read in outcome dataset
df_outcomes <- read_excel("Outcomes.xlsx")

# read in demographic dataset
df_demographic <- read_excel("Demographic.xlsx")

# read in activities dataset
df_activities <- read_excel("Activities.xlsx")


# select specific outcomes columns
df2 <- df_outcomes %>% 
  dplyr::select(1, 3, 4, 5, 6, 7, 40, 41, 42, 43, 44, 45
                , 48, 49, 53, 56) 


# select only unique clients and their outcomes (remove duplicates)
df_outcomes_unique <- df_outcomes %>% 
  group_by(`Unique ID`) %>% 
  mutate(instances = row_number()) %>% 
  filter(instances == 1)

# select specific columns or columns that can be regarded as positive outcomes features
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



# select only Age and sex in the demographic data
df_age <- df_demographic %>% dplyr::select(3,12,13)


# select only unique clients (remove duplicates)
df_age_sex_unique <- df_age %>% 
  group_by(`Unique ID`) %>% 
  mutate(instances = row_number()) %>% 
  filter(instances == 1) %>% 
  dplyr::select(`Unique ID`, Age, Sex)



# 3.2 create a new column that categorizes clients based on their age
df_age_sex_unique <- df_age_sex_unique %>% 
  mutate(Age_cohort = case_when(Age > 26 ~ "Adult"
                                , TRUE ~ "Young")) %>%  # create a new column Age_cohort that assigned clients older than 26 as Adult and young (less or equal to 26 as Young)
  group_by(`Unique ID`) %>% 
  mutate(instances = row_number()) %>% 
  filter(instances == 1) %>% # filter for the first occurences only
  dplyr::select(-5) # remove the instances column from the dataframe



# do a plot showing the count for each gender and another plot showing the age cohort

# -- 
df_age_sex_unique_2 <- df_age_sex_unique %>% 
  mutate(Age_cohort = case_when(Age > 26 ~ "Adult"
                         , TRUE ~ "Young")) %>% 
  group_by(Age_cohort, Sex) %>% 
  count()

# --
k1 <- ggplot(df_age_sex_unique_2, aes(reorder(Sex,n), n))


k2 <- k1 + geom_col() + 
  coord_flip()

# View plot (single plot)
k2

k3 <- k2 + facet_wrap(.~Age_cohort)

# View plot (facet plot)  
k3

# merge the demographic data with the outcomes data (outcomes data should be on the left)

df5 <- df4 %>% 
  left_join(df_age_sex_unique, by = "Unique ID"
            , relationship = "many-to-many")



# create age cohort in the combined dataset
df6 <- df5 %>% 
  mutate(Age_cohort = case_when(Age <= 26 ~ "Young"
                                , Age > 26 ~ "Adult"
                                , TRUE ~ "Age (Unknown)"))



# since the combined data is a many to many relationship, we need to create a row number to pick unique occurrences only
df7 <- df6 %>% 
  group_by(`Unique ID`) %>% 
  mutate(flag = row_number()) %>% 
  filter(flag == 1) %>%  # this picks only the first occurrence for each unique ID
  dplyr::select(-flag, -Age, -Sex) # remove these columns


# pivot longer (for facet plot), to see outcome distribution by age cohort
df8 <- df7 %>% 
  pivot_longer(cols = c(-1, -11), names_to = "outcomes") %>% # pivot longer all columns except column 1 (unique ID) and 17 (Age cohort), both categorical variables
  mutate(Outcome_type = case_when(outcomes == "Subsidy start date" | outcomes == "Subsidy end date" | outcomes == "Work experience start date" | outcomes == "Work experience completion date" | outcomes == "Volunteering start date" | outcomes == "Volunteering completion date" ~ "Others"
                                  , outcomes == "Employment start date"  |  outcomes == "Self-employment start date" | outcomes == "Modern Apprenticeship start date" ~ "Employment"
                                  , TRUE ~ "Education & Training")) %>% # group the outcomes to either employment, education/training or others
  filter(Age_cohort != "Age (Unknown)") %>% # filter out client with unknown age
  group_by(Age_cohort,Outcome_type, outcomes) %>% 
  summarise(total_count = sum(value)) %>% 
  mutate(share = case_when(Age_cohort == "Adult" ~ round(total_count/2018,2) # this calculates the percentage share of clients for each outcome
                           , TRUE ~ round(total_count/2186,2)))




# create a new column that combines the actual value and the percentage
df9 <- df8 %>% 
  mutate(value_share = paste0(share*100,"% (",total_count,")"))
  

# change the Age variable from character to a factor variable
df9$Age_cohort <- factor(df9$Age_cohort, levels = c("Young","Adult"))



# create plot
df9 %>%  ggplot(aes(reorder(outcomes,share), share)) +
  geom_col(fill = "grey70") +
  geom_col(data = df9 %>% 
             filter(total_count > 300) , aes(fill = "#FF9999")) +
  geom_text(data = df9 %>% filter(share < 0.15), 
            aes(label = value_share),hjust = -0.1) +
  geom_text(data = df9 %>% filter(share > 0.13), 
            aes(label = value_share),hjust = 1.1) +
  coord_flip() +
  facet_grid(Outcome_type ~ Age_cohort, scales = "free_y") +
  theme(strip.text.y = element_text(angle = 0, size = 11)
        , strip.text.x = element_text(size = 11)
        , panel.grid.major.y = element_blank()
        , panel.grid.minor.x = element_blank()
        , panel.grid.major.x = element_line(linetype = 2, linewidth = 0.2, color = "grey")
        , legend.position = "none"
        , legend.title = element_blank()
        , axis.text = element_text(size = 11)) +
  labs(y="", x = "Outcomes") +
  scale_y_continuous(labels = scales::percent)
