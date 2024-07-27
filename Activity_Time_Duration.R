# load libraries
library(gplots)
library(tidyverse)
library(readxl)

#install.packages("effects")
#install.packages("HH")
library(HH)
library(effects)
library(rlang) # to create a function for ggplot 

# read in dataset (for data cleaning purposes, I created a new file on excel, this file is a combination of the Outcomes and Activities datasets. The dataset had to be cleaned on excel, in a way, it is better done on excel than using R or python)
df <- read_excel("Unique_Data_Activities_Employed_1.xlsx")


# View Top 5 rows
head(df, 5)

# renaming entry
df$nvhActivityGroupName[df$nvhActivityGroupName == "Finanical/Debt"] <- "Financial/Debt"


# remove activity group that are dependent on the output (Employed)
df2 <- df %>% 
  filter(nvhActivityGroupName != "Outcomes"
         , nvhActivityGroupName != "Sustainment")



# remove activity names that are dependent on the output (Employed)
df3 <- df2 %>% 
  filter(ActivityName != "Vacancy application - Successful"
         , ActivityName != "Vacancy application - Unsuccessful"
         , ActivityName != "Move to Stage 5")


# rename
df4 <- df3 %>% 
  mutate(Employment_status = case_when(
    Employed > 0 ~ "Employed"
    , TRUE ~ "Unemployed"
  ))

# (Objective A) get the total beneficiary and client beneficiary hours for each group and visualize - (1)number of total activities performed, (2)average number of beneficiary hours and (3) avg. of client beneficiary hours)

# (Objective B) get the average total hours by contract type.




#### Objective A ######


# group by Activity group
df_grouped_by_activitygroup <- df4 %>%  
  group_by(nvhActivityGroupName) %>% 
  summarise(count_of_activities = n() # this will count the total number of times each activity was done 
            , average_beneficiary_hours = mean(BeneficiaryHours)
            , average_client_hours = mean(ClientBeneficiaryHours)
            , total_beneficiary_hours = sum(BeneficiaryHours)
            , total_client_hours = sum(ClientBeneficiaryHours))


# pivot longer for facet plot purposes
df_grouped_by_activitygroup_2 <- pivot_longer(df_grouped_by_activitygroup
             , cols = c("count_of_activities", "average_beneficiary_hours", "average_client_hours")
             , names_to = "metrics") %>% 
  dplyr::select(nvhActivityGroupName, metrics, value) # select only the needed columns


# make the metrics column a factor instead of character
df_grouped_by_activitygroup_2$metrics <- factor(df_grouped_by_activitygroup_2$metrics
                                                , levels = c("count_of_activities"
                                                             , "average_beneficiary_hours"
                                                             , "average_client_hours"))


## create facet plot ##

# create a function to visualize facet plot

make_facet_plot <- function(df, col1, col2, col3) {
  # create aesthetic
  ggplot(data = df
               , aes(x = reorder( {{ col1 }}, 
                                  {{ col2 }} )
                     , y = {{ col2 }})) +
    geom_col(fill = "grey70") + # add plot type 
    theme(panel.grid = element_blank()
          , panel.grid.major.x = element_line(linetype = 2, linewidth = 0.09)
          , axis.text = element_text(size = 11)
          , strip.text = element_text(size = 10, face = "bold")) +
    labs(x = "") +
    coord_flip() + # flip plot
    facet_wrap(enquo(col3), scales = 'free_x') # add facet wrap
  
}


# visualize the plot
make_facet_plot(df_grouped_by_activitygroup_2, nvhActivityGroupName, value, metrics) +
   # ---- highlight important metrics ----- #
  
    # highlighting the only Contact bar in the contact plot
  geom_col(data = df_grouped_by_activitygroup_2 %>% 
             filter(nvhActivityGroupName == "Contact", value > 3000 | metrics == "count_of_activities")
           , aes(fill = "#FF9999")) +
  
  # highlighting bar greater than 1.9 value in the average_beneficiary_hours plots
  geom_col(data = df_grouped_by_activitygroup_2 %>% 
             filter(metrics == "average_beneficiary_hours", value > 1.9)
           , aes(fill = "#FF9999")) +
  
  # highlighting bar greater than 1.9 value in the average_beneficiary_hours plots
  geom_col(data = df_grouped_by_activitygroup_2 %>% 
             filter(metrics == "average_client_hours", value > 1.5)
           , aes(fill = "#FF9999")) +
  
  # ---- add text or values in the count_of_activities_plot ---- #
  
      # adding text to the plot for only the contact bar in the count_of_activities_plot
  geom_text(data =df_grouped_by_activitygroup_2 %>% 
              filter(nvhActivityGroupName == "Contact" , metrics == "count_of_activities")
            , aes(label = scales::comma(value), hjust = 1.1)) +
      # adding text to the plot for other activity groups in the count_of_activities_plot
  geom_text(data =df_grouped_by_activitygroup_2 %>% 
              filter(nvhActivityGroupName != "Contact" , metrics == "count_of_activities")
            , aes(label = scales::comma(value), hjust = -0.1)) +
  
  # ---- add text or values in the average beneficiaries hours plot ---- #
  
    # adding text to the plot for high value activities (values inside the bar)
  geom_text(data =df_grouped_by_activitygroup_2 %>% 
              filter(nvhActivityGroupName == "Training" | nvhActivityGroupName == "Job Related" | nvhActivityGroupName == "Job search and job matching"
                     , metrics == "average_beneficiary_hours")
            , aes(label = scales::comma(value), hjust = 1.1)) +
    # adding text to the plot for low value activity group (values outside the bar)
  geom_text(data =df_grouped_by_activitygroup_2 %>% 
              filter(nvhActivityGroupName != "Training" , nvhActivityGroupName != "Job Related" , nvhActivityGroupName != "Job search and job matching"
                     , metrics == "average_beneficiary_hours")
            , aes(label = round(value,2), hjust = -0.1)) +
  
  # ---- add text or values in the average client hours ---- #
  geom_text(data =df_grouped_by_activitygroup_2 %>% 
              filter(nvhActivityGroupName == "Job search and job matching"
                     , metrics == "average_client_hours")
            , aes(label = round(value,2), hjust = 1.1)) +
  
  geom_text(data =df_grouped_by_activitygroup_2 %>% 
              filter(nvhActivityGroupName != "Job search and job matching"
                     , metrics == "average_client_hours")
            , aes(label = round(value,2), hjust = -0.1)) +
  theme(legend.position = "none"
        , plot.caption = element_text(size=12, hjust=1, color="black", family = "mono")) +
  labs(y = "", caption = "Source: Activities dataset") +
  scale_y_continuous(labels = scales::comma)





#### Objective B ######

# (Objective B) get the average total hours by contract type.

# group by contract type
df_group_by_contract_type <- df %>% group_by(`Contract type`) %>% 
  summarise(average_beneficiary_hours = mean(BeneficiaryHours)
            , number_of_clients = n_distinct(ClientRef))



# pivot longer for facet plot purposes
df_group_by_contract_type_2 <- pivot_longer(df_group_by_contract_type
                                              , cols = c("average_beneficiary_hours","number_of_clients"), names_to = "metrics")


# make the metrics column a factor instead of character
df_group_by_contract_type_2$metrics <- factor(df_group_by_contract_type_2$metrics
                                                , levels = c("number_of_clients"
                                                             , "average_beneficiary_hours"))


# exclude unemployment since it is not a contract type
df_group_by_contract_type_3 <- df_group_by_contract_type_2 %>% 
  filter(`Contract type` != "Unemployed")

# visualize the data frame (facet plot)
  
make_facet_plot(df_group_by_contract_type_3,`Contract type`
                , value, metrics) +
  
# highlighting average hours for permanent contract type
geom_col(data = df_group_by_contract_type_3 %>% 
           filter(`Contract type` == "Permanent", value > 1 , metrics == "average_beneficiary_hours")
         , aes(fill = "#FF9999")) +
  
  # ---- add text or values in the number_of_clients plot ---- #
  
  # adding text to the plot for only the contact type bar in the count_of_activities_plot
  geom_text(data =df_group_by_contract_type_3 %>% 
              filter(`Contract type` == "Permanent" , metrics == "number_of_clients")
            , aes(label = scales::comma(value), hjust = 1.1)) +

  # adding text to the plot for other contract type in the number_of_clients plot
  geom_text(data =df_group_by_contract_type_3 %>% 
              filter(`Contract type` != "Permanent" , metrics == "number_of_clients")
            , aes(label = scales::comma(value), hjust = -0.1)) +
  
  # ---- add text or values in the average beneficiaries hours plot ---- #
  
  # adding text to the plot for high value activities (values inside the bar)
  geom_text(data =df_group_by_contract_type_3 %>% 
              filter(`Contract type` == "Permanent"
                     , metrics == "average_beneficiary_hours")
            , aes(label = round(value,2), hjust = 1.1)) +
  
  # adding text to the plot for low value activity group (values outside the bar)
  geom_text(data =df_group_by_contract_type_3 %>% 
              filter(`Contract type` != "Permanent"
                     , metrics == "average_beneficiary_hours")
            , aes(label = round(value,2), hjust = -0.1)) +
  theme(legend.position = "none") +
  labs(x = "Contract type", y = "")
