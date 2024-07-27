# load libraries
library(gplots)
library(tidyverse)
library(readxl)
library(gsubfn) # to remove the fullstop in the activity names from feature importance metrics

# read in dataset
df <- read_excel("Activity_group_name_age_employed_all.xlsx")

# renaming entry
df$nvhActivityGroupName[df$nvhActivityGroupName == "Finanical/Debt"] <- "Financial/Debt"


# remove client(s) that don't exist in both Activity and outcome sheet (just 1 client),na.omit removes entry with missing values

df2 <- df %>% na.omit()


# remove activity group that are dependent on the output (Employed)
df3 <- df2 %>% 
  filter(nvhActivityGroupName != "Outcomes"
         , nvhActivityGroupName != "Sustainment")



# remove activity names that are dependent on the output (Employed)
df4 <- df3 %>% 
  filter(ActivityName != "Vacancy application - Successful"
         , ActivityName != "Vacancy application - Unsuccessful"
         , ActivityName != "Move to Stage 5")



# make df wider such that activity names will be features
df5 <- df4 %>% select(ClientRef, ActivityName, Employed) %>% 
  group_by(ClientRef,Employed,ActivityName) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = ActivityName, values_from = count, values_fill = 0) %>% 
  as_tibble()

# convert the output or dependent varible to a factor and assign to a new variable
Employed_at_any_time <- factor(ifelse(df5$Employed < 1, "No","Yes"))

#create new dataframe
df5 %>% 
  data.frame(Employed_at_any_time) %>% 
  select(-Employed, -ClientRef) %>% 
  na.omit() -> df6


# perform bagging with 1000 trees
set.seed(1)
activity_name_bagging <- randomForest(Employed_at_any_time ~ ., 
                                 data = df6, importance = TRUE, ntree = 500, mtry = 75)

varImpPlot(activity_name_bagging)

# MeanDecrese for bagging
MeanDecrease_activity_name_bagging = importance(activity_name_bagging, type = 1)


#perform random forest with 1000 trees
set.seed(1)
activity_name_rf <- randomForest(Employed_at_any_time ~ ., 
                            data = df6, importance = TRUE, ntree = 500)

varImpPlot(activity_name_rf)

# MeanDecrese for bagging
MeanDecrease_activity_name_rf = importance(activity_name_rf, type = 1)



## create a new dataframe for both bagging and random forest feature importance ##

# for bagging 
activity_name_bagging_df <- as.data.frame(MeanDecrease_activity_name_bagging) %>% 
  mutate(Activity_name = gsub("\\."," ",rownames(MeanDecrease_activity_name_bagging))) # using the gsub to remove the fullstop in the activity names


# for random forest
activity_name_rf_df <- as.data.frame(MeanDecrease_activity_name_rf) %>% 
  mutate(Activity_name = gsub("\\."," ",rownames(MeanDecrease_activity_name_rf))) # using the gsub to remove the fullstop in the activity names


# remove rowheaders in both dataframes
rownames(activity_name_bagging_df) <- NULL
rownames(activity_name_rf_df) <- NULL


# before we merge, we need to ensure the activity names will match with the second table, apparently some activity names have been altered as a result of the gsub used above. Below, I will be adjusting an important intervention. Ignoring other activity names because they will not be needed in the facet plot

activity_name_bagging_df[activity_name_bagging_df$Activity_name == "In work Support Provided","Activity_name"] <- "In-work Support Provided" # for bagging forest table


activity_name_rf_df[activity_name_rf_df$Activity_name == "In work Support Provided","Activity_name"] <- "In-work Support Provided" # for random forest table


## create a facet plot of only important interventions (or activity groups) and the activities under these groups ##

# To do this we need to merge a df of activity groups and activities name to the bagging and randfom forest feature importance data frame


############ Merging and plot for Bagging ##############
# merging the bagging dataframe and the second df with the activity names and activity group
df6_activity_name_bagging <- merge(df5, activity_name_bagging_df,by.x = "ActivityName", by.y = "Activity_name") # df5 has been created in the Activity Name sheet


# filtering for important interventions
df7_activity_name_bagging <- df6_activity_name_bagging %>% filter(nvhActivityGroupName == "Support"
                      | nvhActivityGroupName == "Job Related"
                      | nvhActivityGroupName == "Job search and job matching")

# making the facet plot for bagging
ggplot(df7_activity_name_bagging, aes(reorder(ActivityName,MeanDecreaseAccuracy), MeanDecreaseAccuracy)) +
  geom_col() +
  coord_flip() +
  facet_grid(nvhActivityGroupName~., scales = "free") +
  theme(strip.text.y = element_text(angle = 0)) +
  labs(x="")


############ Merging and plot for Random Forest ##############

# merging the Random forest dataframe and the second df with the activity names and activity group
df6_activity_name_random_forest <- merge(df5, activity_name_rf_df,by.x = "ActivityName", by.y = "Activity_name") # df5 has been created in the Activity Name sheet


# filtering for important interventions
df7_activity_name_random_forest <- df6_activity_name_random_forest %>% filter(nvhActivityGroupName == "Support"
                                                                  | nvhActivityGroupName == "Job Related"
                                                                  | nvhActivityGroupName == "Job search and job matching")

# making the facet plot for RandomForest model

# create plot canvas
t1 <- ggplot(df7_activity_name_random_forest
             , aes(reorder(ActivityName,MeanDecreaseAccuracy)
                   , MeanDecreaseAccuracy))

# add plot type to canvas
t2 <- t1 + geom_col(fill = "grey65") 

# flip the plot so that x axis will rotate (become y axis, and vice-versa)
t3 <- t2 + coord_flip() 

# add the facet grid
t4 <- t3 + facet_grid(nvhActivityGroupName~., scales = "free") 


# add the figures of each activity
t5 <- t4 + 
  geom_text(aes(label = round(MeanDecreaseAccuracy,1)), hjust = -0.1)

# adjust the theme of the plot
t6 <- t5 + theme(panel.grid = element_blank()
                 , panel.grid.major.x = element_line(linetype = 2, linewidth = 0.05)
                 , axis.text = element_text(size = 13)
                 , strip.text = element_text(size = 10, face = "bold")
                 , strip.text.y = element_text(angle = 0)) 

# discard the x-axis label
t7 <- t6 + labs(x=""
                , y = "Feature Importance")

t7
