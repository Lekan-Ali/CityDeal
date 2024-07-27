# load libraries
library(gplots)
library(tidyverse)
library(readxl)

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


# perform logistic regression to check for significant interventions
activity_fit <- glm(Employed ~ nvhActivityGroupName, data = df4, family = "binomial")

# model summary
summary(activity_fit)



# perform Anova (test for group differences in activity groups)
anova_fit <- aov(Employed ~ nvhActivityGroupName, data = df4) 

# check anova summary
summary(anova_fit)


# plot anova fit
plotmeans(Employed ~ nvhActivityGroupName, data = df4)


# calculate group means
activity_group_means <- aggregate(df4$Employed, by = list(df4$nvhActivityGroupName)
          , FUN = mean)

# rename variable
names(activity_group_means)[1:2] <- c("Activity_group", "group_means")


## plot group means ##
# create a function to plot group means
make_plot <- function(city_deal_df) {
  # create aesthetic
  p1 <- ggplot(city_deal_df
               , aes(reorder(Activity_group,group_means)
                     , group_means))
  
  # add plot type
  p2 <- p1 + geom_col(fill = "grey65") +
    coord_flip() # flip chart (switch x and y axis)
  
  
  # highlight activity over 0.4 group mean (most important activities)
  p3 <- p2 + geom_col(data = activity_group_means %>% 
                        filter(group_means > 0.4), fill = "#FF9999") 
  
  # theme
  p4 <- p3 + theme(panel.grid = element_blank()
                   , panel.grid.major.x = element_line(linetype = 2, linewidth = 0.09)
                   , axis.text = element_text(size = 11)) +
    labs(x= "Activity group", y = "group means")
  
  
  # add text or figures to plot (start with activities with more than 0.4 means)
  p5 <- p4 + geom_text(data = activity_group_means %>% filter(group_means > 0.4)
                       , aes(label = round(group_means,2), hjust = 1.1))
  
  # add text or figures to plot (start with activities with less than 0.4 means)
  p6 <- p5 + geom_text(data = activity_group_means %>% filter(group_means < 0.4)
                       , aes(label = round(group_means,2), hjust = -0.1))
  
  
  # expand scale (to reduce the gap between the y axis and the bars)
  p7 <- p6 + scale_y_continuous(expand = c(0,0.009))
  
  
  p7
}


make_plot(activity_group_means)
