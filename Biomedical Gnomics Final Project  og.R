
getwd()

#setwd("..")
#setwd("Desktop/Biomedical Genomics/")
library(tidyverse)
library(reshape2)
library(rstatix)
library(ggpubr)


# reads the csv file 
group6 = read_csv("group6_dataset.csv", show_col_types = FALSE)

# gives a brief summary of the structure of the dataset
glimpse(group6)

# Explore the structure of the dataset.
str(group6)

#list and tallies of the types antigens 
group6 %>% group_by(antigen) %>% tally 

#list and tallies the  types of antibody
group6 %>% group_by(class) %>% tally 

#list and tallies the  antibodies generated against the ospC antigens 
group6 %>% group_by(antibody) %>% tally

#list and tallies the class antibodies
group6 %>% group_by(Ab_class) %>% tally 



#Biological Question: Do the type of antibody influence cell binding?

#Hypothesis: The cross reactivity between the antigens and synthetic type of antibody  will have a  greater binding because the synthetic antibodies are better attuned to binding with the antigens.

#Statistical Null Hypothesis (H0): No effect (equal µ, no significant between the average optical density of the  antibodies)



#Bases on the antibodies  generated against the ospC antigens which  types of antibody were involved  ?

#select the distinct rows from a data set and deletes repeated information
#selects the types of antibody and antibodies generated against  the ospC antigens

group6 %>% select(class,antibody) %>% distinct()


#Average(mean) optical density base on the antibody generated against the the ospC antigens, then saving  output in a varaiable

Mean_avg.og_antibody <- group6 %>% group_by(antibody)%>% summarise(anti_mean_avg = mean(avg.od))

#Returns the average optical density
Mean_avg.og_antibody 

#sorts base on mean
Mean_avg.og_antibody %>% arrange(desc(anti_mean_avg))


# The top two average optical density are synethic, followed by  anti-D  which is  native
# 4 out of the top  are 5 averages are synethic.




#boxplot
ggplot(group6, aes(x= antibody, y= avg.od )) + geom_boxplot(aes(fill=class), outlier.shape = NA, alpha= 0.3) +  geom_jitter(aes(fill=Ab_class),shape=21)  + theme_bw() + labs(title = " Antibodies generated against ospC antigens vs Average Optical Density", y="Average Optical density", x = "Types of Antibodies ") + theme(text = element_text(face="bold", size=10), axis.text = element_text(face="bold", size=10), axis.title = element_text(face="bold", size=12), legend.text= element_text(size=10), legend.title=element_blank(), legend.position="bottom")


# the boxplot helps to visualize the data including high and low extremities. This allows us to see where the data is spread out with specificity.


#synthetic has the highest mean

#violin plot
ggplot(group6,aes(x= class, y= avg.od)) + geom_violin() + geom_jitter(aes(fill = Ab_class),shape=22) +theme_bw()  + labs(title = "Type of antibody vs Average Optical density", y="Average Optical density", x = "Type of antibody") + theme(text = element_text(face="bold", size=12), axis.text = element_text(face="bold", size=12), axis.title = element_text(face="bold", size=12), legend.text=element_text(face="bold", size=12), legend.title=element_text(face="bold", size=12))
# The violin plot helps us to see where the data is more highly concentrated.


#test between the different types of antibodies  
t.test(data=group6, avg.od ~ class)
#p-value = 5.207e-15 therefore it is less than 0.05 therefore we reject the null hypothesis and there is a significant different  between the two antibodies(native and synthetic )

#Anova
model_Ab_class <- lm(data=group6, avg.od ~ Ab_class)
summary(model_Ab_class)

model_antibody <- lm(data=group6, avg.od ~ antibody)
summary(model_antibody)







 





