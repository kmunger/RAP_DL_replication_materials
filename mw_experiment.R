rm(list = ls())

library(dplyr)
library(ggplot2)
#set working directory
setwd("C:/Users/kevin/Desktop/rap_replication")

# # load data - (choose sample to analyze) 
load("data/q.Rdata") # 10759 X 112
# 
# # w/o mid attention check 
# load("data/q_noMid.Rdata") # 11783 X 111
# q <- q_noMid

# w/o mid attention check & only the 1st 9000
# load("data/q_9k.Rdata") # 9000 X 111
# q <- q_9k

# create indicators 
# assign the treatment condition repondents were in 
# across all 4 news types 
types = c("worldnews", "sports", "business", "entertainment")
mw_cols <- lapply(types, function(x) paste0('mw_', x, '_condition')) %>% unlist()
q[mw_cols]<-0 

# cols where info on stimuli shown is saved 
mw_choice <- c("Messing_experiment_World_News_DO", 
               "Messing_experiment_Sports_DO",        
               "Messing_Experiment_Bussiness_DO",
               "Messing_Experiment_Enetertainment_DO")

# question blocks for each news type in the survey
blocks = c(7, 8, 9, 10)

# create indicators 
for (i in 1:4){
  q[mw_cols[i]] <- ifelse(grepl(paste0(blocks[i], ".", "1"), q[mw_choice[i]][,], fixed = T), "source",
                          ifelse(grepl(paste0(blocks[i], ".", "2"), q[mw_choice[i]][,], fixed = T), "social", "both"))
}

################## Social Condition ######################
# do they pick the story with the most likes in the social condition?

# Stories with the most likes 
# World news - 1st story 
# Sports - 3rd story 
# Bussiness - 4th story 
# Entertainment - 4th story 

max_likes <- c("The first story", 
               "The third story", 
               "The forth story", 
               "The forth story")

# Create indicator
# if the most liked story was picked
# cross different news types
mw_cols_social <- lapply(types, function(x) paste0( 'mw.', x, '.social_picked_social')) %>% unlist()
q[mw_cols_social]<-0

for (i in 1:4){
  q[mw_cols_social[i]] <- ifelse(q[mw_cols[i]][,] != "social", NA,
                                 ifelse(q[mw_cols[i]][,] == "social" & 
                                          q[paste0('Q',blocks[i], ".", "4")] == max_likes[i], 1, 0))
}


################## Source Condition ######################
# did they pick politically salient stories in the source condition?

#### Libral outlet #####
# World news: 4th story 
# Sports: 1st story 
# Business: 4th story 
# Entertainment: 2nd story 

pol.lib <- c("The forth story", 
             "The first story",
             "The forth story",
             "The second story")


### Cons outlet ###
# World news:  1st story 
# Sports:  3rd story 
# Business:  3rd story 
# Entertainment: 3rd story 

pol.cons <- c("The first story", 
              "The third story",
              "The third story",
              "The third story")

# create indicator 
# If MSNBC, Fox News or Other was picked
mw_cols_political <- lapply(types, function(x) paste0( 'mw.', x, '.source_picked_political')) %>% unlist()
q[mw_cols_political]<-0

for (i in 1:4){
  q[mw_cols_political[i]] <- ifelse(q[mw_cols[i]][,] != "source", NA,
                                    ifelse(q[mw_cols[i]][,] == "source" & 
                                             q[paste0('Q',blocks[i], ".", "4")] == pol.lib[i], "MSNBC", 
                                           ifelse(q[mw_cols[i]][,] == "source" & 
                                                    q[paste0('Q',blocks[i], ".", "4")] == pol.cons[i], "Fox News", "Other"))) 
}


################### Both Condition ###################

both_max_likes <- c("The second story",
                    "The first story",
                    "The forth story",
                    "The second story")

both_pol.lib <- c("The third story",
                  "The third story",
                  "The forth story",
                  "The forth story")

both_pol.cons <- c("The forth story",
                   "The first story",
                   "The first story",
                   "The first story")



# did they pick the story with the maximum likes in the both condition?
mw_cols_both_social <- lapply(types, function(x) paste0( 'mw.', x, '.both_picked_social')) %>% unlist()
q[mw_cols_both_social]<-0

for (i in 1:4){
  q[mw_cols_both_social[i]] <- ifelse(q[mw_cols[i]][,] != "both", NA,
                                      ifelse(q[mw_cols[i]][,] == "both" & 
                                               q[paste0('Q',blocks[i], ".", "4")] == both_max_likes[i], 1, 0))
}

# did they pick politically salient stories in the bothe condition?
mw_cols_both_political <- lapply(types, function(x) paste0( 'mw.', x, '.both_picked_political')) %>% unlist()
q[mw_cols_both_political]<-0

for (i in 1:4){
  q[mw_cols_both_political[i]] <- ifelse(q[mw_cols[i]][,] != "both", NA,
                                         ifelse(q[mw_cols[i]][,] == "both" & 
                                                  q[paste0('Q',blocks[i], ".", "4")] == both_pol.lib[i], "MSNBC", 
                                                ifelse(q[mw_cols[i]][,] == "both" & 
                                                         q[paste0('Q',blocks[i], ".", "4")] == both_pol.cons[i], "Fox News", "Other"))) 
}



# replace empty cells with NAs 
x <- c("Q7.4", "Q8.4", "Q9.4", "Q10.4")
q$Q7.4[q$Q7.4 ==''] <- NA
q$Q8.4[q$Q8.4 ==''] <- NA
q$Q9.4[q$Q9.4 ==''] <- NA
q$Q10.4[q$Q10.4 ==''] <- NA


### headline effect
# world news 
tab.condition.story1 <- table(q$mw_worldnews_condition, q$Q7.4)
tab.condition.story1 <- round(tab.condition.story1/rowSums(tab.condition.story1), 2)

png(filename = "plots/headline_effect_worldnews.png")
tab.condition.story1[,c(1,3,4,2) ] %>%
  t()%>%
  barplot(beside = T, 
          legend.text = c("Story 1","Story 2","Story 3", "Story 4"),
           args.legend = list(x = "topright", 
               bty = "n"),
          xlab = "Treatment condition",
          ylab = "Choice rate")

dev.off()

# world news 
tab.condition.story2 <- table(q$mw_sports_condition, q$Q8.4)
tab.condition.story2<- round(tab.condition.story2/rowSums(tab.condition.story2), 2)

png(filename = "plots/headline_effect_sports.png")
tab.condition.story2[,c(1,3,4,2) ] %>%
  t()%>%
  barplot(beside = T, 
          legend.text = c("Story 1","Story 2","Story 3", "Story 4"),
          args.legend = list(x = "topright", 
                             bty = "n"),
          xlab = "Treatment condition",
          ylab = "Choice rate")

dev.off()


# world news 
tab.condition.story3 <- table(q$mw_business_condition, q$Q9.4)
tab.condition.story3 <- round(tab.condition.story3/rowSums(tab.condition.story3), 2)

png(filename = "plots/headline_effect_business.png")
tab.condition.story3[,c(1,3,4,2) ] %>%
  t()%>%
  barplot(beside = T, 
          legend.text = c("Story 1","Story 2","Story 3", "Story 4"),
          args.legend = list(x = "topright", 
                             bty = "n"),
          xlab = "Treatment condition",
          ylab = "Choice rate")

dev.off()



# entertainment news 
tab.condition.story4 <- table(q$mw_entertainment_condition, q$Q10.4)
tab.condition.story4 <- round(tab.condition.story4/rowSums(tab.condition.story4), 2)

png(filename = "plots/headline_effect_entertainment.png")
tab.condition.story4[,c(1,3,4,2) ] %>%
  t()%>%
  barplot(beside = T, 
          legend.text = c("Story 1","Story 2","Story 3", "Story 4"),
          args.legend = list(x = "topright", 
                             bty = "n"),
          xlab = "Treatment condition",
          ylab = "Choice rate")

dev.off()



##############BOTH political --- replicate Figure 2

table(as.factor(q$mw.sports.both_picked_political))


both_political_all<-table(q$mw.sports.both_picked_political) + table(q$mw.business.both_picked_political) +
  table(q$mw.entertainment.both_picked_political) + table(q$mw.worldnews.both_picked_political)


both_political_all<-both_political_all/ sum(both_political_all)

both_political_all[3]<-both_political_all[3]/2
####Divide by partisanship

# Political leaning --> 1 = Republican, -1 = Democrat, 0 = Independent

q$leaning
q_dem<-filter(q, leaning == -1 )


both_political_dem<-table(q_dem$mw.sports.both_picked_political) + table(q_dem$mw.business.both_picked_political) +
  table(q_dem$mw.entertainment.both_picked_political) + table(q_dem$mw.worldnews.both_picked_political)


both_political_dem<-both_political_dem/ sum(both_political_dem)

both_political_dem[3]<-both_political_dem[3]/2



q_rep<-filter(q, leaning == 1 )


both_political_rep<-table(q_rep$mw.sports.both_picked_political) + table(q_rep$mw.business.both_picked_political) +
  table(q_rep$mw.entertainment.both_picked_political) + table(q_rep$mw.worldnews.both_picked_political)


both_political_rep<-both_political_rep/ sum(both_political_rep)

both_political_rep[3]<-both_political_rep[3]/2



q_ind<-filter(q, leaning == 0 )


both_political_ind<-table(q_ind$mw.sports.both_picked_political) + table(q_ind$mw.business.both_picked_political) +
  table(q_ind$mw.entertainment.both_picked_political) + table(q_ind$mw.worldnews.both_picked_political)


both_political_ind<-both_political_ind/ sum(both_political_ind)

both_political_ind[3]<-both_political_ind[3]/2



nums<-c(both_political_dem, both_political_ind, both_political_rep)

leaning<-c("Democrat", "Democrat", "Democrat", "Independent", "Independent", "Independent",
           "Republican", "Republican", "Republican")

outlet<-c("Fox News", "MSNBC", "Other", "Fox News", "MSNBC", "Other", "Fox News", "MSNBC", "Other")


poldf<-data.frame("nums" = nums, "leanings" = leaning, "outlet" = outlet )

png(filename = "plots/select_outlet_both_info.png")


ggplot(  poldf, aes(x = outlet, y = nums, fill=leanings  ) ) +

 geom_bar(stat="identity", position=position_dodge()) + scale_fill_grey() +
  theme_bw() + ggtitle("Partisan and social endorsement condition") +

  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) 

  dev.off()

  
  
  
    ##############SOURCE ONLY political --- replicate Figure 2
  
  table(as.factor(q$mw.sports.source_picked_political))
  
  
  source_political_all<-table(q$mw.sports.source_picked_political) + table(q$mw.business.source_picked_political) +
    table(q$mw.entertainment.source_picked_political) + table(q$mw.worldnews.source_picked_political)
  
  
  source_political_all<-source_political_all/ sum(source_political_all)
  
  source_political_all[3]<-source_political_all[3]/2
  ####Divide by partisanship
  
  # Political leaning --> 1 = Republican, -1 = Democrat, 0 = Independent
  
  q$leaning
  q_dem<-filter(q, leaning == -1 )
  
  
  source_political_dem<-table(q_dem$mw.sports.source_picked_political) + table(q_dem$mw.business.source_picked_political) +
    table(q_dem$mw.entertainment.source_picked_political) + table(q_dem$mw.worldnews.source_picked_political)
  
  
  source_political_dem<-source_political_dem/ sum(source_political_dem)
  
  source_political_dem[3]<-source_political_dem[3]/2
  
  
  
  q_rep<-filter(q, leaning == 1 )
  
  
  source_political_rep<-table(q_rep$mw.sports.source_picked_political) + table(q_rep$mw.business.source_picked_political) +
    table(q_rep$mw.entertainment.source_picked_political) + table(q_rep$mw.worldnews.source_picked_political)
  
  
  source_political_rep<-source_political_rep/ sum(source_political_rep)
  
  source_political_rep[3]<-source_political_rep[3]/2
  
  
  
  q_ind<-filter(q, leaning == 0 )
  
  
  source_political_ind<-table(q_ind$mw.sports.source_picked_political) + table(q_ind$mw.business.source_picked_political) +
    table(q_ind$mw.entertainment.source_picked_political) + table(q_ind$mw.worldnews.source_picked_political)
  
  
  source_political_ind<-source_political_ind/ sum(source_political_ind)
  
  source_political_ind[3]<-source_political_ind[3]/2
  
  
  
  nums<-c(source_political_dem, source_political_ind, source_political_rep)
  
  leaning<-c("Democrat", "Democrat", "Democrat", "Independent", "Independent", "Independent",
             "Republican", "Republican", "Republican")
  
  outlet<-c("Fox News", "MSNBC", "Other", "Fox News", "MSNBC", "Other", "Fox News", "MSNBC", "Other")
  
  
  poldf<-data.frame("nums" = nums, "leanings" = leaning, "outlet" = outlet )
  
  png(filename = "plots/select_outlet_source_info.png")
  
  
  ggplot(  poldf, aes(x = outlet, y = nums, fill=leanings  ) ) +
    
    geom_bar(stat="identity", position=position_dodge()) + scale_fill_grey() +
    theme_bw() + ggtitle("Partisan only condition") +
    
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank()) 
  
  dev.off()
  
  
  
  
  ##############social ONLY social --- replicate Figure 3
  
  table(as.factor(q$mw.sports.social_picked_social))
  
  
  social_social_all<-table(q$mw.sports.social_picked_social) + table(q$mw.business.social_picked_social) +
    table(q$mw.entertainment.social_picked_social) + table(q$mw.worldnews.social_picked_social)
  
  
  social_social_all<-social_social_all/ sum(social_social_all)
  
  social_social_all[1]<-social_social_all[1]/3
  
  table(as.factor(q$mw.sports.both_picked_social))
  
  
  both_both_all<-table(q$mw.sports.both_picked_social) + table(q$mw.business.both_picked_social) +
    table(q$mw.entertainment.both_picked_social) + table(q$mw.worldnews.both_picked_social)
  
  
  both_both_all<-both_both_all/ sum(both_both_all)
  
  both_both_all[1]<-both_both_all[1]/3
  
  
  nums<-c(social_social_all,both_both_all)
  

  cue<-c("Weak", "Strong", "Weak", "Strong")
  con<-c("Endorsement Only", "Endorsement Only", "Social and Partisan", "Social and Partisan")
  
  
  poldf<-data.frame("nums" = nums,  "cue" = cue, "con" = con )
  
  png(filename = "plots/select_cue_info.png")
  
  
  ggplot(  poldf, aes(x = cue, y = nums, fill = con ) ) +
    
    geom_bar(stat="identity", position=position_dodge()) + scale_fill_grey() +
    theme_bw() + ggtitle("Impact of Social Cue") +
    
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank()) 
  
  dev.off()
  
  
  
  
  #####################look for het fx of social
  
  
  
  q$social_social_all<- rowMeans(cbind(q$mw.sports.social_picked_social, q$mw.business.social_picked_social,
                                     q$mw.worldnews.social_picked_social, q$mw.entertainment.social_picked_social  ), na.rm = T )
  
  
  
  q$both_both_all<- rowMeans(cbind(q$mw.sports.both_picked_social, q$mw.business.both_picked_social,
                                     q$mw.worldnews.both_picked_social, q$mw.entertainment.both_picked_social  ), na.rm = T )
  
  
 
  
  choosehigh_age <- lm(social_social_all ~ age_self + harg_mean + leaning, data=q) 
  

  summary(choosehigh_age)
  
  
  choosehigh_age_both <- lm(both_both_all ~ age_self + harg_mean + leaning, data=q) 
  
  dev.off()
  summary(choosehigh_age_both)
  
  
  png(filename = "plots/age_high_both.png")
  ggplot(q, aes(x = age_self, y = both_both_all)) +
    geom_smooth(method = "lm")+geom_rug(sides="b", alpha = 1/2, position = "jitter") +
    theme_bw() + ggtitle("Age Predicts Selecting High Endorsement: Source + Endorsement Condition") +
    
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank()) +  coord_cartesian(ylim = c(0, .3))
  
 dev.off()
 
 
 

 png(filename = "plots/age_high_social_only.png")
 
  
  ggplot(q, aes(x = age_self, y = social_social_all)) +
    geom_smooth(method = "lm")+geom_rug(sides="b", alpha = 1/2, position = "jitter") +
    theme_bw() + ggtitle("Age Predicts Selecting High Endorsement: Endorsement Only Condition") +
    
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank()) +  coord_cartesian(ylim = c(0, .3))
  
  
  dev.off()
  
  
  
  png(filename = "plots/dl_high_both.png")
  ggplot(q, aes(x = harg_mean, y = both_both_all)) +
    geom_smooth(method = "lm")+geom_rug(sides="b", alpha = 1/2, position = "jitter") +
    theme_bw() + ggtitle("DigLit Predicts Selecting High Endorsement: Source + Endorsement Condition") +
    
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank()) +  coord_cartesian(ylim = c(0, .3))
  
  dev.off()
  
  
  
  
  png(filename = "plots/dl_high_social_only.png")
  
  
  ggplot(q, aes(x = harg_mean, y = social_social_all)) +
    geom_smooth(method = "lm")+geom_rug(sides="b", alpha = 1/2, position = "jitter") +
    theme_bw() + ggtitle("DigLit Predicts Selecting High Endorsement: Endorsement Only Condition") +
    
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank()) +  coord_cartesian(ylim = c(0, .3))
  
  
  dev.off()