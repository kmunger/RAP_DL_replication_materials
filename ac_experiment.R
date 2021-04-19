############# Anspach and Carlson ##############
rm(list = ls())
options(stringsAsFactors = FALSE)
library(dplyr)
library(stargazer)
library(ggplot2)
library(gridExtra)


#set working directory
setwd("C:/Users/kevin/Desktop/rap_replication")

# load data - (choose sample to analyze) 
load("data/q.Rdata") # 10759 X 112

# # w/o mid attention check 
# load("data/q_noMid.Rdata") # 11783 X 111
# q <- q_noMid

# # w/o mid attention check & only the 1st 9000
# load("data/q_9k.Rdata") # 9000 X 111
# q <- q_9k

# Create factor variable and labels 
q$ac_condition <- factor(q$Block3_DO,
                         levels = c("Q4.1|Q59",
                                    "Q4.2|Q59", 
                                    "Q4.3|Q59"),
                         labels = c("Preview + con. commentary", 
                                    "Preview + lib. commentary",
                                    "Article preview"))

# relevel and make no commentary - 'Article preview' the reference group
q$ac_condition <- relevel(q$ac_condition, 
                          ref="Article preview")

table(q$ac_condition, useNA = "always")


# party affiliation == q$leaning - this is a continuous variable

# Divide age into 4 age groups - based on quartiles
summary(q$age_self)
# Min=18, 1st Qu.=31, Median=45, 3rd Qu.=61, Max=99
q$age_group <- NA
q$age_group[q$age_self < 31] <- '18-31'
q$age_group[q$age_self >= 31 & q$age_self < 45] <- '31-45'
q$age_group[q$age_self >= 45 & q$age_self < 61] <- '45-61'
q$age_group[q$age_self >= 61] <- '61-99'

# Divide dl into 4 groups - - based on quartiles
summary(q$harg_mean) 
# Min=1, 1st Qu.=3.1, Median=3.9, 3rd Qu.=4.5, Max = 5

q$dl_group <- NA
q$dl_group[q$harg_mean < 3.1] <- '1-3.1'
q$dl_group[q$harg_mean >= 3.1 & q$harg_mean < 3.9] <- '3.1-3.9'
q$dl_group[q$harg_mean >= 3.9 & q$harg_mean < 4.5] <- '3.9-4.5'
q$dl_group[q$harg_mean >= 4.5] <- '4.5-5'
table(q$dl_group, useNA = "always")

##trust regression

# Trim all white space 
q$Q5.4 <- trimws(q$Q5.4)
q$Q5.5 <- trimws(q$Q5.5)
q$Q5.6 <- trimws(q$Q5.6)

# Code Q5.4 -- trust in person
q$trust_person<-as.numeric(factor(q$Q5.4, levels = c("Very untrustworthy", 
                                                     "Somewhat untrustworthy", 
                                                     "Not sure", 
                                                     "Somewhat trustworthy", 
                                                     "Very trustworthy")))

# Code Q5.5 -- trust in outlet
q$trust_outlet<-as.numeric(factor(q$Q5.5, levels = c("Very untrustworthy", 
                                                     "Somewhat untrustworthy",
                                                     "Not sure", 
                                                     "Somewhat trustworthy", 
                                                     "Very trustworthy")))


# Q5.6 -- trust in poll
q$trust_poll<-as.numeric(factor(q$Q5.6, levels = c("Very untrustworthy", 
                                                   "Somewhat untrustworthy", 
                                                   "Not sure", 
                                                   "Somewhat trustworthy",
                                                   "Very trustworthy")))


# reg trust - age with no interactions 
fit.trust_outlet_noInteractions <- lm(trust_outlet ~ ac_condition + age_self, data = q)
fit.trust_poll_noInteractions <- lm(trust_poll ~ ac_condition + age_self, data = q)
fit.trust_person_noInteractions <- lm(trust_person ~ ac_condition + age_self, data = q)

# reg trust - age
fit.trust_outlet <- lm(trust_outlet ~ ac_condition * age_self, data = q)
fit.trust_poll <- lm(trust_poll ~ ac_condition * age_self, data = q)
fit.trust_person <- lm(trust_person ~ ac_condition * age_self, data = q)

# reg trust - age - with more control variables 
fit.trust_outlet_controls <- lm(trust_outlet ~ ac_condition * age_self +
                                  male + 
                                  white +
                                  leaning,
                                data = q)

fit.trust_poll_controls <- lm(trust_poll ~ ac_condition * age_self +
                                male + 
                                white +
                                leaning,
                              data = q)

fit.trust_person_controls <- lm(trust_person ~ ac_condition * age_self +
                                  male + 
                                  white +
                                  leaning,
                                data = q)

# create a table
stargazer(fit.trust_outlet_noInteractions, fit.trust_outlet, fit.trust_outlet_controls,
          fit.trust_outlet_noInteractions, fit.trust_poll, fit.trust_poll_controls,
          fit.trust_person_noInteractions, fit.trust_person, fit.trust_person_controls,
          covariate.labels = c("Preview + con. commentary",
                               "Preview + lib. commentary",
                               "Age",
                               "Male",
                               "White",
                               "Party",
                               "Preview + con. commentary * Age",
                               "Preview + lib. commentary * Age",
                               "Constant"),
          dep.var.labels=c("News Outlet", "Author/poster", "Cited poll"),
          dep.var.caption = c(""),
          model.numbers = FALSE,
          single.row = TRUE,
          #type='html',
          #out = "tables/fit_age.html"
          type='latex',
          out = "tables/fit_age.tex"
          )


# reg trust - dl without interactions 
harg.trust_outlet_noInteractions <- lm(trust_outlet ~ ac_condition + harg_mean, data = q)
harg.trust_poll_noInteractions <- lm(trust_poll ~ ac_condition + harg_mean, data = q)
harg.trust_person_noInteractions <- lm(trust_person ~ ac_condition + harg_mean, data = q)


# reg trust - dl
harg.trust_outlet <- lm(trust_outlet ~ ac_condition * harg_mean, data = q)
harg.trust_poll <- lm(trust_poll ~ ac_condition * harg_mean, data = q)
harg.trust_person <- lm(trust_person ~ ac_condition * harg_mean, data = q)

# reg trust - dl - with more controls 
harg.trust_outlet_controls <- lm(trust_outlet ~ ac_condition * harg_mean +
                                   male +
                                   white +
                                   leaning,
                                 data = q)

harg.trust_poll_controls <- lm(trust_poll ~ ac_condition * harg_mean +
                                 male +
                                 white +
                                 leaning,
                               data = q)

harg.trust_person_controls <- lm(trust_person ~ ac_condition * harg_mean +
                                   male +
                                   white +
                                   leaning,
                                 data = q)

# create a table
stargazer(harg.trust_outlet_noInteractions, harg.trust_outlet, harg.trust_outlet_controls,
          harg.trust_poll_noInteractions, harg.trust_poll, harg.trust_poll_controls,
          harg.trust_person_noInteractions, harg.trust_person, harg.trust_person_controls,
          covariate.labels = c("Preview + con. commentary",
                               "Preview + lib. commentary",
                               "Digital Literacy",
                               "Male",
                               "White",
                               "Party",
                               "Preview + con. commentary * Digital Literacy",
                               "Preview + lib. commentary * Digital Literacy",
                               "Constant"),
          dep.var.labels=c('New Outlet','Author/Poster', 'Cited poll'),
          dep.var.caption = c(""),
          model.numbers = FALSE,
          single.row = TRUE,
          type='latex',
          out = "tables/fit_dl.tex"
          #type='html',
          #out = "tables/fit_dl.html"
          )

# reg trust power_mean
power.trust_outlet <- lm(trust_outlet ~ ac_condition * power_mean, data = q)
power.trust_poll <- lm(trust_poll ~ ac_condition * power_mean, data = q)
power.trust_person <- lm(trust_person ~ ac_condition * power_mean, data = q)

# correct answer
q$poll_36<-0
q$poll_36[q$Q5.2 == "36%"]<-1

# wrong answer conservative commentary
q$poll_49<-0
q$poll_49[q$Q5.2 == "49%"]<-1

# wrong answer liberal commentary
q$poll_23<-0
q$poll_23[q$Q5.2 == "23%"]<-1

# What predicts choosing the correct answer - 36% - for the three treatments?
choose36_age <- lm(poll_36 ~ age_self*ac_condition, data=q) 

stargazer(choose36_age,
          covariate.labels = c("Age",
                               "Preview + con. commentary",
                               "Preview + lib. commentary",
                               "Preview + con. commentary * Age",
                               "Preview + lib. commentary * Age",
                               "Constant"),
          dep.var.labels=c(""),
          dep.var.caption = c("Choose correct Answer - 36%"),
          model.numbers = FALSE,
          single.row = TRUE,
          type='latex',
          out = "tables/choose36_age.tex"
          #type='html',
          #out = "tables/choose36_age.html"
          )


p36_age <- plot_model(choose36_age, type = 'int', colors = c("grey", "red", "blue")) + 
  labs(title = "",
       x = "Age",
       y = "Choose 36% \n (0=did not choose 36%; 1=choose 36%)",
       color = "Treatment") + theme(legend.position = "none")

choose36_dl <- lm(poll_36 ~ harg_mean*ac_condition, data=q) 

stargazer(choose36_dl,
          covariate.labels = c("Digit Literacy",
                               "Preview + con. commentary",
                               "Preview + lib. commentary",
                               "Preview + con. commentary * Digit Literacy",
                               "Preview + lib. commentary * Digit Literacy",
                               "Constant"),
          dep.var.labels=c(""),
          dep.var.caption = c("Choose correct Answer - 36%"),
          model.numbers = FALSE,
          single.row = TRUE,
         # type='html',
          #out = "tables/choose36_dl.html"
          type='latex',
          out = "tables/choose36_dl.tex"
          )



p36_dl <- plot_model(choose36_dl, type = 'int', colors = c("grey", "red", "blue")) + 
  labs(title = "",
       x = "Digital Literacy",
       y = "Choose 36% \n (0=did not choose 36%; 1=choose 36%)",
       color = "Treatment") + theme(legend.position = "bottom")

# function to add all plots to single page and combining legend

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend<-g_legend(p36_dl)

p36_combo <- grid.arrange(arrangeGrob(p36_age + theme(legend.position="none"),
                                      p36_dl + theme(legend.position="none"),
                                      nrow=1),
                          mylegend, 
                          nrow=2,
                          heights=c(10, 1))


# Replicate the bar plot from Anspach and Carlson (2018)
# Create df for each type of stimiuli 
fig_t <- table(q$Q5.2, q$ac_condition, useNA = "no")
fig_t <- round((fig_t/colSums(fig_t))*100, 2)
fig_t <- fig_t[2:4,] %>% t()
fig_t <- fig_t[,c(2, 1, 3)]  

d1 <- fig_t[,1]%>% data.frame()
names(d1) <- "perct"
d1$name <- rownames(d1)

p1 <- d1 %>%
  ggplot(aes(x=name, y=perct)) + 
  geom_bar(stat = "identity") +
  ylim(0, 100) + 
  scale_x_discrete(labels= c("Article Preview",
                             "Preview + \n 49% Comment",
                             "Preview + \n 23% Comment"))+ 
  labs(x = "\n Correctly Identified 36% as Correct Rating", 
       y = "% of Participants Selecting Each Opinion")

d2 <- fig_t[,2]%>% data.frame()
names(d2) <- "perct"
d2$name <- rownames(d2)

p2 <- d2 %>%
  ggplot(aes(x=name, y=perct)) + 
  geom_bar(stat = "identity") +
  ylim(0, 100) + 
  scale_x_discrete(labels= c("Article Preview",
                             "Preview + \n 49% Comment",
                             "Preview + \n 23% Comment"))+ 
  labs(x = "\n Mistakenly Identified 23% as Correct Rating",
       y = "")

d3 <- fig_t[,3]%>% data.frame()
names(d3) <- "perct"
d3$name <- rownames(d3)

p3 <- d3 %>%
  ggplot(aes(x=name, y=perct)) + 
  geom_bar(stat = "identity") +
  ylim(0, 100) + 
  scale_x_discrete(labels= c("Article Preview",
                             "Preview + \n 49% Comment",
                             "Preview + \n 23% Comment"))+ 
  labs(x = "\n Mistakenly Identified 49% as Correct Rating",
       y ="")

p4 <- grid.arrange(p1,p2,p3, nrow=1)

# subset the dataset for manipulation checks 
# Commentary = conservative slant - "surveyed more dems"
qcons<-filter(q, ac_condition =="Preview + con. commentary") 
# Commentary = liberal slant
qlib<-filter(q, ac_condition == "Preview + lib. commentary" )
# No Commentary 
qnor<-filter(q, ac_condition =="Article preview")


## manipulation check - Overall 
# cited flaw in the poll 
flaw_cons <- round(prop.table(table(qcons$Q5.3)), 2)*100
flaw_lib <- round(prop.table(table(qlib$Q5.3)), 2)*100
flaw_nor <- round(prop.table(table(qnor$Q5.3)), 2)*100
#
l <- list(flaw_nor,flaw_lib, flaw_cons)
l <- lapply(l, as.data.frame)
f <- function(x, y) merge(x, y, by="Var1", all=TRUE)
l <- Reduce(f, l)
stargazer(l,
          summary = FALSE,
          dep.var.caption = "Type of commentary",
          covariate.labels = c("Perceived Flaw",
                               "Article Preview(%)",
                               "'Oversampled Republicans' comment (%)",
                               "'Oversampled Democrats' comment (%)"),
          rownames = FALSE,
          type = "latex",
          out = "tables/manipulation_check_commentary.tex"
          #type = "html",
          #out = "tables/manipulation_check_commentary.html"
          )


## manipulation check by Age 
flaw_cons_age <- (round(prop.table(table(qcons$Q5.3, qcons$age_group)), 2)*100)
flaw_lib_age <- (round(prop.table(table(qlib$Q5.3, qlib$age_group)), 2)*100)
flaw_nor_age <- (round(prop.table(table(qnor$Q5.3, qnor$age_group)), 2)*100)

t <- cbind(flaw_nor_age, flaw_lib_age, flaw_cons_age) %>% data.frame()
stargazer(t, 
          summary = FALSE,
          type = "latex",
          out = "tables/manipulation_check_commentary_by_age.tex",
          #type = "html",
          #out = "tables/manipulation_check_commentary_by_age.html",
          align = TRUE)

## manipulation check by Digital Literacy  
flaw_cons_dl <- (round(prop.table(table(qcons$Q5.3, qcons$dl_group)), 2)*100)
flaw_lib_dl <- (round(prop.table(table(qlib$Q5.3, qlib$dl_group)), 2)*100)
flaw_nor_dl <- (round(prop.table(table(qnor$Q5.3, qnor$dl_group)), 2)*100)

dl_t <- cbind(flaw_nor_dl, flaw_lib_dl, flaw_cons_dl) %>% data.frame()
stargazer(dl_t, 
          summary = FALSE,
          type = "latex",
          out = "tables/manipulation_check_commentary_by_dl.tex",
          #type = "html",
          #out = "tables/manipulation_check_commentary_by_dl.html",
          align = TRUE)



