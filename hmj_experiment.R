rm(list = ls())
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

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


# convert answers to ordinal rank 
# 1 = Strongly Support
# 7 = Strongly Oppose
q$gun_oppose_num<-as.numeric(gsub("[^0-9.]", "",  q$Q12.3))

q$Block4_DO

# indicators for experimental condition:

# individual rights frame
q$gun_rights<-0
q$gun_rights[grepl("12.1", q$Block4_DO, fixed = T)]<-1
# public safety frame
q$gun_threat<-0
q$gun_threat[grepl("12.2", q$Block4_DO, fixed = T)]<-1


### create seperate dataframes for each condition
qrights<-filter(q, gun_rights ==1 )
qthreat<-filter(q, gun_threat ==1 )

# plot responses within each condition
# public safety 
mean(qthreat$gun_oppose_num, na.rm = TRUE)

x_axis_labels = 1:7

plot_threats <- ggplot(qthreat, aes(x = gun_oppose_num)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent, limits = c(0, .35)) +
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+
  labs(x = "1 = strongly support, 7 = strongly oppose",
       y = "Proportion of Respondents", 
       title = "Public Safety frame; ave=4.12")


# individual rights 
mean(qrights$gun_oppose_num, na.rm = TRUE)

x_axis_labels = 1:7

plot_rights <- ggplot(qrights, aes(x = gun_oppose_num)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent, limits = c(0, .35)) +
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)+
  labs(x = "1 = strongly support, 7 = strongly oppose",
       y = "Proportion of Respondents", 
       title = "Individual rights frame; ave=3.32")

guns_plots <- grid.arrange(plot_threats, plot_rights, ncol=1)

ggsave(plot = guns_plots,
       filename = "plots/guns_plots.png",
       height = 12,
       width = 6)

# Ordered logit regrssion 
# Determinants of Opposition to Concealed Handgun Law 
library(MASS)
# 1 = Strongly Support
# 7 = Strongly Oppose

# OLS: with age - interactions
guns.age.noInteraction<- lm(gun_oppose_num ~ gun_rights + age_self + leaning + female, data = q)
summary(guns.age.noInteraction)

# OLS: with age + interaction
guns.age.lr<- lm(gun_oppose_num ~ gun_rights * age_self + leaning + female, data = q)
summary(guns.age.lr)

# OLS: with digital lit - interactions 
guns.dl.noInteraction<- lm(gun_oppose_num ~ gun_rights + harg_mean + leaning + female, data = q)
summary(guns.dl.noInteraction)

# OLS: with digital lit + interaction
guns.dl.lr<- lm(gun_oppose_num ~ gun_rights * harg_mean + leaning + female, data = q)
summary(guns.dl.lr)

#### Ordered Logistic Regression ###
q$gun_oppose_num <- as.ordered(q$gun_oppose_num)

# Ordered logit: with age - interaction
guns.age.logit.noInteraction <- polr(gun_oppose_num ~ gun_rights + age_self + leaning + female , data = q, Hess=TRUE)
summary(guns.age.logit.noInteraction)

# Ordered logit: with age + interaction
guns.age.logit <- polr(gun_oppose_num ~ gun_rights * age_self + leaning + female , data = q, Hess=TRUE)
summary(guns.age.logit)

# Ordered logit: with digital lit - interaction
guns.dl.logit.noInteraction <- polr(gun_oppose_num ~ gun_rights + harg_mean + leaning + female , data = q, Hess=TRUE)
summary(guns.dl.logit.noInteraction)

# Ordered logit: with digital lit + interaction
guns.dl.logit <- polr(gun_oppose_num ~ gun_rights * harg_mean + leaning + female , data = q, Hess=TRUE)
summary(guns.dl.logit)

# # logit p values 
# (ctable.age <- coef(summary(guns.age.logit)))
# p.age <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable.age <- cbind(ctable.age, "p value" = p))
# If you are in the gun rights condition 
# we expect -0.925 decrease in the expected value of gun opposition 
# in the log odds scale 
# given all other variables is the model are held constat 

# print table (guns.age.logit) 
stargazer(guns.age.logit.noInteraction,
          guns.age.logit, 
          guns.age.noInteraction,
          guns.age.lr,
          ord.intercepts = TRUE,
          model.numbers = FALSE,
          single.row = TRUE,
          covariate.labels = c( "Gun Rights Frame",
                                "Age",
                                "Party",
                                "Female",
                                "Gun Rights Frame * Age"),
          dep.var.caption = "Determinants for opposition to concealed handgun laws",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          column.labels = c("Ordered Logit", "Ordered Logit", "OLS", "OLS"),
          type = "latex",
          out = "tables/handguns_results_age.tex"
          #type = "text",
          #out = "tables/handguns_results_age.html"
          )
          
# print table (guns.age.logit) 
stargazer(guns.dl.logit.noInteraction,
          guns.dl.logit, 
          guns.dl.noInteraction,
          guns.dl.lr,
          ord.intercepts = TRUE,
          model.numbers = FALSE,
          single.row = TRUE,
          covariate.labels = c( "Gun Rights Frame",
                                "Digital Literacy",
                                "Party",
                                "Female",
                                "Gun Rights Frame * Digital Lit"),
          dep.var.caption = "Determinants for opposition to concealed handgun laws",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          column.labels = c("Ordered Logit", "Ordered Logit", "OLS", "OLS"),
          type = "latex",
          out = "tables/handguns_results_dl.tex"
          #type = "text",
          #out = "tables/handguns_results_dl.html"
)


          