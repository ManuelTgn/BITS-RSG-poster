## RSG Bioinformatic Survey - Perception of Bioinformatics of STEM students

library(tidyverse)
library(ggplot2)
library(caret)
library(MASS)


##################### Pre-processing & EDA ####################################
# we start by importing the dataset
df <- read.csv("BioinformaticsSurvey2022.csv")
str(df)


# we then subset for STEM students by taking only students whose 
# main focus of study is NOT bioinformatics

table(df$Is.bioinformatics.the.main.focus.of.your.studies.)
# No Yes 
# 128 133 

df.stem <- subset(df[,c(2:25)], Is.bioinformatics.the.main.focus.of.your.studies. == "No")
str(df.stem)

table(df.stem$What.are.your.pronouns.)
#             He/him I prefer not to say                  Me             She/her           They/them 
#                 47                   3                   1                  76                   1 

table(df.stem$How.old.are.you.)
#    19-22    23-26    27-30    31-35  Over 35 Under 18 
#       39       67        8        5        8        1 

table(df.stem$What.is.your.current.position.)
# Bachelor’s student 
# 49 
# Employed in other academic positions (research assistant, facility technician, …) 
# 6 
# Employed outside academia (company, start-up, spin-off, …) 
# 10 
# Master's student 
# 50 
# PhD student (either in academia or industry) 
# 13 

table(df.stem$Have.you.ever.taken.a.course.in.computational.biology.bioinformatics.)
# No Yes 
# 45  83


meta <- c("Age", "Pronouns", "Origin", "Study.Place", "Off.Site","Current.Position", "Bioinformatics.Student", "Degree.Area","Heard.About","3.Words","Course.Taken")
work.perspectives <- c('Academia','Industry (R&D)', 'Data Analyst','IRCSS','Communication','Teaching','Entepreneurship','Editorial')
role.research <- c('Drug-development','Diagnostics', 'Disease characterization','Software Development','Data Analysis')

colnames(df.stem) <- c(meta, work.perspectives, role.research)
# ordered factors
df.stem[,c(work.perspectives,role.research)] <- lapply(df.stem[,c(work.perspectives,role.research)], FUN=function(c) factor(c, ordered = TRUE))
df.stem$Age <- factor(df.stem$Age, levels = c("Under 18","19-22", "23-26", "27-30", "31-35", "Over 35"), ordered = TRUE)

# non-ordered factors
df.stem[,c("Pronouns", "Origin", "Study.Place", "Off.Site", "Current.Position", 
          "Bioinformatics.Student", "Degree.Area", "Course.Taken", "Heard.About")] <- lapply(df.stem[,c("Pronouns", "Origin", "Study.Place", "Off.Site", "Current.Position", 
                                                                                        "Bioinformatics.Student", "Degree.Area", "Course.Taken", "Heard.About")], factor)
######################### Course Taken #######################################
# We can check whether having taken a course in bioinformatics influences both work perspectives 
# and/or bioinformatics' role in basic and applied research

df.stem.course.taken.work <- reshape2::melt(df.stem[,c("Course.Taken", work.perspectives)], id=c("Course.Taken"))

# How likely it is that a bioinformatician would work here? Stratified by course taken
ggplot(data=df.stem.course.taken.work, aes(x=Course.Taken, y=as.numeric(value), color=Course.Taken)) +
  facet_grid(cols=vars(variable)) +
  theme_bw() +
  theme(legend.position = "none") +
  stat_summary_bin() +
  labs(x="I took a course in bioinformatics", 
       y="Likelihood", 
       title = "How likely it is that a bioinformatician would work here?")

ggsave("work_perspective_stem_course_taken_yes_no.png", 
       units = "cm", height = 10, width = 25)

df.stem.course.taken.role <- reshape2::melt(df.stem[,c("Course.Taken", role.research)], 
                                            id=c("Course.Taken"))


# How much is bioinformatics involved in this area? Stratified by course taken
ggplot(data=df.stem.course.taken.role, aes(x=Course.Taken, y=as.numeric(value), color=Course.Taken)) +
  facet_grid(cols=vars(variable)) +
  theme_bw() +
  theme(legend.position = "none") +
  stat_summary_bin() +
  labs(x="I took a course in bioinformatics", 
       y="Involvement", 
       title = "How much is bioinformatics involved in this area?")
ggsave("role_bioinf_stem_course_taken_yes_no.png", 
       units = "cm", height = 10, width = 20)

## now we test both models with an ordinal logistic regression
set.seed(123)
train.idx <- createDataPartition(df.stem$Course.Taken, p=0.65, list=FALSE)

df.stem.train <- df.stem[train.idx,]
df.stem.test <- df.stem[-train.idx,]

Course.Taken.Academia.polr <- train(Academia~Course.Taken, data=df.stem.train, method="polr")
Course.Taken.Academia.pred <- predict(Course.Taken.Academia.polr, df.stem.test)
(postResample(pred=Course.Taken.Academia.pred, obs=df.stem.test$Academia))
# Accuracy       Kappa 
# 0.30555556     -0.03092784
# No statistical concordance k <= 0

confusionMatrix(Course.Taken.Academia.pred, reference = df.stem.test$Academia)

Course.Taken.Teaching.polr <- train(Teaching~Course.Taken, data=df.stem.train, method="polr")
Course.Taken.Teaching.pred <- predict(Course.Taken.Teaching.polr, df.stem.test)
(postResample(pred=Course.Taken.Teaching.pred, obs=df.stem.test$Teaching))
# Accuracy    Kappa 
# 0.372093    0.000000 
# No statistical concordance k <= 0

Course.Taken.Editorial.polr <- train(Editorial~Course.Taken, data=df.stem.train, method="polr")
Course.Taken.Editorial.pred <- predict(Course.Taken.Editorial.polr, df.stem.test)
(postResample(pred=Course.Taken.Editorial.pred, obs=df.stem.test$Editorial))
# Accuracy      Kappa 
# 0.32558140    0.06940299 
# Weak statistical concordance 0 < k <= 0.4

Course.Taken.Diagnostics.polr <- train(Diagnostics~Course.Taken, data=df.stem.train, method="polr")
Course.Taken.Diagnostics.pred <- predict(Course.Taken.Diagnostics.polr, df.stem.test)
(postResample(pred=Course.Taken.Diagnostics.pred, obs=df.stem.test$Diagnostics))
# Accuracy     Kappa 
# 0.3023256    0.0000000 

Course.Taken.Software.Development.polr <- train(`Software Development`~Course.Taken, data=df.stem.train, method="polr")
Course.Taken.Software.Development.pred <- predict(Course.Taken.Software.Development.polr, df.stem.test)
(postResample(pred=Course.Taken.Software.Development.pred, obs=df.stem.test$`Software Development`))
# Accuracy      Kappa 
# 0.23255814    0.01183844 

Course.Taken.Editorial.polr <- train(`Data Analysis`~Course.Taken, data=df.stem.train, method="polr")
Course.Taken.Editorial.pred <- predict(Course.Taken.Editorial.polr, df.stem.test)
(postResample(pred=Course.Taken.Editorial.pred, obs=df.stem.test$`Data Analysis`))
# Accuracy     Kappa 
# 0.6046512    0.0000000 

######################## Heard about ########################################
# The division between heard about/not heard about is quite strong, 
# not sure whether we can stratify by this variable

table(df.stem$Heard.About)
# No, never 
# 3
# Yes, but I'm not familiar with the term 
# 26 
# Yes, I'm familiar with the term 
# 99 


# We split Heard.About into two single categories (Yes, No)
# Yes : Yes, I'm familiar with the term
# No : No + Yes, but I'm not familiar with the term

df.stem$Heard.About.bin <- factor(ifelse(df.stem$Heard.About == "Yes, I'm familiar with the term", "Yes", "No"))

table(df.stem$Heard.About.bin)
# No Yes 
# 29  99 

df.stem.heard.about.work <- reshape2::melt(df.stem[,c("Heard.About.bin", work.perspectives)], 
                                           id=c("Heard.About.bin"))

# How likely it is that a bioinformatician would work here? Stratified by course taken
ggplot(data=df.stem.heard.about.work, aes(x=Heard.About.bin, y=as.numeric(value), color=Heard.About.bin)) +
  facet_grid(cols=vars(variable)) +
  theme_bw() +
  theme(legend.position = "none") +
  stat_summary_bin() +
  labs(x="I have heard about bioinformatics", 
       y="Likelihood", 
       title = "How likely it is that a bioinformatician would work here?")

ggsave("work_perspective_stem_heard_about_yes_no.png", 
       units = "cm", height = 10, width = 25)

df.stem.heard.about.role <- reshape2::melt(df.stem[,c("Heard.About.bin", role.research)], 
                                            id=c("Heard.About.bin"))


# How much is bioinformatics involved in this area? Stratified by course taken
ggplot(data=df.stem.heard.about.role, aes(x=Heard.About.bin, y=as.numeric(value), color=Heard.About.bin)) +
  facet_grid(cols=vars(variable)) +
  theme_bw() +
  stat_summary_bin() +
  theme(legend.position = "none") +
  labs(x="I have heard about bioinformatics", 
       y="Involvement", 
       title = "How much is bioinformatics involved in this area?")
ggsave("role_bioinf_stem_heard_about_yes_no.png", 
       units = "cm", height = 10, width = 20)





