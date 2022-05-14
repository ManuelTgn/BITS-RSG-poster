## RSG Bioinformatic Survey - Perception of Bioinformatics of STEM students

library(tidyverse)
library(ggplot2)
library(corrplot)

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

# We can check whether having taken a course in bioinformatics influences both work perspectives 
# and/or bioinformatics' role in basic and applied research
meta <- c("Age", "Pronouns", "Origin", "Study.Place", "Off.Site","Current.Position", "Bioinformatics.Student", "Degree.Area","Heard.About","3.Words","Course.Taken")
work.perspectives <- c('Academia','Industry (R&D)', 'Data Analyst','IRCSS','Communication','Teaching','Entepreneurship','Editorial')
role.research <- c('Drug-development','Diagnostics', 'Disease characterization','Software Development','Data Analysis')

colnames(df.stem) <- c(meta, work.perspectives, role.research)

df.stem.course.taken.work <- reshape2::melt(df.stem[,c("Course.Taken", work.perspectives)], id=c("Course.Taken"))

ggplot(data=df.stem.course.taken.work, aes(x=Course.Taken, y=value)) +
  facet_grid(cols=vars(variable)) +
  theme_bw() +
  stat_summary_bin() +
  labs(x="I took a course in bioinformatics", 
       y="Likelihood", 
       title = "How likely it is that a bioinformatician would work here?")

ggsave("work_perspective_stem_course_taken_yes_no.png", 
       units = "cm", height = 10, width = 25)

df.stem.course.taken.role <- reshape2::melt(df.stem[,c("Course.Taken", role.research)], 
                                            id=c("Course.Taken"))


ggplot(data=df.stem.course.taken.role, aes(x=Course.Taken, y=value)) +
  facet_grid(cols=vars(variable)) +
  theme_bw() +
  stat_summary_bin() +
  labs(x="I took a course in bioinformatics", 
       y="Involvement", 
       title = "How much is bioinformatics involved in this area?")
ggsave("role_bioinf_stem_course_taken_yes_no.png", 
       units = "cm", height = 10, width = 20)

