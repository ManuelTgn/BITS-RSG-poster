library(RColorBrewer) 
library(wordcloud2)
library(ggplot2)
library(ggrepel)
library(dplyr)

##### Import data #####
## Read in data and split dataframe between STEM and Bioinformatics students
data<-read.csv2("data/BioinformaticsSurvey2022.csv",sep=',') 
data_bioinf<-data[data$Is.bioinformatics.the.main.focus.of.your.studies.=='Yes',c(64:71)]
data_STEM<-data[data$Is.bioinformatics.the.main.focus.of.your.studies.=='No',c(9:25)]

# change colnames
other_info<-c("Degree.Area","Heard.About","3.Words","Course.Taken")
work_perspectives <- c('Academia','Industry (R&D)', 'Data Analyst','IRCSS','Communication','Teaching','Entepreneurship','Editorial')
role_research <- c('Drug-development','Diagnostics', 'Disease characterization','Software Development','Data Analysis')
colnames(data_STEM)<-c(other_info,work_perspectives,role_research)
colnames(data_bioinf)<-work_perspectives

##### Word cloud #####
## Create a word cloud with the terms used by STEM students to describe bioinformatics

# extract words 
words<-unlist(strsplit(data_STEM$`3.Words`,',',fixed=T))

# preprocess
words<-trimws(words)
words<-tolower(words)
 
# correct words
tmp<-words[28]
words<-words[-28]
words<-c(words,trimws(unlist(strsplit(tmp,'/'))))
words<-words[words!="the use of a informatica tool to understand anything bio related"] 
words[grep('machine',words)]<-'machine-learning'
words[grep('single',words)]<-'single-cell'
words[grep('big',words)]<-'big-data'
words[grep('rapid',words)]<-'rapid'
words<-unlist(strsplit(words,' '))
words[grep('cha',words)]<-'challenging'
words[grep('stat',words)]<-'statistics'
words[grep('useful',words)]<-'useful'
words[grep('utile',words)]<-'useful'
words[grep('model',words)]<-'modeling'
words[grep('analisis',words)]<-'analysis'
words[grep('anal',words)]<-'analysis'
words[grep('tool',words)]<-'tools'
words[grep('programs',words)]<-'tools' 
words[grep('seq',words)]<-'sequencing'
words[grep('algorithm',words)]<-'algorithm'
words[grep('science',words)]<-'science'
words[grep('powerfull',words)]<-'powerful'
words[grep('confusa',words)]<-'confusing'
words[grep('programmazione',words)]<-'programming'
words[grep('moderna',words)]<-'modern'
words[grep('pred',words)]<-'prediction'
words[grep('informatic',words)]<-'informatics'
words[grep('precis',words)]<-'precise' 
words[grep('sistemi',words)]<-'systems' 
words[grep('pratic',words)]<-'practical' 
words[grep('found',words)]<-'fundamental' 
words[grep('intere',words)]<-'interesting'
words[grep('intesting',words)]<-'interesting'
words[grep('biol',words)]<-'biology'
words[grep('visulization',words)]<-'visualization'
words[grep('helpful',words)]<-'helpful'
words[grep('innov',words)]<-'innovative'
words[grep('complex',words)]<-'complexity'
words[grep('fondamental',words)]<-'fundamental'
words[grep('computation',words)]<-'computation'
words[grep('database',words)]<-'databases'
words[grep('comunication',words)]<-'communication'
words[grep('genetic',words)]<-'genetics'
words[grep('computing',words)]<-'computation'
words[grep('geomes',words)]<-'genomes'
words[grep('indispensabile',words)]<-'indispensable'
words[words%in%'omic']<-'omics'
words<-words[!words%in%c('a','in','very','the','i','and','of','to','for','know','dont',"don’t")]
 
#table(words) 
wordcloud2(data.frame(table(words)),shape='circle',color='random-dark',shuffle=F,size=1.2) 
 
########## DEGREE AREA ###############
##### STEM student's degree area #####
data_STEM_degree<-data.frame(data_STEM[,c("Degree.Area")])

donut_chart_fc <- function(data,title_name) {
    colnames(data)[1] <- "category" 
    data <- data %>%
        group_by(category) %>%
        mutate(count = n()) %>% unique()
    data$fraction = data$count / sum(data$count)
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax = cumsum(data$fraction)
    # Compute the bottom of each rectangle
    data$ymin = c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    # Compute a good label
    data$label <- paste0(round(data$fraction*100,digits = 2),"%")
    # Make the plot
    donut_chart_plot <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
        geom_rect() + 
        scale_fill_brewer(palette=4) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        theme_void() +
        guides(fill = guide_legend(
            title = "Legend",
            override.aes = aes(label = ""))) +
        theme(legend.position = "left",legend.title = element_text(size = 12),plot.title = element_text(size = 20)) +
        ggtitle(title_name) + geom_label_repel(mapping = aes(x=3.57,y=labelPosition,label = label))
    
    return(donut_chart_plot)
}

STEM_degree_chart<-donut_chart_fc(data_STEM_degree,"STEM Degree Area")
ggsave("src/STEM/STEM_degree_area.png", STEM_degree_chart, width = 16, height = 8)


##### STEM student's knowledge on bioinformatics (stratified by degree area) #####  
## We plot for each degree area whether the students heard about bioinformatics.
data_STEM_heard_by_degree<-data_STEM[,c("Degree.Area",
                                        "Heard.About")] 

#We only take the three main degree areas available (Computer science, engineering and life sciences)
data_STEM_heard_by_degree<-data_STEM_heard_by_degree[data_STEM_heard_by_degree$Degree.Area %in% c("Life Sciences (Biology/Biotechnology)",
                                                                                                  "Computer Science",
                                                                                                   "Engineering"),]
data_STEM_heard_by_degree<-as.data.frame(table(data_STEM_heard_by_degree))
plot_STEM_heard_by_degree<-ggplot(data = data_STEM_heard_by_degree,
       aes(x = Degree.Area, y = Freq,
           fill = Heard.About)) + 
    geom_bar(position='fill',stat='identity',show.legend=TRUE) + 
    scale_y_continuous(labels=scales::percent) +
    labs(x = "Degree Area", 
         y = "Relative frequency", 
         title  = "Have STEM students ever heard of bioinformatics?") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 50, hjust=1)) +
    scale_fill_manual(values=c("#245668","#04817E","#39AB7E")) +
    guides(fill = guide_legend(
        title = "Legend",
        override.aes = aes(label = ""))) +
    theme(legend.position = "right",legend.title = element_text(size = 12),plot.title = element_text(size = 20)) 

ggsave("src/STEM/STEM_heard_by_degree_area.png", plot_STEM_heard_by_degree, width = 16, height = 8)


##### STEM student's bioinformatics courses (stratified by degree area) ##### 
# We plot for each degree area whether the students ever had a bioinformatics course
data_STEM_course_by_degree<-data_STEM[,c("Degree.Area",
                                        "Course.Taken")] 

# We only take the three main degree areas available (Computer science, engineering and life sciences)
data_STEM_course_by_degree<-data_STEM_course_by_degree[data_STEM_course_by_degree$Degree.Area %in% c("Life Sciences (Biology/Biotechnology)",
                                                                                                  "Computer Science",
                                                                                                  "Engineering"),]
data_STEM_course_by_degree<-as.data.frame(table(data_STEM_course_by_degree))

plot_STEM_course_by_degree<-ggplot(data = data_STEM_course_by_degree,
                                  aes(x = Degree.Area, y = Freq,
                                      fill = Course.Taken)) + 
    geom_bar(position='fill',stat='identity',show.legend=TRUE) + 
    scale_y_continuous(labels=scales::percent) +
    labs(x = "Degree Area", 
         y = "Relative frequency", 
         title  = "Have STEM students ever taken a bioinformatics course?") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 50, hjust=1)) +
    scale_fill_manual(values=c("#245668","#39AB7E")) +
    guides(fill = guide_legend(
        title = "Legend",
        override.aes = aes(label = ""))) +
    theme(legend.position = "right",legend.title = element_text(size = 12),plot.title = element_text(size = 20)) 

ggsave("src/STEM/STEM_course_by_degree_area.png", plot_STEM_course_by_degree, width = 16, height = 8)

##### STEM student's perception of workplace (stratified by degree area) ##### 
data_STEM_workplace_by_degree <- reshape2::melt(data_STEM[,c("Degree.Area", work_perspectives)], id=c("Degree.Area"))

#filter for main degree areas
data_STEM_workplace_by_degree<-data_STEM_workplace_by_degree[data_STEM_workplace_by_degree$Degree.Area %in% c("Life Sciences (Biology/Biotechnology)",
                                                                                                              "Computer Science",
                                                                                                              "Engineering"),]
#rename life sciences
data_STEM_workplace_by_degree[data_STEM_workplace_by_degree$Degree.Area %in% "Life Sciences (Biology/Biotechnology)",]$Degree.Area<-'Life Sciences'
                                                                                                              

# How likely it is that a bioinformatician would work here? Stratified by degree area
ggplot(data=data_STEM_workplace_by_degree, aes(x=Degree.Area, y=as.numeric(value), color=Degree.Area)) +
    facet_grid(cols=vars(variable)) +
    theme_bw() +
    theme(legend.position = "none") +
    stat_summary_bin() +
    labs(x="Degree Area", 
         y="Score", 
         title = "How likely is it that a bioinformatician would work here?") +
    theme(axis.text.x = element_text(angle = 50, hjust=1))  +
    scale_color_manual(values=c("#245668","#04817E","#39AB7E")) + ylim(1,5)

ggsave("src/STEM/STEM_workplace_by_degree_area.png", 
       units = "cm", height = 10, width = 25)

##### STEM student's perception of bioinformatician role (stratified by degree area) ##### 
data_STEM_role_by_degree <- reshape2::melt(data_STEM[,c("Degree.Area", role_research)], id=c("Degree.Area"))

#filter for main degree areas
data_STEM_role_by_degree<-data_STEM_role_by_degree[data_STEM_role_by_degree$Degree.Area %in% c("Life Sciences (Biology/Biotechnology)",
                                                                                                              "Computer Science",
                                                                                                              "Engineering"),]
#rename life sciences
data_STEM_role_by_degree[data_STEM_role_by_degree$Degree.Area %in% "Life Sciences (Biology/Biotechnology)",]$Degree.Area<-'Life Sciences'


# How likely it is that a bioinformatician would be involved in such an activity? Stratified by degree area
ggplot(data=data_STEM_role_by_degree, aes(x=Degree.Area, y=as.numeric(value), color=Degree.Area)) +
    facet_grid(cols=vars(variable)) +
    theme_bw() +
    theme(legend.position = "none") +
    stat_summary_bin() +
    labs(x="Degree Area", 
         y="Score", 
         title = "How much do you think bioinformatics is involved in the following activities?") +
    theme(axis.text.x = element_text(angle = 50, hjust=1))  +
    scale_color_manual(values=c("#245668","#04817E","#39AB7E")) + ylim(1,5)

ggsave("src/STEM/STEM_role_by_degree_area.png", 
       units = "cm", height = 10, width = 25)

##########STEM and Bioinformatics students comparison#################
##### Perception of workplace between STEM and bioinformaticians ##### 
# We investigate the relationship between the perception of STEM students of where a bioinformatician works and where bioinformatics students see themselves working in the future
data_bioinf$category<-'BIOINF'
data_STEM$category<-'STEM'

data_workplace_by_category<-rbind(data_bioinf[,c('category',work_perspectives)], 
                     data_STEM[,c('category',work_perspectives)])
  
data_workplace_by_category <- reshape2::melt(data_workplace_by_category[,c("category", work_perspectives)], id=c("category"))

# How likely it is that a bioinformatician would be involved in such an activity? Stratified by degree area
ggplot(data=data_workplace_by_category, aes(x=category, y=as.numeric(value), color=category)) +
    facet_grid(cols=vars(variable)) +
    theme_bw() +
    theme(legend.position = "none") +
    stat_summary_bin() +
    labs(x="Degree Area", 
         y="Score", 
         title = "Where would a bioinformatician work?") +
    theme(axis.text.x = element_text(angle = 50, hjust=1)) +
    ylim(1,5) + scale_color_manual(values=c("#245668","#04817E")) 

ggsave("src/STEM/workplace_by_category.png", 
       units = "cm", height = 10, width = 25)