library(ggplot2)
library(dplyr)
library(lubridate)

#Load Knowledge data
languages <- read.csv("KnowledgeSum.csv")

#Fill up info to today's date
maxknow <- cbind(lapply(split(languages, languages$language), function(x) max(x$knowledge)))
update <- data.frame(paste(day(today()),month(today()),year(today()),sep = "-"),rownames(maxknow),maxknow)
names(update) <- names(languages)
#Merge data
languages <- merge(languages, update, all=TRUE)
languages <- arrange(languages, date)

#Plot cummulative knowledge chart
ggplot(languages,aes(x=dmy(date), y=as.numeric(knowledge), fill=language, group=language)) + 
    geom_line(aes(ymax=as.numeric(knowledge)), position="stack", colour='darkgrey') + 
    geom_area(alpha=.75) +
    labs(x = "Date", y = "", title = "Knowledge and Experience") + 
    scale_fill_brewer(type="seq", palette= "Paired", name = "Language") +
    theme_classic() +
    
#Plot university / work references
    geom_vline(xintercept = as.numeric(dmy("02-05-2011")), color="red") +
    geom_text(aes(dmy("02-05-2011"), Inf, label = "ExxonMobil", hjust=-0.1, vjust=1.2), color="red") +
    #geom_rect(aes(xmin=dmy("02-05-2011"),ymin=-Inf,xmax=dmy("02-05-2015"),ymax=Inf),color="red",alpha=0) +
    geom_vline(xintercept = as.numeric(dmy("01-03-2002")), color="blue") +
    geom_text(aes(dmy("01-03-2002"), Inf, label = "University", hjust=-0.1, vjust=1.2), color="blue") +
    #geom_rect(aes(xmin=dmy("01-03-2002"),ymin=-Inf,xmax=dmy("28-10-2011"),ymax=Inf),color="blue",alpha=0) +
    geom_vline(xintercept = as.numeric(dmy("01-03-2008")), color="green") +
    geom_text(aes(dmy("01-03-2008"), Inf, label = "Huella Soft", hjust=-0.1, vjust=1.2), color="green") +
    #geom_rect(aes(xmin=dmy("01-03-2008"),ymin=-Inf,xmax=dmy("01-12-2008"),ymax=Inf),color="green",alpha=0) +
    #Knowledge Hunger Line
    geom_segment(aes(x=dmy("01-03-2003"),xend=ymd(today()),y=0,yend=350),size=.75) +
    annotate("text", x=dmy("01-03-2008"), y=160, label="Knowledge Hunger Trend", color="black",angle = 45)

