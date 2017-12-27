library(ggplot2)
library(dplyr)
library(lubridate)

#Load Knowledge data
languages <- read.csv("KnowledgeSum.csv")
work_exp <- read.csv("WorkExperience.csv")

#Fill up info to today's date
maxknow <- cbind(lapply(split(languages, languages$language), function(x) max(x$knowledge)))
update <- data.frame(paste(day(today()),month(today()),year(today()),sep = "-"),rownames(maxknow),maxknow)
names(update) <- names(languages)
#Merge data
languages <- merge(languages, update, all=TRUE)
languages <- arrange(languages, date)

#Plot cummulative knowledge chart
p <- ggplot(languages,aes(x=dmy(date), y=as.numeric(knowledge), fill=language, group=language)) + 
    geom_line(position="stack", colour='darkgrey') + 
    geom_area(alpha=.75) +
    labs(x = "Date", y = "", title = "Knowledge and Experience") + 
    scale_fill_brewer(type="seq", palette= "Paired", name = "Language") +
    scale_x_date(date_breaks = "1 year", date_labels =  "%Y")  +
    theme_classic() +
    
#Plot university / work references
    geom_vline(xintercept = as.numeric(mdy(work_exp$startdate))) +
    geom_text(aes(mdy(work_exp$startdate[[1]]), Inf, label = work_exp$name[[1]], hjust=-0.1, vjust=1.2), color="darkgrey") +
    geom_text(aes(mdy(work_exp$startdate[[2]]), Inf, label = work_exp$name[[2]], hjust=-0.1, vjust=1.2), color="darkgrey") +
    geom_text(aes(mdy(work_exp$startdate[[3]]), Inf, label = work_exp$name[[3]], hjust=-0.1, vjust=1.2), color="darkgrey") +
    geom_text(aes(mdy(work_exp$startdate[[4]]), Inf, label = work_exp$name[[4]], hjust=1.1, vjust=1.2), color="darkgrey") +
    geom_text(aes(mdy(work_exp$startdate[[5]]), Inf, label = work_exp$name[[5]], hjust=-0.1, vjust=1.2), color="darkgrey") +
    
#Knowledge Hunger Line
    geom_segment(aes(x=dmy("01-03-2003"),xend=ymd(today()),y=0,yend=400),size=.75) +
    annotate("text", x=dmy("01-01-2010"), y=210, label="Knowledge Hunger Trend", color="black",angle = 30)

p
