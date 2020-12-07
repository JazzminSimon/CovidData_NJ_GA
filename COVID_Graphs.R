#install.packages("readr")
library(readr)
#install.packages("ggplot2")
library(ggplot2)

#Import Data

GAfile = "https://covidtracking.com/api/v1/states/GA/daily.csv"
NJfile = "https://covidtracking.com/api/v1/states/NJ/daily.csv"

GAdata <- read_csv(url(GAfile))
NJdata <- read_csv(url(NJfile))

#change date from double to date type
GAdata$date <- as.character(GAdata$date)
NJdata$date <- as.character(NJdata$date)

GAdata$date <- as.Date(GAdata$date, "%Y%m%d")
NJdata$date <- as.Date(NJdata$date, "%Y%m%d")

options(scipen=999) #takes away decimal point

#Clean Data
NJdata$positive[is.na(NJdata$positive)] <- 0
GAdata$positive[is.na(GAdata$positive)] <- 0
options(scipen=999) #takes away decimal point
###################################
total <- rbind(GAdata, NJdata)

p <- ggplot(total, aes(x=date,y=dif,color=factor(state)))
p <- p + geom_line(size=1.3)
p <- p + labs(color="State")
p <- p + labs(title = "Total Positive COVID-19 Cases")
p <- p + xlab("Date")+ ylab("Positive Cases")
p <- p + scale_color_manual(values=c("red4","blue4"))
ggsave("pos_covid_cases.png", width = 10, height = 5)

####################################
#GA Barg GRaph

GAch <- GAdata[strftime(GAdata$date,"%A") == "Friday",]

GAch$dif <- c(abs(diff(GAch$positive)), GAch$positive[nrow(GAch)])

p <- ggplot(GAch,aes(x=date, y=dif)) +  geom_col(fill="Red4")
p <- p + labs(title = "New COVID-19 Cases in Georgia Weekly")
p <- p + xlab("Date")+ ylab("New Cases (Weekly)")
ggsave("new_cases_bar_GA.png", width = 10, height = 5)


#NJ Bar Graph

NJch <- NJdata[strftime(NJdata$date,"%A") == "Friday",]

NJch$dif <- c(abs(diff(NJch$positive)), NJch$positive[nrow(NJch)])

p <- ggplot(NJch,aes(x=date, y=dif)) +  geom_col(fill="Blue4")
p <- p + labs(title = "New COVID-19 Cases in New Jersey Weekly")
p <- p + xlab("Date")+ ylab("New Cases (Weekly)")
ggsave("new_cases_bar_NJ.png", width = 10, height = 5)

#########################################

GAdata$dif <- c(abs(diff(GAdata$positive)), GAdata$positive[nrow(GAdata)])
NJdata$dif <- c(abs(diff(NJdata$positive)), NJdata$positive[nrow(NJdata)])
total <- rbind(GAdata, NJdata)

p <- ggplot(total, aes(x = date, y=dif, color=factor(state)))
p <- p+geom_smooth(level=0, span=.1, ymin=0, size = 1.3)
p <- p+ scale_color_manual(values=c("red4","blue4"))
p <- p+scale_y_continuous(limits=c(0,4000))
p <- p + labs(title = "New COVID-19 Cases")
p <- p + xlab("Date")+ ylab("New Cases")
p <- p + labs(color="State")

ggsave("new_cases_line_GA_NJ.png", width = 10, height = 5)
##########################################
NJdata$func <- predict(loess(dif~as.numeric(date),NJdata, span = .5), NJdata$date)
GAdata$func <- predict(loess(dif~as.numeric(date),GAdata, span = .5), GAdata$date)

NJdata$date[NJdata$positive == max(NJdata$positive)]

peaks <- data.frame("max"= c(max(NJdata$dif),max(GAdata$dif)))
"max"= c(max(NJdata$dif),max(GAdata$dif)),
"date" = c(NJdata$date[NJdata$dif == max(NJdata$dif)], )

)
