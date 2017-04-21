setwd("~/Dropbox/gh_projects/ukip_media/data")

require(lubridate)
library(plyr)
require(ggplot2)

##########################################################
# 1. Munge weekly poll data from Jennings
##########################################################

weeks1<-read.csv("UKPolls20102015.csv", header=T, stringsAsFactors=FALSE, fileEncoding="latin1", encoding="UTF-8")
weeks1<-subset(weeks1, select=c("vote_ukip", "date3", "date_notes"))

weeks2<-read.csv("UKPolls20152020.csv", header=T, stringsAsFactors=FALSE, fileEncoding="latin1", encoding="UTF-8")
weeks2$date3<-weeks2$edate

weeks2<-subset(weeks2, select=c("vote_ukip", "date3", "date_notes"))

weekdf<-rbind(weeks1, weeks2)

weekdf$date<-mdy(weekdf$date3)

weekdf$year <- year(weekdf$date)
weekdf$week <-week(weekdf$date)
weekdf$month <-month(weekdf$date)
weekdf$day <-day(weekdf$date)
weekdf$shortdate <- paste(weekdf$year, weekdf$week,  sep="-")

week.data<-ddply(weekdf, .(shortdate), summarize,
                    vote_ukip=mean(vote_ukip, na.rm = T),
                    week=mean(week, na.rm = T),
                    month=mean(month, na.rm = T),
                    day=mean(day, na.rm = T),
                    year=mean(year, na.rm = T))

week.data$fulldate <- paste(week.data$year, round(week.data$month,0), round(week.data$day,0), sep="-")

week.data$fulldate <- ymd(week.data$fulldate)

week.data <- week.data[complete.cases(week.data$fulldate),]

week.data <- arrange(week.data, year, month, week)

# qplot(week.data$vote_ukip)


##########################################################
# 1. Munge weekly article counts from Nexis
##########################################################

articles.df<-read.csv("weekly_coverage.csv", header=T)

articles.df$fulldate <- mdy(articles.df$date)

articles.df$year <- year(articles.df$fulldate)
articles.df$week <-week(articles.df$fulldate)
articles.df$month <-month(articles.df$fulldate)
articles.df$day <-day(articles.df$fulldate)
articles.df$shortdate <- paste(articles.df$year, articles.df$week,  sep="-")

# Merge series by nearest year-week

# week.data<- merge(week.data, articles.df, "shortdate")

# Merge series by nearest fulldate
# (Suppport fulldate = center day of the polls within a week)
# (Media fulldate = first day of the week)

require(data.table) # v1.9.6+
setDT(week.data)[, articles := setDT(articles.df)[week.data, articles, on = "fulldate", roll = "nearest"]]

week.data <- week.data[complete.cases(week.data),]

week.data <- merge(week.data, subset(articles.df, select=c("shortdate", "general.elections", "euro.elections", "brexit")), "shortdate")

# weeks go out of order here ^

week.data <- arrange(week.data, year, month, week)


Because the polling data is collected irregularly, the best way to gather, aggregate, and merge weekly time-series for UKIP support and media coverage
is not obvious. Here we summarize how we proceeded. The raw data for UKIP vote intention is aggregated from multiple polls from multiple polling firms, taken at multiple times from 2010 to 2017. We aggregate
these polls by calculating the mean of all polls taken within each week of each year (the week beginning January 1 takes a value of one,
                                                                                    and so on to the final week of the year). This produced
a time-series of UKIP support in which the unit of analysis is the year-week (e.g. 2012-1 reflects the seven days from January 1, 2012 to January 7, 2012.)

We then collected counts of media coverage by week, just as described for the monthly data, beginning in April 24, 2010, a week before
the beginning of our weekly polling series. There are two reasonable ways to merge these series. One is to assign the year-week value
to each of our media weeks. Alternatively, we could merge
each media week to the closest support week by using the full year-month-day for the beginning of each media week and the year-month-day
for the first year-week. Each method produces a different merge, by one time unit.


We then assigned to each observation its corresponding year-week value, as explained above for
the polling data. This gave us a second time-series mergeable with the first.

We also included dummy variables for UK general elections and European Parliament elections, as in the monthly models. The EP elections dummy was given a value of one during the
week of May 17 and the week of May 24, as the election took place across both weeks (May 22-25). All other weeks were assigned a zero. The general elections dummy was given a value
of one for the week of May 1, 2010 for the General Election that took place on May 6, 2010; a value of one for the week of May 2, 2015, for the election that took place on May 7, 2015;
and a zero for all other weeks. Finally, media coverage of UKIP during the week of the "Brexit" referendum is a clear outlier, (2082 articles), so we also include a dummy variable
for the week of Brexit (June 18, 2016).

require(arm)

molten <- subset(week.data, select=c("articles", "vote_ukip", "fulldate"))
molten$articles <- rescale(molten$articles)
molten$vote_ukip <- rescale(molten$vote_ukip)

molten<-melt(molten, id.var="fulldate")

ggplot(molten, aes(x=fulldate, y=value, color=variable)) + geom_line()

# adf.test(diff(week.data$articles))  # stationary
# adf.test(diff(week.data$vote_ukip)) # differenced-stationary

vardf <- subset(week.data, select=c("shortdate", "fulldate", "articles", "vote_ukip", "general.elections", "euro.elections", "brexit"))

vardf$articles <- diff(log(zoo(vardf$articles)), na.pad=T)
vardf$vote_ukip <- diff(log(zoo(vardf$vote_ukip)), na.pad=T)
vardf <- as.data.frame(vardf)
vardf <- vardf[complete.cases(vardf),]

vardf$events <- vardf$general.elections + vardf$euro.elections + vardf$brexit

# VARselect(vardf[c(3,4)], type="both")

varmodel2<-VAR(vardf[c(3,4)], lag.max = 10, ic="AIC", type="both", exogen=vardf[8])
serial.test(varmodel2)  # big p = good
arch.test(varmodel2)    # big p = good
stab<-stability(varmodel2)
plot(stab2)
# summary(varmodel)
# summary(varmodel, equation="UKIP.Articles")
# summary(varmodel, equation="UKIP.Vote")


gc.articles2<-causality(varmodel2, cause="vote_ukip", vcov.=vcovHC(varmodel))
gc.vote2<-causality(varmodel2, cause="articles", vcov.=vcovHC(varmodel))

gc.articles.result2<-as.data.frame(unlist(gc.articles2$Granger))
gc.vote.result2<-as.data.frame(unlist(gc.vote2$Granger))

results2<-merge(gc.vote.result2, gc.articles.result, by="row.names")

results2<-results2[3:6,1:3]
results2$Row.names<-c("P-value", "DF1", "DF2", "F-test")
names(results2)<-c("", "Support", "Articles")
results2$Support<-as.numeric(as.character(results2$Support))
results2$Articles<-as.numeric(as.character(results2$Articles))

stargazer(results2,
          summary=F,
          title="Granger Causality Tests",
          style="APSR",
          digits=3,
          rownames=F,
          header=F)

set.seed(666)

irf.vote2 <- irf(varmodel2, impulse="articles", response="vote_ukip", cumulative=F, boot=T,
                seed=345, n.ahead=5, ortho=T)
plot(irf.vote2, main="Impulse Response of Support from Articles")

# One percent of articles
max.x<-max(week.data$articles)
max.y<-max(week.data$vote_ukip)
one.unit.x<-max.x/100

# Coefficients of regression model
# coeff.l1<-as.vector(varmodel$varresult$UKIP.Vote$coefficients[1])
# coeff.l2<-as.vector(varmodel$varresult$UKIP.Vote$coefficients[3])
# coeff.l3<-as.vector(varmodel$varresult$UKIP.Vote$coefficients[5])
# 
# coeff<-coeff.l1 + coeff.l2 + coeff.l3

coeff<-irf.vote2$irf$articles[1]

# Change in original poll scale due to a 1% increase in articles
effect<-max.y*(coeff/100)

# large.diff.x <- max(diff(unscaled$UKIP.Articles))
large.diff.x<-quantile(diff(week.data$articles), .99)

large.diff.x.percent <- (large.diff.x / max.x)*100

coeff.large<-coeff*large.diff.x.percent

# Change in original poll scale due to a 15% increase in articles
large.effect<-max.y*(coeff.large/100)
```
