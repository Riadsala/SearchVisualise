library(ggplot2)
library(dplyr)
library(lme4)


# setwd("Documents/SearchVisualise/analysis/")

dat = read.csv('../results/data 1-30.txt')


# tell R that person and targetPresent are categorical factors
dat$person = as.factor(dat$person)
dat$targetPresent = as.factor(dat$targetPresent)
levels(dat$visualise) = c("no", "yes")
# levels(dat$block) = c("coloured", "uncoloured")

# remove response = 3 (timed out?)
dat = filter(dat, response!=3)

# display number of incorrect trials - should be low!
print(paste("misses:", 100*nrow(filter(dat, targetPresent==1, response==-1))/nrow(dat)))
print(paste("false pos:", 100*nrow(filter(dat, targetPresent==0, response==1))/nrow(dat)))

# remove incorrect trials from data
dat = rbind(filter(dat, targetPresent==1, response==1), filter(dat, targetPresent==0, response==-1))


m = lmer(responseTime ~ trialNumber + (trialNumber | person), data=filter(dat, visualise=="no"))


dat$trialNumber = as.factor(dat$trialNumber)

# dat = filter(dat, block=='coloured')

# dat$responseTime = log(dat$responseTime)
# compute mean of median and 95% CI
aggData = (dat 
  %>% group_by(person, block, visualise, trialNumber) 
    %>% summarise(
     nTrials = length(responseTime),
     meanRT 	= mean(responseTime),
     medianRT 	= median(responseTime), 
     stddev 	= sd(responseTime),
     stderr=stddev/sqrt(nTrials),
     lower=meanRT-1.96*stderr,
     upper=meanRT+1.96*stderr))

aggData$visualise = factor(aggData$visualise, levels(aggData$visualise)[c(2,1)])

write.csv(format(aggData, ndigit=3), "aggData.txt", row.names=F, quote=F)

# plot! 

plt = ggplot(aggData, aes(x=trialNumber, y=meanRT, ymin=lower, ymax=upper, colour=visualise, group=visualise))
plt = plt + geom_path() + geom_errorbar()
# plt = plt + geom_path(aes(y=medianRT), linetype=2)
plt = plt + facet_wrap(~person, scales='free_y') + theme_bw()
plt = plt +  scale_colour_brewer(palette="Set1")
# plt = plt + scale_y_continuous(limits=c(0.400,0.900),breaks=c(0.580,0.720))
ggsave("pilotResults.pdf", height=8, width=12)


names(aggData)[6] = "responseTimeMean"
names(aggData)[7] = "responseTimeMedian"

aggDat2 = (aggData 
  %>% group_by(block, visualise, trialNumber) 
    %>% summarise(
     nPeople = length(levels(dat$person)),
     meanRT 	= mean(responseTimeMedian),
     stddev 	= sd(responseTimeMedian),
     stderr=stddev/sqrt(nPeople),
     lower=meanRT-1.96*stderr,
     upper=meanRT+1.96*stderr))




plt2 = ggplot(aggDat2, aes(x=trialNumber, y=meanRT, ymin=lower, ymax=upper,colour=visualise, group=visualise))
plt2 = plt2 + geom_path() + geom_errorbar()
plt2 = plt2 + theme_bw() 
plt2 = plt2 +  scale_colour_brewer(palette="Set1")
plt2 = plt2 + scale_y_continuous(name="mean median reaction time (seconds)") + scale_x_discrete(name="trial number in run")
plt2
ggsave("pilotResults2.pdf", height=4, width=5)

#  t-test - is there a practise effect?
trial1 = filter(aggData, visualise=="no", trialNumber==1)$responseTimeMedian
trial3 = filter(aggData, visualise=="no", trialNumber==3)$responseTimeMedian

t.test(trial1, trial3, paired=T, alternative="greater")
