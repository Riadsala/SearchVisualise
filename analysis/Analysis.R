s=library(ggplot2)
library(dplyr)
library(lme4)
library(car)
library(tidyr)

# setwd("Documents/SearchVisualise/analysis/")

dat = read.csv('../results/data 1-30.txt')


# tell R that person and targetPresent are categorical factors
dat$person = as.factor(dat$person)
levels(dat$person) = as.character(seq(1,30))
dat$targetPresent = as.factor(dat$targetPresent)
levels(dat$visualise) = c("no", "yes")
# levels(dat$block) = c("coloured", "uncoloured")

# remove response = 3 (timed out?)
dat = filter(dat, response!=3)

dat$responseTime = 1000*dat$responseTime

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
     responseTimeMeanLog 	= mean(log(responseTime)),
     responseTimeMean   = mean(responseTime),
     responseTimeMedian = median(responseTime),  
     stddev 	= sd(log(responseTime)),
     stderr=stddev/sqrt(nTrials),
     lower=exp(responseTimeMeanLog-1.96*stderr),
     upper=exp(responseTimeMeanLog+1.96*stderr)))

aggData$responseTimeMeanLog = exp(aggData$responseTimeMeanLog)

aggData$visualise = factor(aggData$visualise, levels(aggData$visualise)[c(2,1)])


# # export some data to send to Ben Vincent
# write.csv(rbind(filter(aggData, trialNumber==1, visualise=="no"),filter(aggData, trialNumber==3, visualise=="yes")), 'newData.txt')
# write.csv(rbind(filter(reinhartDat, trialNumber==1, visualise=="no"),filter(reinhartDat, trialNumber==3, visualise=="yes")), 'oldData.txt')


# plot! 

plt = ggplot(aggData, aes(x=trialNumber, y=responseTimeMeanLog, ymin=lower, ymax=upper, colour=visualise, group=visualise, shape=visualise))
plt = plt + geom_smooth(method=lm, se=F) + geom_errorbar() + geom_point()
plt = plt + facet_wrap(~person, nrow=5) + theme_bw()   #, scales='free_y'
plt = plt +  scale_colour_brewer(palette="Set1")+ scale_x_discrete(name="target repetition")
ggsave("indivResults.pdf", height=12, width=8)



aggDat2 = (aggData 
  %>% group_by(visualise, trialNumber) 
    %>% summarise(
     nPeople = length(levels(dat$person)),
     meanRT 	= mean(responseTimeMean),
     stddev 	= sd(responseTimeMean),
     stderr=stddev/sqrt(nPeople),
     lower=meanRT-stderr,
     upper=meanRT+stderr))


#  load reinhart (2015) data
reinhartDat = read.csv("reinhartData.txt", sep="\t")
reinhartDat = gather(reinhartDat, condition, reactionTime, imagery_1:stim_5.7)

reinhartDat$visualise = "no"
reinhartDat$visualise[1:54] = "yes"
reinhartDat$visualise = as.factor(reinhartDat$visualise)
reinhartDat$visualise=factor(reinhartDat$visualise, levels(reinhartDat$visualise)[c(2,1)])

reinhartDat$trialNumber = 0
reinhartDat$trialNumber[which(reinhartDat$visualise=="yes")] = rep(3:5,each=18)
reinhartDat$trialNumber[which(reinhartDat$visualise=="no")] = rep(1:5,each=18)
reinhartDat$trialNumber = as.factor(reinhartDat$trialNumber)

aggDat3 = (reinhartDat 
  %>% group_by(visualise, trialNumber) 
    %>% summarise(
     nPeople = length(unique(reinhartDat$subject)),
     meanRT     = mean(reactionTime),
     stddev     = sd(reactionTime),
     stderr=stddev/sqrt(nPeople),
     lower=meanRT-stderr,
     upper=meanRT+stderr))

aggDat2$dataset = "new"
aggDat3$dataset = "Reinhart et al (2015)"

allDat = rbind(aggDat2, aggDat3)

plt2 = ggplot(allDat, aes(x=trialNumber, y=meanRT, ymin=lower, ymax=upper,colour=visualise, group=visualise, shape=visualise))
plt2 = plt2 + geom_path() + geom_errorbar()
plt2 = plt2 + theme_bw() + facet_wrap(~dataset)
plt2 = plt2 +  scale_colour_brewer(palette="Set1")
plt2 = plt2 + scale_y_continuous(name="mean reaction time (ms)", breaks=seq(500,700,50), limits=c(500,700)) + scale_x_discrete(name="target repetition")
ggsave("meanResults.pdf", height=4, width=5)


# lme4 - is there a practise effect?
dat$trialNumber = as.numeric(dat$trialNumber)
mP = lmer(log(responseTime)~trialNumber*visualise+(trialNumber*visualise|person), dat)


#  t-test - is there a practise effect?
trial1 = filter(aggData, visualise=="no", trialNumber==1)$responseTimeMean
trial2 = filter(aggData, visualise=="no", trialNumber==2)$responseTimeMean
trial3 = filter(aggData, visualise=="no", trialNumber==3)$responseTimeMean


trial3v = filter(aggData, visualise=="yes", trialNumber==3)$responseTimeMean

t.test(trial1, trial2, paired=T, alternative="greater")
t.test(trial1, trial3, paired=T, alternative="greater")


t.test(trial1, trial3v, paired=T, alternative="greater")
t.test(trial3, trial3v, paired=T, alternative="greater")


t.test(trial1, trial3v, paired=T, alternative="greater")
t.test(trial2, trial3v, paired=T, alternative="greater")


dat$trialNumber = as.numeric(dat$trialNumber)
dat$trialNumber[which(dat$visualise=="yes")] = dat$trialNumber[which(dat$visualise=="yes")]-2 


mean(filter(reinhartDat, visualise=="no", trialNumber==1)$reactionTime-filter(reinhartDat, visualise=="yes", trialNumber==3)$reactionTime)
sd(filter(reinhartDat, visualise=="no", trialNumber==1)$reactionTime-filter(reinhartDat, visualise=="yes", trialNumber==3)$reactionTime)/sqrt(18)

