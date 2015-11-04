library(ggplot2)
library(dplyr)

people = c(1,2,3,4)
dat = data.frame()
for (person in people)
{
	pdat = rbind(read.csv(paste('../results/', person, 'results.txt', sep='')),read.csv('../results/2results.txt'))	
	dat = rbind(dat, pdat)
	rm(pdat)
}
# tell R that person and targetPresent are categorical factors
dat$person = as.factor(dat$person)
dat$targetPresent = as.factor(dat$targetPresent)

# remove response = 3 (timed out?)
dat = filter(dat, response!=3)

# display number of incorrect trials - should be low!
print(paste("misses:", 100*nrow(filter(dat, targetPresent==1, response==-1))/nrow(dat)))
print(paste("false pos:", 100*nrow(filter(dat, targetPresent==0, response==1))/nrow(dat)))

# remove incorrect trials from data
dat = rbind(filter(dat, targetPresent==1, response==1), filter(dat, targetPresent==0, response==-1))

# calcualte median RT for each person * condition
mdat = aggregate(data=dat, responseTime~person+block+visualise+trialNumber, FUN='median')


# compute mean of median and 95% CI
aggData = (mdat 
  %>% group_by(block, visualise, trialNumber) 
    %>% summarise(
     meanRT 	= mean(responseTime),
     medianRT 	= median(responseTime), 
     stddev 	= sd(responseTime),
     stderr=stddev/sqrt(length(levels(person))),
     lower=meanRT-1.96*stderr,
     upper=meanRT+1.96*stderr))

# plot! 

plt = ggplot(aggData, aes(x=trialNumber, y=meanRT, ymin=lower, ymax=upper, colour=visualise))
plt = plt + geom_path() + geom_errorbar()
plt = plt + facet_wrap(~block, scales='free_y') + theme_bw()
plt = plt +  scale_colour_brewer(palette="Set1")
ggsave("pilotResults.pdf", height=4, width=8)