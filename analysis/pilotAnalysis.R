library(ggplot2)
library(dplyr)

people = 11#c(1,2,3,4,5,11)
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
levels(dat$block) = c("coloured", "uncoloured")

# remove response = 3 (timed out?)
dat = filter(dat, response!=3)

# display number of incorrect trials - should be low!
print(paste("misses:", 100*nrow(filter(dat, targetPresent==1, response==-1))/nrow(dat)))
print(paste("false pos:", 100*nrow(filter(dat, targetPresent==0, response==1))/nrow(dat)))

# remove incorrect trials from data
dat = rbind(filter(dat, targetPresent==1, response==1), filter(dat, targetPresent==0, response==-1))

dat$trialNumber = as.factor(dat$trialNumber)


dat = filter(dat, block=='coloured')

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

# plot! 

plt = ggplot(aggData, aes(x=trialNumber, y=meanRT, ymin=lower, ymax=upper, colour=visualise, group=visualise))
plt = plt + geom_path() + geom_errorbar()
plt = plt + geom_path(aes(y=medianRT), linetype=2)
plt = plt + facet_wrap(~person, scales='free_y') + theme_bw()
plt = plt +  scale_colour_brewer(palette="Set1")
ggsave("pilotResults.pdf", height=4, width=8)
plt



# # sample to get CI estimates 
# ii = 0
# for (n in seq(6,72,6))
# {
#     ii = ii + 1
# sdat = dat[sample(x=nrow(dat), size=n),]
# m[ii] = mean(sdat$responseTime)
# stderr[ii] = sd(sdat$responseTime)/sqrt(n)

# }


# plt = ggplot(data.frame(m = m, n=seq(6,72,6), lower=m-1.96*stderr, upper=m+1.96*stderr), aes(x=n, y=m, ymin=lower, ymax=upper))
# plt = plt + geom_errorbar() + geom_point() + scale_y_continuous(limits=c(0.3,0.7))
# plt 
