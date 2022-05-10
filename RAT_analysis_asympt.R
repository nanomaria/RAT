library(readr)

RTdata = read.csv('~/Documents/Research/COVID/Rapid_test_results/data/data_all.csv')

# Jan 22 : neg, pos.no, pos for K-6 nd 7-12
#same for Jan 25
# Total : 12 columns

# data subsets considered

# all data
tests.t = RTdata[,3:14]

# Functions

# prop.pos.no gives the percent of asymptomatic and symptomatic cases
# We consider positive cases on Jan 25 and because PH instructed to not repeat the rapid test
# if positive (and as some households have done so),
# we also consider positive tests on Jan 22, that have no been reported again on Jan 22.

#prop.pos.no.K6 and prop.pos.no.712 gives the percent of positives in K-6 with respect to 7-12.
# We define positive cases as in the function above. 


prop.pos.no = function(data){
  # consider positive cases on Jan 25
  tot.pos.no.25 = c()
  tot.pos.25 = c()
  for (i in 1:dim(data)[1]){
    #add up how many asymptomatic and symptomatic positives there is in each household (on Jan 25)
    pos.no.25 = sum(data[i,c(8,11)] )
    pos.25 = sum(data[i,c(9,12)]  )
    tot.pos.no.25[i] = pos.no.25
    tot.pos.25[i] = pos.25
  }
  # consider positive cases on Jan 22,
  # that have then not successively been reported on Jan 25
  pos.no.K6 = c()
  pos.K6 = c()
  pos.no.712 = c()
  pos.712 = c()
  for (i in 1:dim(data)[1]){
    if (data$X22.pos.no.K6[i] > data$X25.pos.no.K6[i] & sum(data[i,1:6]) > sum(data[i,7:12]) )
    {pos.no.K6[i] = data$X22.pos.no.K6[i]-data$X25.pos.no.K6[i]} else
    {pos.no.K6[i] = 0}
    if (data$X22.pos.K6[i] > data$X25.pos.K6[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
    {pos.K6[i] = data$X22.pos.K6[i]-data$X25.pos.K6[i]} else
    {pos.K6[i] = 0}
    if (data$X22.pos.no.712[i] > data$X25.pos.no.712[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
    {pos.no.712[i] = data$X22.pos.no.712[i]-data$X25.pos.no.712[i]} else
    {pos.no.712[i] = 0}
    if (data$X22.pos.712[i] > data$X25.pos.712[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
    {pos.712[i] = data$X22.pos.712[i]-data$X25.pos.712[i]} else
    {pos.712[i] = 0}
  }
  tot.pos.no.22 = sum(pos.no.K6)+sum(pos.no.712)
  tot.pos.22 = sum(pos.K6) + sum(pos.712)
  # add positive cases on Jan 22 (not reported then on Jan 25) and positive cases on Jan 25 
  tot.pos.no.t = tot.pos.no.22+sum(tot.pos.no.25)
  tot.pos.t = tot.pos.22+sum(tot.pos.25)
  prop.no = tot.pos.no.t/(tot.pos.no.t+tot.pos.t)
  
  labels.t = c('prop pos no symptoms','total pos no symptoms','total positives (symptoms+no symptoms)')
  stat.asympt = c(prop.no, tot.pos.no.t,tot.pos.t+tot.pos.no.t)
  round.stat = round(stat.asympt, digits=3)
  df.no.t = data.frame(labels.t,round.stat)
  return(df.no.t)
}


## Function to count asymptomatics between K6
prop.pos.no.K6 = function(data){
  # consider positive cases on Jan 25
  tot.pos.no.25 = c()
  tot.pos.25 = c()
  for (i in 1:dim(data)[1]){
    #add up how many asymptomatic and symptomatic positives there is in each household (on Jan 25)
    pos.no.25 = sum(data[i,8] )
    pos.25 = sum(data[i,9]  )
    tot.pos.no.25[i] = pos.no.25
    tot.pos.25[i] = pos.25
  }
  # consider positive cases on Jan 22,
  # that have then not successively been reported on Jan 25
  pos.no.K6 = c()
  pos.K6 = c()
  for (i in 1:dim(data)[1]){
    if (data$X22.pos.no.K6[i] > data$X25.pos.no.K6[i] & sum(data[i,1:6]) > sum(data[i,7:12]) )
    {pos.no.K6[i] = data$X22.pos.no.K6[i]-data$X25.pos.no.K6[i]} else
    {pos.no.K6[i] = 0}
    if (data$X22.pos.K6[i] > data$X25.pos.K6[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
    {pos.K6[i] = data$X22.pos.K6[i]-data$X25.pos.K6[i]} else
    {pos.K6[i] = 0}
  }
  tot.pos.no.22 = sum(pos.no.K6)
  tot.pos.22 = sum(pos.K6)
  # add positive cases on Jan 22 (not reported then on Jan 25) and positive cases on Jan 25 
  tot.pos.no.t = tot.pos.no.22+sum(tot.pos.no.25)
  tot.pos.t = tot.pos.22+sum(tot.pos.25)
  prop.no = tot.pos.no.t/(tot.pos.no.t+tot.pos.t)
  

  labels.K6 = c('prop pos no symptoms in K6','total pos no symptoms in K6','total positives (symptoms+no symptoms) in K6')
  stat.asympt = c(prop.no, tot.pos.no.t,tot.pos.t+tot.pos.no.t)
  round.stat = round(stat.asympt, digits=3)
  df.no.K6 = data.frame(labels.K6,round.stat)
  return(df.no.K6)
  
}


## percentage of asymptomatic between 7-12

prop.pos.no.712 = function(data){
  # consider positive cases on Jan 25
  tot.pos.no.25 = c()
  tot.pos.25 = c()
  for (i in 1:dim(data)[1]){
    #add up how many asymptomatic and symptomatic positives there is in each household (on Jan 25)
    pos.no.25 = sum(data[i,11] )
    pos.25 = sum(data[i,12]  )
    tot.pos.no.25[i] = pos.no.25
    tot.pos.25[i] = pos.25
  }
  # consider positive cases on Jan 22,
  # that have then not successively been reported on Jan 25
  pos.no.712 = c()
  pos.712 = c()
  for (i in 1:dim(data)[1]){
    if (data$X22.pos.no.712[i] > data$X25.pos.no.712[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
    {pos.no.712[i] = data$X22.pos.no.712[i]-data$X25.pos.no.712[i]} else
    {pos.no.712[i] = 0}
    if (data$X22.pos.712[i] > data$X25.pos.712[i] & sum(data[i,1:6]) > sum(data[i,7:12]))
    {pos.712[i] = data$X22.pos.712[i]-data$X25.pos.712[i]} else
    {pos.712[i] = 0}
  }
  tot.pos.no.22 = sum(pos.no.712)
  tot.pos.22 = sum(pos.712)
  # add positive cases on Jan 22 (not reported then on Jan 25) and positive cases on Jan 25 
  tot.pos.no.t = tot.pos.no.22+sum(tot.pos.no.25)
  tot.pos.t = tot.pos.22+sum(tot.pos.25)
  prop.no = tot.pos.no.t/(tot.pos.no.t+tot.pos.t)
  
  labels.712 = c('prop pos no symptoms in 712','total pos no symptoms in 712','total positives (symptoms+no symptoms) in 712')
  stat.asympt = c(prop.no, tot.pos.no.t,tot.pos.t+tot.pos.no.t)
  round.stat = round(stat.asympt, digits=3)
  df.no.712 = data.frame(labels.712,round.stat)
  return(df.no.712)
  
}


prop.asympt = prop.pos.no(tests.t)
prop.asympt.K6 = prop.pos.no.K6(tests.t)
prop.asympt.712 = prop.pos.no.712(tests.t)

#labels.all.data = c('Total','K6','712')
df = data.frame(prop.asympt,prop.asympt.K6, prop.asympt.712)
df