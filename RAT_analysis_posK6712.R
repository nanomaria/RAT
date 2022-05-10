# Percentage of positives between K6 and 712

library(readr)

RTdata = read.csv('~/Documents/Research/COVID/Rapid_test_results/data/data_all.csv')

# Jan 22 : neg, pos.no, pos for K-6 nd 7-12
#same for Jan 25
# Total : 12 columns

# data subsets considered

# all data
tests.t = RTdata[,3:14]

# Functions

# prop.pos.K6712 gives the percent of positive tests in K-6 with respect to 7-12.
# We define positive cases as positives on Jan 25,
# and positive cases on Jan 22 that have not been reported on Jan 25.
# We define negative cases as negative cases on Jan 25.

prop.pos.K6712 = function(data){
  # consider positive cases on Jan 25
  pos.25.K6 = c()
  pos.25.712 = c()
  neg.25.K6 = c()
  neg.25.712 = c()
  for (i in 1:dim(data)[1]){
    #add up how many asymptomatic and symptomatic positives there is in each household (on Jan 25)
    pos.25.K6[i] = sum(data[i,c(8,9)] )
    pos.25.712[i] = sum(data[i,c(11,12)] )
    neg.25.K6[i] = sum(data[i,7])
    neg.25.712[i] = sum(data[i,10])
  }
  # consider positive cases on Jan 22,
  # that have then not successively been reported on Jan 25
  pos.no.K6 = c()
  pos.no.712 = c()
  pos.K6 = c()
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
  tot.pos.22.K6 = sum(pos.no.K6)+sum(pos.K6)
  tot.pos.22.712 = sum(pos.no.712) + sum(pos.712)
  # add positive cases on Jan 22 (not reported then on Jan 25) and positive cases on Jan 25 
  tot.pos.K6.t = tot.pos.22.K6+sum(pos.25.K6)
  tot.pos.712.t = tot.pos.22.712+sum(pos.25.712)
  tot.neg.K6.t = sum(neg.25.K6)
  tot.neg.712.t = sum(neg.25.712)
  
  tot.pos.t = tot.pos.K6.t+tot.pos.712.t
  tot.neg.t = tot.neg.K6.t+tot.neg.712.t
  
  prop.pos = tot.pos.t/(tot.neg.t+tot.pos.t)
  prop.pos.K6.m = tot.pos.K6.t/(tot.pos.K6.t+tot.pos.712.t)
  prop.pos.712.m = tot.pos.712.t/(tot.pos.K6.t+tot.pos.712.t)
  prop.pos.K6 = tot.pos.K6.t/(tot.neg.t+tot.pos.t)
  prop.pos.712 = tot.pos.712.t/(tot.neg.t+tot.pos.t)
  
  
  
  labels = c('prop positives','prop positives in K6', 'prop positives in 7-12','prop of total in K6','prop of total in 7-12',
             'total positives','total positives in K6','total positives in 7-12',
             'total negatives','total negatives in K6','total negatives in 7-12')
  stat.K6712 = c(prop.pos, prop.pos.K6, prop.pos.712,prop.pos.K6.m,prop.pos.712.m,
                 tot.pos.t,tot.pos.K6.t,tot.pos.712.t,
                 tot.neg.t, tot.neg.K6.t, tot.neg.712.t)
  round.stat = round(stat.K6712,digits=3)
  df.pos.K6712 = data.frame(labels,round.stat)
  return(df.pos.K6712)
}



prop.pos.RHA = function(data){
  # consider positive cases on Jan 25
  pos.25.K6 = c()
  pos.25.712 = c()
  neg.25.K6 = c()
  neg.25.712 = c()
  for (i in 1:dim(data)[1]){
    #add up how many asymptomatic and symptomatic positives there is in each household (on Jan 25)
    pos.25.K6[i] = sum(data[i,c(8,9)] )
    pos.25.712[i] = sum(data[i,c(11,12)] )
    neg.25.K6[i] = sum(data[i,7])
    neg.25.712[i] = sum(data[i,10])
  }
  # consider positive cases on Jan 22,
  # that have then not successively been reported on Jan 25
  pos.no.K6 = c()
  pos.no.712 = c()
  pos.K6 = c()
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
  tot.pos.22.K6 = sum(pos.no.K6)+sum(pos.K6)
  tot.pos.22.712 = sum(pos.no.712) + sum(pos.712)
  # add positive cases on Jan 22 (not reported then on Jan 25) and positive cases on Jan 25 
  tot.pos.K6.t = tot.pos.22.K6+sum(pos.25.K6)
  tot.pos.712.t = tot.pos.22.712+sum(pos.25.712)
  tot.neg.K6.t = sum(neg.25.K6)
  tot.neg.712.t = sum(neg.25.712)
  
  tot.pos.t = tot.pos.K6.t+tot.pos.712.t
  tot.neg.t = tot.neg.K6.t+tot.neg.712.t
  
  prop.pos = tot.pos.t/(tot.neg.t+tot.pos.t)
  prop.pos.K6.m = tot.pos.K6.t/(tot.pos.K6.t+tot.pos.712.t)
  prop.pos.712.m = tot.pos.712.t/(tot.pos.K6.t+tot.pos.712.t)
  prop.pos.K6 = tot.pos.K6.t/(tot.neg.t+tot.pos.t)
  prop.pos.712 = tot.pos.712.t/(tot.neg.t+tot.pos.t)
  
  
  
  labels = c('prop positives','total positives','total negatives')
  stat.K6712 = c(prop.pos,tot.pos.t,tot.neg.t)
  round.stat = round(stat.K6712,digits=3)
  df.pos.K6712 = data.frame(labels,round.stat)
  return(df.pos.K6712)
}


prop.pos = prop.pos.K6712(tests.t)
prop.pos



#CONSIDER ONLY A CERTAIN HEALTH REGION

tests.EH = RTdata[RTdata$RHA == 'EH',3:14]
tot.h.EH = dim(tests.EH)[1]
tests.CH = RTdata[RTdata$RHA == 'CH',3:14]
tot.h.CH = dim(tests.CH)[1]
tests.WH = RTdata[RTdata$RHA == 'WH',3:14]
tot.h.WH = dim(tests.WH)[1]
tests.LG = RTdata[RTdata$RHA == 'LG',3:14]
tot.h.LG = dim(tests.LG)[1]


prop.pos.total = prop.pos.RHA(tests.t)
prop.pos.EH = prop.pos.RHA(tests.EH)
prop.pos.CH = prop.pos.RHA(tests.CH)
prop.pos.WH = prop.pos.RHA(tests.WH)
prop.pos.LG = prop.pos.RHA(tests.LG)


## Consider only a certain postal code area

prop.pos.A1G = prop.pos.RHA(tests.A1G)
prop.pos.A1H = prop.pos.RHA(tests.A1H)
prop.pos.A1K = prop.pos.RHA(tests.A1K)
prop.pos.A1L = prop.pos.RHA(tests.A1L)
prop.pos.A1M = prop.pos.RHA(tests.A1M)

prop.pos.A1N = prop.pos.RHA(tests.A1N)
prop.pos.A1S = prop.pos.RHA(tests.A1S)
prop.pos.A0A = prop.pos.RHA(tests.A0A)
prop.pos.A0B = prop.pos.RHA(tests.A0B)
#A0C

prop.pos.A0E = prop.pos.RHA(tests.A0E)
prop.pos.A0G = prop.pos.RHA(tests.A0G)
#
#
prop.pos.A0K = prop.pos.RHA(tests.A0K)

prop.pos.A0L = prop.pos.RHA(tests.A0L)
prop.pos.A0M = prop.pos.RHA(tests.A0M)
prop.pos.A0N = prop.pos.RHA(tests.A0N)
prop.pos.A0P = prop.pos.RHA(tests.A0P)
prop.pos.A0R = prop.pos.RHA(tests.A0R)

prop.pos.A1A = prop.pos.RHA(tests.A1A)
prop.pos.A1B = prop.pos.RHA(tests.A1B)
prop.pos.A1C = prop.pos.RHA(tests.A1C)
prop.pos.A1E = prop.pos.RHA(tests.A1E)
prop.pos.A1V = prop.pos.RHA(tests.A1V)

prop.pos.A1W = prop.pos.RHA(tests.A1W)
prop.pos.A1X = prop.pos.RHA(tests.A1X)
prop.pos.A1Y = prop.pos.RHA(tests.A1Y)
prop.pos.A2A = prop.pos.RHA(tests.A2A)
#

prop.pos.A2H = prop.pos.RHA(tests.A2H)
prop.pos.A2N = prop.pos.RHA(tests.A2N)
prop.pos.A2V = prop.pos.RHA(tests.A2V)
prop.pos.A5A = prop.pos.RHA(tests.A5A)
prop.pos.A8A = prop.pos.RHA(tests.A8A)

