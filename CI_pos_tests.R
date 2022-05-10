sminus <- 1 #probability of testing positive if uninfected 
splus <- 1#-(1-0.75)*(1-0.75) #probability of testing negative if infected

#x : Observed number of positive results
#n : Total number of tests

fn.fp = function(x,n){
  num = 1-sminus - x/n 
    denom = 1-sminus-splus
  p = num/denom
  return(p)
}


### Reorganize the dataframe from RAT_analysis_posK6712 and RAT_analysis_asympt

# Rows are Tot, K6, 7-12.
# We are making the columns

#tot positive
tot.pos = prop.pos$round.stat[6:8]
#tot tests
tot.t = prop.pos$round.stat[6:8]+prop.pos$round.stat[9:11]
#percent positive
perc.pos = prop.pos$round.stat[1:3]*100
#percent positive out of total
#perc.pos.m = c('--',prop.pos$round.stat[4]*100,prop.pos$round.stat[5]*100)


pos.sympt = data.frame(tot.pos,tot.t,perc.pos)
#pos.asympt = data.frame(tot.asympt,perc.asympt,perc.of.tot)

#put CI on positive cases
x1 = pos.sympt$tot.pos[1]
n1 = pos.sympt$tot.t[1]
#proportion of infected (estimated)
p1 = fn.fp(x1,n1)
perc1 = 100*p1

cI.lower.1 = round(100*qbeta(0.025, p1*n1+.5, n1-n1*p1+.5),1)
cI.upper.1 = round(100*qbeta(0.975, p1*n1+.5, n1-n1*p1+.5),1)

x2 = pos.sympt$tot.pos[2]
n2 = pos.sympt$tot.t[1]
#proportion of infected (estimated)
p2 = fn.fp(x2,n2)
perc2 = 100*p2

cI.lower.2 = round(100*qbeta(0.025, p2*n2+.5, n2-n2*p2+.5),1)
cI.upper.2 = round(100*qbeta(0.975, p2*n2+.5, n2-n2*p2+.5),1)

x3 = pos.sympt$tot.pos[3]
n3 = pos.sympt$tot.t[1]
#proportion of infected (estimated)
p3 = fn.fp(x3,n3)
perc3 = 100*p3

cI.lower.3 = round(100*qbeta(0.025, p3*n3+.5, n3-n3*p3+.5),1)
cI.upper.3 = round(100*qbeta(0.975, p3*n3+.5, n3-n3*p3+.5),1)

perc = c(perc1,perc2,perc3)
cI.lower = c(cI.lower.1,cI.lower.2,cI.lower.3)
cI.upper = c(cI.upper.1,cI.upper.2,cI.upper.3)

pos.sympt.1 = cbind(pos.sympt, perc, cI.lower,cI.upper)
pos.sympt.1$perc = round(pos.sympt.1$perc,1)
#pos.sympt.1$perc.tot = c('--',prop.pos.K6.m,prop.pos.712.m)
pos.sympt.1


names(pos.sympt.1)[names(pos.sympt.1) == "tot.pos"] <- "Total reported positives"
names(pos.sympt.1)[names(pos.sympt.1) == "tot.t"] <- "Total tests"
#names(pos.sympt.1)[names(pos.sympt.1) == "perc.pos.m"] <- "Proportion of positives in each group"
names(pos.sympt.1)[names(pos.sympt.1) == "perc.pos"] <- "Raw percent"
names(pos.sympt.1)[names(pos.sympt.1) == "perc"] <- "% estimated true positives"

names(pos.sympt.1)[names(pos.sympt.1) == "cI.lower"] <- 'Lower bound'
names(pos.sympt.1)[names(pos.sympt.1) == "cI.upper"] <- "Upper bound"

pos.sympt.1


## Add now positive results by health regions

pos.RHA = c(prop.pos.EH[2,2],prop.pos.CH[2,2],prop.pos.WH[2,2],prop.pos.LG[2,2] )
tot.RHA = c(prop.pos.EH[2,2]+prop.pos.EH[3,2],prop.pos.CH[2,2]+prop.pos.CH[3,2],prop.pos.WH[2,2]+prop.pos.WH[3,2],prop.pos.LG[2,2]+prop.pos.LG[3,2] )
perc.RHA = 100*c(prop.pos.EH[1,2],prop.pos.CH[1,2],prop.pos.WH[1,2],prop.pos.LG[1,2])

x = pos.RHA
n = tot.RHA
#proportion of infected (estimated)
p = fn.fp(x,n)
perc = 100*p

cI.lower = round(100*qbeta(0.025, p*n+.5, n-n*p+.5),1)
cI.upper = round(100*qbeta(0.975, p*n+.5, n-n*p+.5),1)

pos.RHA.df = data.frame(pos.RHA, tot.RHA, perc.RHA,perc,cI.lower,cI.upper)
pos.RHA.df$perc = round(pos.RHA.df$perc,1)

names(pos.RHA.df)[names(pos.RHA.df) == "pos.RHA"] <- "Total reported positives"
names(pos.RHA.df)[names(pos.RHA.df) == "tot.RHA"] <- "Total tests"
names(pos.RHA.df)[names(pos.RHA.df) == "perc.RHA"] <- "Raw percent"
names(pos.RHA.df)[names(pos.RHA.df) == "perc"] <- "% estimated true positives"
names(pos.RHA.df)[names(pos.RHA.df) == "cI.lower"] <- 'Lower bound'
names(pos.RHA.df)[names(pos.RHA.df) == "cI.upper"] <- "Upper bound"


## Now add positives by postal code area

pos.pc = c(prop.pos.A1G[2,2],prop.pos.A1H[2,2],prop.pos.A1K[2,2],prop.pos.A1L[2,2],prop.pos.A1M[2,2],
           prop.pos.A1N[2,2],prop.pos.A1S[2,2],prop.pos.A0A[2,2],prop.pos.A0B[2,2],NA,
           prop.pos.A0E[2,2],prop.pos.A0G[2,2],NA              ,NA,prop.pos.A0K[2,2],
           prop.pos.A0L[2,2],prop.pos.A0M[2,2],prop.pos.A0N[2,2],prop.pos.A0P[2,2],prop.pos.A0R[2,2],
           prop.pos.A1A[2,2],prop.pos.A1B[2,2],prop.pos.A1C[2,2],prop.pos.A1E[2,2],prop.pos.A1V[2,2],
           prop.pos.A1W[2,2],prop.pos.A1X[2,2],prop.pos.A1Y[2,2],prop.pos.A2A[2,2],NA,
           prop.pos.A2H[2,2],prop.pos.A2N[2,2],prop.pos.A2V[2,2],prop.pos.A5A[2,2],prop.pos.A8A[2,2])

tot.pc = c(prop.pos.A1G[2,2]+prop.pos.A1G[3,2],prop.pos.A1H[2,2]+prop.pos.A1H[3,2],prop.pos.A1K[2,2]+prop.pos.A1K[3,2],prop.pos.A1L[2,2]+prop.pos.A1L[3,2],prop.pos.A1M[2,2]+prop.pos.A1M[3,2],
           prop.pos.A1N[2,2]+prop.pos.A1N[3,2],prop.pos.A1S[2,2]+prop.pos.A1S[3,2],prop.pos.A0A[2,2]+prop.pos.A0A[3,2],prop.pos.A0B[2,2]+prop.pos.A0B[3,2],NA,
           prop.pos.A0E[2,2]+prop.pos.A0E[3,2],prop.pos.A0G[2,2]+prop.pos.A0G[3,2],NA                                 , NA                                ,prop.pos.A0K[2,2]+prop.pos.A0K[3,2],
           prop.pos.A0L[2,2]+prop.pos.A0L[3,2],prop.pos.A0M[2,2]+prop.pos.A0M[3,2],prop.pos.A0N[2,2]+prop.pos.A0N[3,2],prop.pos.A0P[2,2]+prop.pos.A0P[3,2],prop.pos.A0R[2,2]+prop.pos.A0R[3,2],
           prop.pos.A1A[2,2]+prop.pos.A1A[3,2],prop.pos.A1B[2,2]+prop.pos.A1B[3,2],prop.pos.A1C[2,2]+prop.pos.A1C[3,2],prop.pos.A1E[2,2]+prop.pos.A1E[3,2],prop.pos.A1V[2,2]+prop.pos.A1V[3,2],
           prop.pos.A1W[2,2]+prop.pos.A1W[3,2],prop.pos.A1X[2,2]+prop.pos.A1X[3,2],prop.pos.A1Y[2,2]+prop.pos.A1Y[3,2],prop.pos.A2A[2,2]+prop.pos.A2A[3,2],NA,
           prop.pos.A2H[2,2]+prop.pos.A2H[3,2],prop.pos.A2N[2,2]+prop.pos.A2N[3,2],prop.pos.A2V[2,2]+prop.pos.A2V[3,2],prop.pos.A5A[2,2]+prop.pos.A5A[3,2],prop.pos.A8A[2,2]+prop.pos.A8A[3,2])

perc.pc = 100*c(prop.pos.A1G[1,2],prop.pos.A1H[1,2],prop.pos.A1K[1,2],prop.pos.A1L[1,2], prop.pos.A1M[1,2],
                 prop.pos.A1N[1,2],prop.pos.A1S[1,2],prop.pos.A0A[1,2],prop.pos.A0B[1,2], NA,
                 prop.pos.A0E[1,2],prop.pos.A0G[1,2],NA, NA, prop.pos.A0K[1,2],
                 prop.pos.A0L[1,2],prop.pos.A0M[1,2],prop.pos.A0N[1,2],prop.pos.A0P[1,2], prop.pos.A0R[1,2],
                 prop.pos.A1A[1,2],prop.pos.A1B[1,2],prop.pos.A1C[1,2],prop.pos.A1E[1,2], prop.pos.A1V[1,2],
                 prop.pos.A1W[1,2],prop.pos.A1X[1,2],prop.pos.A1Y[1,2],prop.pos.A2A[1,2], NA,
                 prop.pos.A2H[1,2],prop.pos.A2N[1,2],prop.pos.A2V[1,2],prop.pos.A5A[1,2], prop.pos.A8A[1,2])


#
pos.pc[pos.pc == 0] <- NA
perc.pc[perc.pc == 0] <- NA


x = pos.pc
n = tot.pc
#proportion of infected (estimated)
p = fn.fp(x,n)
perc = 100*p

cI.lower = round(100*qbeta(0.025, p*n+.5, n-n*p+.5),1)
cI.upper = round(100*qbeta(0.975, p*n+.5, n-n*p+.5),1)

pos.pc.df = data.frame(pos.pc, tot.pc, perc.pc,perc,cI.lower,cI.upper)
pos.pc.df$perc = round(pos.pc.df$perc,1)
pos.pc.df

names(pos.pc.df)[names(pos.pc.df) == "pos.pc"] <- "Total reported positives"
names(pos.pc.df)[names(pos.pc.df) == "tot.pc"] <- "Total tests"
names(pos.pc.df)[names(pos.pc.df) == "perc.pc"] <- "Raw percent"
names(pos.pc.df)[names(pos.pc.df) == "perc"] <- "% estimated true positives"
names(pos.pc.df)[names(pos.pc.df) == "cI.lower"] <- 'Lower bound'
names(pos.pc.df)[names(pos.pc.df) == "cI.upper"] <- "Upper bound"


# Put zeros instead of NA where zero results
pos.pc.df[c(6,9,17,18,20,23,26,28,29),c(1,3:6)]=0

# set postal code where less than six reports have occurred to 'no data'

pos.pc.df[c(10,13,14,30),c(1:6)]=c('no data')

# Put no data instead of NA

labels.pos = c('Newfoundland and Labrador','Grades K-6',' Grades 7-12','Eastern Health','Central Health','Western Health','Labrador and Grenfell',
               'St. John`s South','St. John`s Southwest', 'Torbay','Paradise','Portugal Cove-St. Philips',
               'Mount Pearl', 'Goulds','Southeastern Avalon Peninsula (Ferryland)','Western Avalon Peninsula (Argentia)','Bonavista Peninsula (Bonavista)',
               'Burin Peninsula (Marystown)','Northeast Newfoundland (Lewisporte)','Central Newfoundland (Bishops Falls)','Northern Newfoundland (Springdale)','Northwest Newfoundland/Eastern Labrador (Mary`s Harbour)',
               'Western Newfoundland (Lark Harbour)','Southwestern Newfoundland (Channel-Port aux Basques)','Port au Port Peninsula region (St. George`s)','Central Labrador (Happy Valley-Goose Bay)','North/Western Labrador (Churchill Falls)',
               'St. John`s North','St. John`s Northwest Newfoundland & Labrador Provincial Government','St. John`s North Central','St. John`s Central','Gander',
               'Manuels','Conception Bay','Carbonear','Grand Falls','Windsor',
               'Corner Brook','Stephenville','Labrador City','Clarenville','Deer Lake')

symbols.pos = c('NL','K6','7-12','EH','CH','WH','LG',
              'A1G','A1H','A1K','A1L','A1M',
              'A1N','A1S','A0A','A0B','A0C',
              'A0E','A0G','A0H','A0J','A0K',
              'A0L','A0M','A0N','A0P','A0R',
              'A1A','A1B','A1C','A1E','A1V',
              'A1W','A1X','A1Y','A2A','A2B',
              'A2H','A2N','A2V','A5A','A8A')

pos.df.pre = rbind(pos.sympt.1,pos.RHA.df,pos.pc.df)
pos.df = cbind(labels.pos,symbols.pos,pos.df.pre)

names(pos.df)[names(pos.df) == "labels.pos"] <- "Region"
names(pos.df)[names(pos.df) == "symbols.pos"] <- "Abbreviation"

# Eliminate Raw percent, it is just to double check the impact of sensitivity and specificity
pos.df = subset(pos.df, select = c(1:4,6:8))

#Elimination K6 and 712 as we will plot them in another table (table 3)


pos.df = pos.df[-c(2:3),]

write.csv(pos.df, "positive_tests_table.csv")

