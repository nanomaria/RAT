library(readr)

RTdata = read.csv('~/Documents/Research/COVID/Rapid_test_results/Paper/data_all.csv')

# Jan 22 : neg, pos.no, pos for K-6 nd 7-12
#same for Jan 25
# Total : 12 columns


# data subsets considered

# all data
tests.t = RTdata[,3:14]
tot.h.t = dim(RTdata)[1] #total number of households

# consider only a certain health region
tests.EH = RTdata[RTdata$RHA == 'EH',3:14]
tot.h.EH = dim(tests.EH)[1]
tests.CH = RTdata[RTdata$RHA == 'CH',3:14]
tot.h.CH = dim(tests.CH)[1]
tests.WH = RTdata[RTdata$RHA == 'WH',3:14]
tot.h.WH = dim(tests.WH)[1]
tests.LG = RTdata[RTdata$RHA == 'LG',3:14]
tot.h.LG = dim(tests.LG)[1]

# consider only a certain postal codes
# we have a total 30 postal codes
# (postal codes with less than 6 entries have not been considered)

tests.A0A = RTdata[RTdata$PC == 'A0A',3:14]
tot.h.A0A = dim(tests.A0A)[1]
tests.A0B = RTdata[RTdata$PC == 'A0B',3:14]
tot.h.A0B = dim(tests.A0B)[1]
tests.A0E = RTdata[RTdata$PC == 'A0E',3:14]
tot.h.A0E = dim(tests.A0E)[1]
tests.A0G = RTdata[RTdata$PC == 'A0G',3:14]
tot.h.A0G = dim(tests.A0G)[1]
tests.A0K = RTdata[RTdata$PC == 'A0K',3:14]
tot.h.A0K = dim(tests.A0K)[1]

tests.A0L = RTdata[RTdata$PC == 'A0L',3:14]
tot.h.A0L = dim(tests.A0L)[1]
tests.A0M = RTdata[RTdata$PC == 'A0M',3:14]
tot.h.A0M = dim(tests.A0M)[1]
tests.A0N = RTdata[RTdata$PC == 'A0N',3:14]
tot.h.A0N = dim(tests.A0N)[1]
tests.A0P = RTdata[RTdata$PC == 'A0P',3:14]
tot.h.A0P = dim(tests.A0P)[1]
tests.A0R = RTdata[RTdata$PC == 'A0R',3:14]
tot.h.A0R = dim(tests.A0R)[1]
tests.A1A = RTdata[RTdata$PC == 'A1A',3:14]
tot.h.A1A = dim(tests.A1A)[1]

tests.A1B = RTdata[RTdata$PC == 'A1B',3:14]
tot.h.A1B = dim(tests.A1B)[1]
tests.A1C = RTdata[RTdata$PC == 'A1C',3:14]
tot.h.A1C = dim(tests.A1C)[1]
tests.A1E = RTdata[RTdata$PC == 'A1E',3:14]
tot.h.A1E = dim(tests.A1E)[1]
tests.A1G = RTdata[RTdata$PC == 'A1G',3:14]
tot.h.A1G = dim(tests.A1G)[1]
tests.A1H = RTdata[RTdata$PC == 'A1H',3:14]
tot.h.A1H = dim(tests.A1H)[1]

tests.A1K = RTdata[RTdata$PC == 'A1K',3:14]
tot.h.A1K = dim(tests.A1K)[1]
tests.A1L = RTdata[RTdata$PC == 'A1L',3:14]
tot.h.A1L = dim(tests.A1L)[1]
tests.A1M = RTdata[RTdata$PC == 'A1M',3:14]
tot.h.A1M = dim(tests.A1M)[1]
tests.A1N = RTdata[RTdata$PC == 'A1N',3:14]
tot.h.A1N = dim(tests.A1N)[1]
tests.A1S = RTdata[RTdata$PC == 'A1S',3:14]
tot.h.A1S = dim(tests.A1S)[1]

tests.A1V = RTdata[RTdata$PC == 'A1V',3:14]
tot.h.A1V = dim(tests.A1V)[1]
tests.A1W = RTdata[RTdata$PC == 'A1W',3:14]
tot.h.A1W = dim(tests.A1W)[1]
tests.A1X = RTdata[RTdata$PC == 'A1X',3:14]
tot.h.A1X = dim(tests.A1X)[1]
tests.A1Y = RTdata[RTdata$PC == 'A1Y',3:14]
tot.h.A1Y = dim(tests.A1Y)[1]
tests.A2A = RTdata[RTdata$PC == 'A2A',3:14]
tot.h.A2A = dim(tests.A2A)[1]

tests.A2B = RTdata[RTdata$PC == 'A2B',3:14]
tot.h.A2B = dim(tests.A2B)[1]
tests.A2H = RTdata[RTdata$PC == 'A2H',3:14]
tot.h.A2H = dim(tests.A2H)[1]
tests.A2N = RTdata[RTdata$PC == 'A2N',3:14]
tot.h.A2N = dim(tests.A2N)[1]
tests.A2V = RTdata[RTdata$PC == 'A2V',3:14]
tot.h.A2V = dim(tests.A2V)[1]
tests.A5A = RTdata[RTdata$PC == 'A5A',3:14]
tot.h.A5A = dim(tests.A5A)[1]

tests.A8A = RTdata[RTdata$PC == 'A8A',3:14]
tot.h.A8A = dim(tests.A8A)[1]



# Functions:

#prop.pos.hh gives the percent of households testing positives on Jan 22 OR on Jan 25.

prop.pos.hh = function(data,tot.h)
  {
hpos = c()
for (i in 1:dim(data)[1]){
  #select columns that have positive results for Jan 22 or Jan 25
  household = data[i,c(2,3,5,6,8,9,11,12)]  
  # if there is any positives, count 1, otherwise, count zero
  pos = length(household[household>0])
  if (pos > 0){hpos[i] = 1}
  else{hpos[i]=0}
}
# number of of households reporting positive tests
tot.hpos = sum(hpos)
#percentage of households with at least one positive results
prop.hpos = tot.hpos/tot.h
prop.hpos.r = round(prop.hpos, digits = 3)
mylist = c(tot.hpos,prop.hpos.r*100)
return(mylist)
}


#Percentages pos households by health regions

tot.pos.h = prop.pos.hh(tests.t,tot.h.t)[2]
EH.pos.h = prop.pos.hh(tests.EH,tot.h.EH)[2]
CH.pos.h = prop.pos.hh(tests.CH,tot.h.CH)[2]
WH.pos.h = prop.pos.hh(tests.WH,tot.h.WH)[2]
LG.pos.h = prop.pos.hh(tests.LG,tot.h.LG)[2]

ptot.pos.h = prop.pos.hh(tests.t,tot.h.t)[1]
pEH.pos.h = prop.pos.hh(tests.EH,tot.h.EH)[1]
pCH.pos.h = prop.pos.hh(tests.CH,tot.h.CH)[1]
pWH.pos.h = prop.pos.hh(tests.WH,tot.h.WH)[1]
pLG.pos.h = prop.pos.hh(tests.LG,tot.h.LG)[1]

tot.rha = c(tot.h.t,tot.h.EH,tot.h.CH,tot.h.WH,tot.h.LG) #total reports
data.pos.h = c(tot.pos.h,EH.pos.h,CH.pos.h,WH.pos.h,LG.pos.h) #number of reported positive households
data.pos.h.p = c(ptot.pos.h,pEH.pos.h,pCH.pos.h,pWH.pos.h,pLG.pos.h) #%positive households
names.rha = c('Newfoundland and Labrador','Eastern Health', 'Central Health', 'Western Health', 'Labrador-Grenfell Health')
symbol.rha = c('NL','EH','CH','WH','LG')
df.pos.h = data.frame(names.rha,symbol.rha,data.pos.h.p,tot.rha,data.pos.h)

df.pos.h

# Percentage pos. households by postal code

A0A.pos.h = prop.pos.hh(tests.A0A,tot.h.A0A)[2]
A0B.pos.h = prop.pos.hh(tests.A0B,tot.h.A0B)[2]
A0E.pos.h = prop.pos.hh(tests.A0E,tot.h.A0E)[2]
A0G.pos.h = prop.pos.hh(tests.A0G,tot.h.A0G)[2]
A0K.pos.h = prop.pos.hh(tests.A0K,tot.h.A0K)[2]

A0L.pos.h = prop.pos.hh(tests.A0L,tot.h.A0L)[2]
A0M.pos.h = prop.pos.hh(tests.A0M,tot.h.A0M)[2]
A0N.pos.h = prop.pos.hh(tests.A0N,tot.h.A0N)[2]
A0P.pos.h = prop.pos.hh(tests.A0P,tot.h.A0P)[2]

A0R.pos.h = prop.pos.hh(tests.A0R,tot.h.A0R)[2]
A1A.pos.h = prop.pos.hh(tests.A1A,tot.h.A1A)[2]

A1B.pos.h = prop.pos.hh(tests.A1B,tot.h.A1B)[2]
A1C.pos.h = prop.pos.hh(tests.A1C,tot.h.A1C)[2]
A1E.pos.h = prop.pos.hh(tests.A1E,tot.h.A1E)[2]
A1G.pos.h = prop.pos.hh(tests.A1G,tot.h.A1G)[2]
A1H.pos.h = prop.pos.hh(tests.A1H,tot.h.A1H)[2]

A1K.pos.h = prop.pos.hh(tests.A1K,tot.h.A1K)[2]
A1L.pos.h = prop.pos.hh(tests.A1L,tot.h.A1L)[2]
A1M.pos.h = prop.pos.hh(tests.A1M,tot.h.A1M)[2]
A1N.pos.h = prop.pos.hh(tests.A1N,tot.h.A1N)[2]
A1S.pos.h = prop.pos.hh(tests.A1S,tot.h.A1S)[2]

A1V.pos.h = prop.pos.hh(tests.A1V,tot.h.A1V)[2]
A1W.pos.h = prop.pos.hh(tests.A1W,tot.h.A1W)[2]
A1X.pos.h = prop.pos.hh(tests.A1X,tot.h.A1X)[2]
A1Y.pos.h = prop.pos.hh(tests.A1Y,tot.h.A1Y)[2]
A2A.pos.h = prop.pos.hh(tests.A2A,tot.h.A2A)[2]

A2H.pos.h = prop.pos.hh(tests.A2H,tot.h.A2H)[2]
A2N.pos.h = prop.pos.hh(tests.A2N,tot.h.A2N)[2]
A2V.pos.h = prop.pos.hh(tests.A2V,tot.h.A2V)[2]
A5A.pos.h = prop.pos.hh(tests.A5A,tot.h.A5A)[2]
A8A.pos.h = prop.pos.hh(tests.A8A,tot.h.A8A)[2]

## Positives total number

pA0A.pos.h = prop.pos.hh(tests.A0A,tot.h.A0A)[1]
pA0B.pos.h = prop.pos.hh(tests.A0B,tot.h.A0B)[1]
pA0E.pos.h = prop.pos.hh(tests.A0E,tot.h.A0E)[1]
pA0G.pos.h = prop.pos.hh(tests.A0G,tot.h.A0G)[1]
pA0K.pos.h = prop.pos.hh(tests.A0K,tot.h.A0K)[1]

pA0L.pos.h = prop.pos.hh(tests.A0L,tot.h.A0L)[1]
pA0M.pos.h = prop.pos.hh(tests.A0M,tot.h.A0M)[1]
pA0N.pos.h = prop.pos.hh(tests.A0N,tot.h.A0N)[1]
pA0P.pos.h = prop.pos.hh(tests.A0P,tot.h.A0P)[1]

pA0R.pos.h = prop.pos.hh(tests.A0R,tot.h.A0R)[1]
pA1A.pos.h = prop.pos.hh(tests.A1A,tot.h.A1A)[1]

pA1B.pos.h = prop.pos.hh(tests.A1B,tot.h.A1B)[1]
pA1C.pos.h = prop.pos.hh(tests.A1C,tot.h.A1C)[1]
pA1E.pos.h = prop.pos.hh(tests.A1E,tot.h.A1E)[1]
pA1G.pos.h = prop.pos.hh(tests.A1G,tot.h.A1G)[1]
pA1H.pos.h = prop.pos.hh(tests.A1H,tot.h.A1H)[1]

pA1K.pos.h = prop.pos.hh(tests.A1K,tot.h.A1K)[1]
pA1L.pos.h = prop.pos.hh(tests.A1L,tot.h.A1L)[1]
pA1M.pos.h = prop.pos.hh(tests.A1M,tot.h.A1M)[1]
pA1N.pos.h = prop.pos.hh(tests.A1N,tot.h.A1N)[1]
pA1S.pos.h = prop.pos.hh(tests.A1S,tot.h.A1S)[1]

pA1V.pos.h = prop.pos.hh(tests.A1V,tot.h.A1V)[1]
pA1W.pos.h = prop.pos.hh(tests.A1W,tot.h.A1W)[1]
pA1X.pos.h = prop.pos.hh(tests.A1X,tot.h.A1X)[1]
pA1Y.pos.h = prop.pos.hh(tests.A1Y,tot.h.A1Y)[1]
pA2A.pos.h = prop.pos.hh(tests.A2A,tot.h.A2A)[1]

pA2H.pos.h = prop.pos.hh(tests.A2H,tot.h.A2H)[1]
pA2N.pos.h = prop.pos.hh(tests.A2N,tot.h.A2N)[1]
pA2V.pos.h = prop.pos.hh(tests.A2V,tot.h.A2V)[1]
pA5A.pos.h = prop.pos.hh(tests.A5A,tot.h.A5A)[1]
pA8A.pos.h = prop.pos.hh(tests.A8A,tot.h.A8A)[1]


#order as in polygons file (for map)

names.pc = c('St. John`s South','St. John`s Southwest', 'Torbay','Paradise','Portugal Cove-St. Philips',
             'Mount Pearl', 'Goulds','Southeastern Avalon Peninsula (Ferryland)','Western Avalon Peninsula (Argentia)','Bonavista Peninsula (Bonavista)',
             'Burin Peninsula (Marystown)','Northeast Newfoundland (Lewisporte)','Central Newfoundland (Bishops Falls)','Northern Newfoundland (Springdale)','Northwest Newfoundland/Eastern Labrador (Mary`s Harbour)',
             'Western Newfoundland (Lark Harbour)','Southwestern Newfoundland (Channel-Port aux Basques)','Port au Port Peninsula region (St. George`s)','Central Labrador (Happy Valley-Goose Bay)','North/Western Labrador (Churchill Falls)',
             'St. John`s North','St. John`s Northwest Newfoundland & Labrador Provincial Government','St. John`s North Central','St. John`s Central','Gander',
             'Manuels','Conception Bay','Carbonear','Grand Falls','Windsor',
             'Corner Brook','Stephenville','Labrador City','Clarenville','Deer Lake')

labels.pos.pc = c('A1G','A1H','A1K','A1L','A1M',
                  'A1N','A1S','A0A','A0B','A0C',
                  'A0E','A0G','A0H','A0J','A0K',
                  'A0L','A0M','A0N','A0P','A0R',
                  'A1A','A1B','A1C','A1E','A1V',
                  'A1W','A1X','A1Y','A2A','A2B',
                  'A2H','A2N','A2V','A5A','A8A')

# -99 means no data available
tot.pc = c(tot.h.A1G, tot.h.A1H, tot.h.A1K, tot.h.A1L, tot.h.A1M,
           tot.h.A1N, tot.h.A1S, tot.h.A0A, tot.h.A0B, -99,
           tot.h.A0E, tot.h.A0G, -99       , -99       , tot.h.A0K,
           tot.h.A0L, tot.h.A0M, tot.h.A0N, tot.h.A0P, tot.h.A0R,
           tot.h.A1A, tot.h.A1B, tot.h.A1C, tot.h.A1E, tot.h.A1V,
           tot.h.A1W, tot.h.A1X, tot.h.A1Y, tot.h.A2A, -99,
           tot.h.A2H, tot.h.A2N, tot.h.A2V, tot.h.A5A, tot.h.A8A)

pos.h.pc = c(A1G.pos.h, A1H.pos.h, A1K.pos.h, A1L.pos.h, A1M.pos.h,
             A1N.pos.h, A1S.pos.h, A0A.pos.h, A0B.pos.h, -99,
             A0E.pos.h, A0G.pos.h, -99       , -99      , A0K.pos.h,
             A0L.pos.h, A0M.pos.h, A0N.pos.h, A0P.pos.h, A0R.pos.h,
             A1A.pos.h, A1B.pos.h, A1C.pos.h, A1E.pos.h, A1V.pos.h,
             A1W.pos.h, A1X.pos.h, A1Y.pos.h, A2A.pos.h, -99,
             A2H.pos.h, A2N.pos.h, A2V.pos.h, A5A.pos.h, A8A.pos.h)

p.pos.h.pc = c(pA1G.pos.h, pA1H.pos.h, pA1K.pos.h, pA1L.pos.h, pA1M.pos.h,
               pA1N.pos.h, pA1S.pos.h, pA0A.pos.h, pA0B.pos.h, -99,
               pA0E.pos.h, pA0G.pos.h, -99       , -99       , pA0K.pos.h,
               pA0L.pos.h, pA0M.pos.h, pA0N.pos.h, pA0P.pos.h, pA0R.pos.h,
               pA1A.pos.h, pA1B.pos.h, pA1C.pos.h, pA1E.pos.h, pA1V.pos.h,
               pA1W.pos.h, pA1X.pos.h, pA1Y.pos.h, pA2A.pos.h, -99,
               pA2H.pos.h, pA2N.pos.h, pA2V.pos.h, pA5A.pos.h, pA8A.pos.h)


df.pc = data.frame(names.pc,labels.pos.pc, p.pos.h.pc,tot.pc, pos.h.pc)

df.pc