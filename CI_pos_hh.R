sminus <- 1 #probability of testing positive if uninfected 
splus <- 1 #-(1-0.75)*(1-0.75) #probability of testing negative if infected

#x : Observed number of positive results
#n : Total number of tests

fn.fp = function(x,n){
  num = 1-sminus - x/n 
    denom = 1-sminus-splus
  p = num/denom
  return(p)
}

## By health region
df.pos.h.n = df.pos.h

x = df.pos.h.n$data.pos.h.p
#num.pos.hh(tests.t,tot.h.t)
n = df.pos.h.n$tot.rha
#proportion of infected (estimated)
p = fn.fp(x,n)
perc = 100*p

cI.lower = round(100*qbeta(0.025, p*n+.5, n-n*p+.5),1)
cI.upper = round(100*qbeta(0.975, p*n+.5, n-n*p+.5),1)
df.pos.h.new = cbind(df.pos.h.n, perc, cI.lower,cI.upper)
df.pos.h.new$perc = round(df.pos.h.new$perc,1)
df.pos.h.new


names(df.pos.h.new)[names(df.pos.h.new) == "names.rha"] <- "Region"
names(df.pos.h.new)[names(df.pos.h.new) == "symbol.rha"] <- "Abbreviation"
names(df.pos.h.new)[names(df.pos.h.new) == "data.pos.h.p"] <- "Total reported positive households"
names(df.pos.h.new)[names(df.pos.h.new) == "tot.rha"] <- "Total household"
names(df.pos.h.new)[names(df.pos.h.new) == "data.pos.h"] <- "% Raw"
names(df.pos.h.new)[names(df.pos.h.new) == "perc"] <- "% estimated true positive households"
names(df.pos.h.new)[names(df.pos.h.new) == "cI.lower"] <- "Lower bound"
names(df.pos.h.new)[names(df.pos.h.new) == "cI.upper"] <- "Upper bound"



## POSTAL CODE TABLE
df.pc.n = df.pc

# This is just to remove the -99 values and 0 values
df.pc.n$pos.h.pc[df.pc.n$pos.h.pc == -99] <- NA
df.pc.n$tot.pc[df.pc.n$tot.pc == -99] <- NA
df.pc.n$p.pos.h.pc[df.pc.n$p.pos.h.pc == -99] <- NA
df.pc.n$pos.h.pc[df.pc.n$pos.h.pc == 0] <- NA
df.pc.n$tot.pc[df.pc.n$tot.pc == 0] <- NA
df.pc.n$p.pos.h.pc[df.pc.n$p.pos.h.pc == 0] <- NA


x = df.pc.n$p.pos.h.pc
#num.pos.hh(tests.t,tot.h.t)
n = df.pc.n$tot.pc
#proportion of infected (estimated)
p = fn.fp(x,n)
perc = 100*p

cI.lower = round(100*qbeta(0.025, p*n+.5, n-n*p+.5),1)
cI.upper = round(100*qbeta(0.975, p*n+.5, n-n*p+.5),1)
df.pc.new = cbind(df.pc.n, perc, cI.lower,cI.upper)
df.pc.new$perc = round(df.pc.new$perc,1)
df.pc.new

df.pos.h.new

## FINISH TO EXPORT FILE HERE.
# - name
# - postal code
# - total number of tests
# - reported positives
# - estimated percentage positive (+confidence interval in brackets), based on test sensitivity analysis

# speak about 'observed' positive tests in the description above
names(df.pc.new)[names(df.pc.new) == "names.pc"] <- "Region"
names(df.pc.new)[names(df.pc.new) == "labels.pos.pc"] <- "Abbreviation"
names(df.pc.new)[names(df.pc.new) == "p.pos.h.pc"] <- "Total reported positive households"
names(df.pc.new)[names(df.pc.new) == "tot.pc"] <- "Total household"
names(df.pc.new)[names(df.pc.new) == "pos.h.pc"] <- "% Raw"
names(df.pc.new)[names(df.pc.new) == "perc"] <- "% estimated true positive households"
names(df.pc.new)[names(df.pc.new) == "cI.lower"] <- "Lower bound"
names(df.pc.new)[names(df.pc.new) == "cI.upper"] <- "Upper bound"

#data frame outputs

df.pc.new

df.hh = rbind(df.pos.h.new,df.pc.new)


# set postal code where zero positive where reported to zero

df.hh[c(11,14,22,23,25,28,31,33,34),c(3,5,6,7,8)]=0

# set postal code where less than six reports have occurred to 'no data'

df.hh[c(15,18,19,35),c(3,4,5,6,7,8)]=c('no data')


# remove column '% raw' (it is just to check the effect of sensitivity and specificity)
df.hh <- subset(df.hh, select = c(1:4,6:8))

df.hh
write.csv(df.hh, "positive_households_table.csv")


