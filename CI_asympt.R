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


# Find confidence intervals for cases in K6 and in 7-12

pos.sympt = data.frame(tot.pos,tot.t,perc.pos)


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

pos.kj = cbind(pos.sympt,perc,cI.lower,cI.upper)
#pos.kj$perc = round(pos.kj$perc,1)  #not yet because we use it for the calculation


#1st row


# true positives total, min and max
tp.t =0.01*tot.t[1]*c(perc[1],cI.lower[1],cI.upper[1])
tp.t
#true pos k6 and 712
tp.k = 0.01*tot.t[1]*c(perc[2],cI.lower[2],cI.upper[2])
tp.j = 0.01*tot.t[1]*c(perc[3],cI.lower[3],cI.upper[3])

# percent pos + CI (take mean of true positive total.. is that alright?).. Maybe not the best.
perc.k = tp.k/tot.pos[1]*100
perc.j = tp.j/tot.pos[1]*100

perc.k=round(perc.k,1)
perc.j=round(perc.j,1)

kj.ci = rbind(perc.k,perc.j)

labels.kj = c('Grades K-6','Grades 7-12')

kj.pos.t = cbind(labels.kj,pos.kj[2:3,1:2],kj.ci)
colnames(kj.pos.t) <- c('Ages','Reported positives','Total reported','% positives in the grades','Lower bound','Upper bound')

kj.pos.t


## Do something similar for asymptomatic cases

pos.a = df

# Check confidence intervals, so look at total cases first

xa = pos.a$round.stat[2]
na = pos.sympt$tot.t[1]
#proportion of infected (estimated)
pa = fn.fp(xa,na)
perca = 100*pa

cI.lower.a = round(100*qbeta(0.025, pa*na+.5, na-na*pa+.5),1)
cI.upper.a = round(100*qbeta(0.975, pa*na+.5, na-na*pa+.5),1)

perca.t = c(perca,cI.lower.a,cI.upper.a)
true.pos = tot.t[1]*0.01*perca.t
a.pos = round(true.pos/pos.a$round.stat[3]*100,1)


# Do the same for kindergarten

xk = pos.a$round.stat.1[2]
nk = pos.sympt$tot.t[1]
#proportion of infected (estimated)
pk = fn.fp(xk,nk)
perck = 100*pk

cI.lower.k = round(100*qbeta(0.025, pk*nk+.5, nk-nk*pk+.5),1)
cI.upper.k = round(100*qbeta(0.975, pk*nk+.5, nk-nk*pk+.5),1)

perck.t = c(perck,cI.lower.k,cI.upper.k)
perck.t
true.pos.k = tot.t[1]*0.01*perck.t
true.pos.k
a.pos.k = round(true.pos.k/pos.a$round.stat.1[3]*100,1)
a.pos.k


# Now for Junior schools

xj = pos.a$round.stat.2[2]
nj = pos.sympt$tot.t[1]
#proportion of infected (estimated)
pj = fn.fp(xj,nj)
percj = 100*pj

cI.lower.j = round(100*qbeta(0.025, pj*nj+.5, nj-nj*pj+.5),1)
cI.upper.j = round(100*qbeta(0.975, pj*nj+.5, nj-nj*pj+.5),1)

percj.t = c(percj,cI.lower.j,cI.upper.j)
percj.t
true.pos.j = tot.t[1]*0.01*percj.t
true.pos.j
a.pos.j = round(true.pos.j/pos.a$round.stat.2[3]*100,1)
a.pos.j


#asymptomatic cases + CI

a.pos.kj = rbind(a.pos,a.pos.k,a.pos.j)





a.pos.kj
pos.kj

#MAKE A TABLE
#ANALYSIS OF POSITIVE CASES:
  #Table columnns:
  # total in K6
  # Total in 7-12
  # % in K6 + CI
  # % in 7-12 + CI
  # % asympt
  # % asympt in K6 + CI
  # % asympt in 7 - 12 + CI


write.csv(kj.pos.t, "positive_K6712.csv")




## PUT ASYMPTOMATIC!!!
