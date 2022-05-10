
library(sf)
library(viridis)



zip = "~/Documents/Research/COVID/Rapid_test_results/Paper/map/lfsa000b16a_e.zip"
exdir ="~/Documents/Research/COVID/Rapid_test_results/Paper/map"
unzip(zip, exdir = exdir)

file <- paste0(exdir, "/lfsa000b16a_e")
st_layers(file)
g <- st_read(file, layer = "lfsa000b16a_e")
g.NL= g[g$PRNAME == 'Newfoundland and Labrador / Terre-Neuve-et-Labrador',]

par(mar=c(0,0,0,0)) #margins
#(To see the map)
#plot(g.NL, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )
#plot(g.NL["PRUID"], logz = TRUE, bg="white", lwd=0.25, border=1,  main=NULL)
#plot(g.NL["PRUID"], logz = TRUE,  main=NULL)

# map of Newfoundland
library(ggplot2)


# Configure graphical device
g.ramp <- heat.colors(6)#, start = 0, end = 1)


###CHECK COLORS, SHOULD BE NICE ENOUGH.
## CHECK VALUES, CHECK THAT THEY ARE RIGHT (what about these data that we have changed?)
## 
col.function = function(data){
col = c()
for (i in 1: length(data))
     {if (data[i] == 0)
     {col[i] = 'white'}
     if (data[i] < 4 & data[i] > 0)
     {col[i] = g.ramp[6]}
    if (data[i] <  8 & data[i] >= 4)
     {col[i] = g.ramp[5]}
    if (data[i] < 12 & data[i] >= 8)
     {col[i] = g.ramp[4]}
    if (data[i] < 16 & data[i] >= 12)
    {col[i] = g.ramp[3]}
#    if (data[i] < 20 & data[i] >= 16)
#    {col[i] = g.ramp[2]}
#    if (data[i] >= 20)
#     {col[i] = g.ramp[1]}
    #For 'NA' values (when data are not provided for a certain region)
    if (data[i] < 0)
    {col[i] = 'grey'}
  }
return(col)
}


## Data (from CI_hh_pos)

data.map.hh = df.pc.new$`% Positive Households`
data.map.hh[c(6,9,17,18,20,23,26,28,29)] = 0
# set postal code where less than six reports have occurred to 'no data'

data.map.hh[c(10,13,14,30)]=-99

col.pc = col.function(df.pc$pos.h.pc)
plot(g.NL["PRUID"], logz = TRUE,  main='Percentage of positive households',bg="white",
           col=col.pc,lwd=0.25, border=1)


legend("topright",
       legend = c('0 %',#'>0 to 4%',
                  '>4 to 8%','>8 to 12%','>12 to 16%','no data'), #,'16 to 20%', '>20%','No data'),
       fill = c('white',#g.ramp[6],
                g.ramp[5],g.ramp[4],g.ramp[3],'grey'),#g.ramp[2],g.ramp[1],'grey'),       # Color of the squares
       border = "black") 






### By health regions (EVENTUALLY, DELETE)

url <- "https://www150.statcan.gc.ca/n1/pub/82-402-x/2018001/data-donnees/boundary-limites/arcinfo/HR_010b18a-eng.zip"
zip = "~/Documents/Research/COVID/Rapid_test_results/data/map/HR_010b18a-eng.zip"
download.file(url, destfile = zip)

exdir ="~/Documents/Research/COVID/Rapid_test_results/data/map"
unzip(zip, exdir = exdir)

file <- paste0(exdir, "/HR_010b18a_e")
st_layers(file)

g <- st_read(file, layer = "HR_010b18a_e")
par(mar=c(0,0,0,0))

col.RHA = col.function(df.pos.h$data.pos.h[2:5])


plot(g["HR_UID"], logz = TRUE,  main='Percentage of positive households',bg="white",
     col=col.RHA,lwd=0.25, border=1)


legend("topright",
       legend = c('0 %',#'>0 to 4%',
                  '>4 to 8%','>8 to 12%','>12 to 16%','no data'), #,'16 to 20%', '>20%','No data'),
       fill = c('white',#g.ramp[6],
                g.ramp[5],g.ramp[4],g.ramp[3],'grey'),#g.ramp[2],g.ramp[1],'grey'),       # Color of the squares
       border = "black") 


