library(dplyr)

d.DAX = read.csv("data-raw/DAX_2015.csv", stringsAsFactors=FALSE)
d.DAX$Date = as.POSIXct(d.DAX$Date)
d.DAX <- d.DAX[order(d.DAX$Date),]
comment(d.DAX) = 'DAX'
save(d.DAX, file = "data/DAX_2015.rdata")

d.Adidas = read.csv("data-raw/DAX_2015_Adidas.csv", stringsAsFactors=FALSE)
d.Adidas <- d.Adidas[d.Adidas$Volume != 0, ]
d.Adidas$Date = as.POSIXct(d.Adidas$Date)
d.Adidas <- d.Adidas[order(d.Adidas$Date),]
comment(d.Adidas) = 'Adidas'
save(d.Adidas, file = "data/DAX_2015_Adidas.rdata")

d.VW = read.csv("data-raw/DAX_2015_VW.csv", stringsAsFactors=FALSE)
d.VW <- d.VW[d.VW$Volume != 0, ]
d.VW$Date = as.POSIXct(d.VW$Date)
d.VW <- d.VW[order(d.VW$Date),]
comment(d.VW) = 'VW'
save(d.VW, file = "data/DAX_2015_VW.rdata")
