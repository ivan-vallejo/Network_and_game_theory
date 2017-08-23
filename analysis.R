
#install.packages("sp")
#install.packages("rgdal")
#install.packages("rgeos")
#install.packages("maptools")
#install.packages("data.table")

library(maptools)
library(data.table)
library(rgdal)
library(rgeos)
library(sp)

setwd("~/BGSE_Classes/SocialNetworks/project/maps/")

#Load the shapefile
shp <- readOGR(path.expand("~/BGSE_Classes/SocialNetworks/project/maps/bnda_dissolv.shp"),"bnda_dissolv",stringsAsFactors = F)
  

#plot it
plot(shp)

#get data from shapefile (dbf)
dbf <- shp@data
str(dbf)

#edit dbf
dades <- data.table(shp@data)
dades[,'Year'] <- 1
colnames(dades) <- c("CTRY","ISO","LTE")

lte <- read.csv("LTE_deployments.csv")

dades[,'LTE'] <- lte$LTE

shp@data <- as.data.frame(dades)
writeOGR(shp,path.expand("~/BGSE_Classes/SocialNetworks/project/maps/world_map.shp"),"world_map",driver="ESRI Shapefile")



# GRAPH
#install.packages("igraph")
library(igraph)

adjm <- matrix(c(0,0,1,0,0,0,1,0,0),nrow=3, byrow=TRUE)
g <- graph_from_adjacency_matrix(adjm,mode="undirected")

el <- c("ARG","CHL",
        "ARG","URY",
        "ARG","PRY",
        "ARG","BOL",
        "ARG","BRA",
        "CHL","PER",
        "CHL","BOL",
        "BOL","PER",
        "BOL","BRA",
        "PRY","BOL",
        "PRY","BRA",
        "URY","BRA",
        "PER","BRA",
        "PER","ECU",
        "PER","COL",
        "BRA","COL",
        "BRA","VEN",
        "BRA","GUY",
        "BRA","GUF",
        "BRA","SUR",
        "GUF","SUR",
        "GUF","FRA",
        "SUR","GUY",
        "GUY","VEN",
        "VEN","COL",
        "COL","PAN",
        "COL","ECU",
        "PAN","CRI",
        "CRI","NIC",
        "NIC","HND",
        "HND","SLV",
        "HND","GTM",
        "SLV","GTM",
        "GTM","BLZ",
        "GTM","MEX",
        "BLZ","MEX",
        "MEX","USA",
        "USA", "CAN",
        "USA", "PRI")


elm <- matrix(el,ncol=2, byrow=TRUE)
g <- graph_from_edgelist(elm, directed = FALSE)

lte <- read.csv("~/BGSE_Classes/SocialNetworks/project/maps/LTE_deployments.csv")

years <- lte[match(V(g)$name,lte$ISO),3]

library(RColorBrewer)
color_pal <- data.frame("colors"=brewer.pal(9,"YlOrRd"),"years"=seq(2017,2009,-1), stringsAsFactors = FALSE)
color_pal[10,] <- c("#FFFFFF",1)

colors <- color_pal$colors[match(years,color_pal$years)]

V(g)$color <- colors  
tkplot(g,vertex.size=25,canvas.height=650,edge.width=2,edge.color='black')

adj <- as.matrix(as_adjacency_matrix(g))
countries <- V(g)$name

  
years[years==1] <- 2020
countries_4g <- countries[years!=2020]
years_4g <- years[years!=2020]
results <- data.frame(row.names=countries_4g,stringsAsFactors = FALSE)
results['abs_count'] <- -1
results['rel_count'] <- -1

for(i in 1:length(countries_4g)){
  tmp <- adj[,countries_4g[i]] * years
  tmp <- tmp[tmp!=0]
  results[i,1] <- sum(tmp<=years_4g[i])
  results[i,2] <- sum(tmp<=years_4g[i])/length(tmp) 
}
results <- results[-13,]

#pdf(file="histogram_americas.pdf")
h <- hist(results$rel_count*100,
      main="", 
    #main="Histogram: LTE licensing in the Americas",
     ylab="# countries (out of 23)",
     xlab="% neighbours having licensed LTE previously/simultaneously",
     col="darkmagenta"
)
#dev.off()

h$counts <- cumsum(h$counts)
#pdf(file="cumulative_histogram_americas.pdf")
plot(h,main="", #"Cumulative histogram: LTE licensing in the Americas",
     ylab="# countries (out of 23)",
     border="gray",
     xlab="% neighbours having licensed LTE previously/simultaneously")
#lines(density(h$counts)) 
par(new = TRUE)
num <- length(results$rel_count)
plot(sort(results$rel_count)*100, 1:num,xlab="",ylab="",
     axes=FALSE,pch=21,bg="gold",cex=1.2)
text(sort(results$rel_count)[1:(num-9)]*100, 1:(num-9),
     labels=countries_4g[-13][order(results$rel_count)][1:(num-9)], cex= 0.7, pos = 4)
text(sort(results$rel_count)[(num-8):num]*100, (num-8):num,
     labels=countries_4g[-13][order(results$rel_count)][(num-8):num], cex= 0.7, pos = 2)
## Plot the cumulative density
d <- density((results$rel_count)*100)
lines(x = d$x, y = cumsum(d$y)/max(cumsum(d$y)) * length(results$rel_count), lwd = 2)
#dev.off()
line_real <- data.frame("x"=d$x, "y"= cumsum(d$y)/max(cumsum(d$y)) * length(results$rel_count))


# WHAT DO WE OBTAIN IF YEARS WERE RANDOM
years1 <- sample(2009:2017,25,replace=T)
results_rand <- data.frame(row.names=countries_4g,stringsAsFactors = FALSE)
results_rand['abs_count'] <- -1
results_rand['rel_count'] <- -1


for(i in 1:length(countries_4g)){
  tmp <- adj[,countries_4g[i]] * years1
  tmp <- tmp[tmp!=0]
  results_rand[i,1] <- sum(tmp<=years1[i])
  results_rand[i,2] <- sum(tmp<=years1[i])/length(tmp) 
}
cum_results <- results_rand[-13,][2]

for(j in 1:100){
  years1 <- sample(2009:2017,25,replace=T)
  results_rand <- data.frame(row.names=countries_4g,stringsAsFactors = FALSE)
  results_rand['abs_count'] <- -1
  results_rand['rel_count'] <- -1
  for(i in 1:length(countries_4g)){
    tmp <- adj[,countries_4g[i]] * years1
    tmp <- tmp[tmp!=0]
    results_rand[i,1] <- sum(tmp<=years1[i])
    results_rand[i,2] <- sum(tmp<=years1[i])/length(tmp) 
  }
  cum_results <- rbind(cum_results,results_rand[-13,][2])
}


h_rand <- hist(cum_results$rel_count*100,
          main="Histogram: LTE licensing in Latin America",
          breaks=c(0,20,40,60,80,100),
          ylab="# countries (out of 23)",
          xlab="% neighbours having licensed LTE previously/simultaneously",
          col="darkmagenta")

h_rand$counts <- cumsum(h_rand$counts)

#pdf(file="random_americas.pdf")
plot(h_rand,
     main="",#Cumulative histogram: random generating model",
     ylab="# countries (23 x 100 random draws)",
     border="gray",
     xlab="% neighbours having licensed LTE previously/simultaneously")
#lines(density(h$counts)) 
par(new = TRUE)
plot(sort(cum_results$rel_count)*100, 1:length(cum_results$rel_count),xlab="",ylab="",
     axes=FALSE,pch='21',col="red",cex=0.2)
## Plot the cumulative density
d <- density((cum_results$rel_count)*100)
lines(x = d$x, y = cumsum(d$y)/max(cumsum(d$y)) * length(cum_results$rel_count), lwd = 2)
#dev.off()

line_rdm <- data.frame("x"=d$x, "y"= cumsum(d$y)/max(cumsum(d$y)) * length(cum_results$rel_count)/100)

line_rdm['group'] <- 'Random'
line_real['group'] <- 'Real'
combined <- rbind(line_rdm,line_real)

library(ggplot2)

ggplot(data=combined, aes(x=x, y=y, group = group, colour = group)) +
  geom_line()+
  ylab("# countries (out of 23)")+
  xlab("% neighbours having licensed LTE previously/simultaneously")+
  theme_classic()+
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)))+
  xlim(0,120)+
  theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))+
  theme(panel.grid.major = element_line(colour="grey80", linetype="dashed",size=0.2),
        panel.grid.major.x = element_line(colour="white", linetype="dashed",size=0))+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=rel(1.5)),legend.direction="vertical",
        legend.position=c(0.85,0.45),
        legend.key.height=unit(3,"line"),legend.key.size=unit(1,"cm"),
        legend.background = element_rect(fill="transparent"))+
  theme(axis.title.y = element_text(size = rel(1.5)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(size = rel(1.7)))+      
  theme(axis.text.y = element_text(size = rel(1.7)))+
  theme(legend.key.width = unit(1.5,"cm"),plot.margin = unit(c(2,2,1,1), "lines"))

#ggsave("comparison_americas.pdf",width=898/72,height=610/72)


# EUROPE

library(igraph)

adjm <- matrix(c(0,0,1,0,0,0,1,0,0),nrow=3, byrow=TRUE)
g <- graph_from_adjacency_matrix(adjm,mode="undirected")

el <- c("PRT","ESP",
        "ESP","AND",
        "ESP","FRA",
        "AND","FRA",
        "FRA","BEL",
        "FRA","ITA",
        "FRA","CHE",
        "FRA","DEU",
        "FRA","LUX",
        "BEL","NLD",
        "BEL","LUX",
        "BEL","DEU",
        "LUX","DEU",
        "CHE","DEU",
        "CHE","ITA",
        "CHE","AUT",
        "NLD","DEU",
        "ITA","AUT",
        "ITA","SVN",
        "DEU","DNK",
        "DEU","AUT",
        "DEU","POL",
        "DEU","CZE",
        "AUT","SVN",
        "AUT","HUN",
        "AUT","CZE",
        "AUT","SVK",
        "GBR","IRL",
        "ISL","NOR",
        "ISL","DNK",
        "ISL","SWE",
        "DNK","SWE",
        "DNK","NOR",
        "SWE","NOR",
        "SWE","FIN",
        "FIN","RUS",
        "FIN","EST",
        "FIN","NOR",
        "NOR","RUS",
        "EST", "RUS",
        "EST", "LVA",
        "LVA", "RUS",
        "LVA", "LTU",
        "LVA", "BLR",
        "LTU", "RUS",
        "LTU", "BLR",
        "LTU", "POL",
        "POL", "BLR",
        "BLR", "RUS",
        "BLR", "UKR",
        "UKR", "RUS",
        "UKR", "POL",
        "UKR", "MDA",
        "UKR", "ROU",
        "UKR", "SVK",
        "UKR", "HUN",
        "POL", "RUS",
        "POL", "CZE",
        "POL", "SVK",
        "MDA", "ROU",
        "CZE", "SVK",
        "SVK", "HUN",
        "ROU", "HUN",
        "ROU", "BGR",
        "ROU", "SRB",
        "HUN", "SVN",
        "HUN", "HRV",
        "HUN", "SRB",
        "SVN", "HRV",
        "HRV", "BIH",
        "HRV", "SRB",
        "HRV", "MNE",
        "BIH", "SRB",
        "BIH", "MNE",
        "MNE", "SRB",
        "MNE", "ALB",
        "SRB", "BGR",
        "SRB", "MKD",
        "ALB", "MKD",
        "ALB", "SRB",
        "ALB", "GRC",
        "MKD", "BGR",
        "BGR", "GRC",
        "BGR", "TUR",
        "GRC", "TUR",
        "GRC", "CYP",
        "MLT", "GBR")

elm <- matrix(el,ncol=2, byrow=TRUE)
g <- graph_from_edgelist(elm, directed = FALSE)

lte <- read.csv("~/BGSE_Classes/SocialNetworks/project/maps/LTE_deployments.csv")

years <- lte[match(V(g)$name,lte$ISO),3]

library(RColorBrewer)
color_pal <- data.frame("colors"=brewer.pal(9,"YlOrRd"),"years"=seq(2017,2009,-1), stringsAsFactors = FALSE)
color_pal[10,] <- c("#FFFFFF",1)

colors <- color_pal$colors[match(years,color_pal$years)]

V(g)$color <- colors  
tkplot(g,vertex.size=25,canvas.height=650,edge.width=2,edge.color='black')

adj <- as.matrix(as_adjacency_matrix(g))
countries <- V(g)$name


years[years==1] <- 2020
countries_4g <- countries[years!=2020]
years_4g <- years[years!=2020]
results <- data.frame(row.names=countries_4g,stringsAsFactors = FALSE)
results['abs_count'] <- -1
results['rel_count'] <- -1

for(i in 1:length(countries_4g)){
  tmp <- adj[,countries_4g[i]] * years
  tmp <- tmp[tmp!=0]
  results[i,1] <- sum(tmp<=years_4g[i])
  results[i,2] <- sum(tmp<=years_4g[i])/length(tmp) 
}

#pdf(file="histogram_europe.pdf")
h <- hist(results$rel_count*100,
          main="", 
          #main="Histogram: LTE licensing in the Americas",
          ylab="# countries (out of 40)",
          xlab="% neighbours having licensed LTE previously/simultaneously",
          col="darkmagenta"
)
#dev.off()

h$counts <- cumsum(h$counts)
#pdf(file="cumulative_histogram_europe.pdf")
plot(h,main="", #"Cumulative histogram: LTE licensing in the Americas",
     ylab="# countries (out of 40)",
     border="gray",
     xlab="% neighbours having licensed LTE previously/simultaneously")
#lines(density(h$counts)) 
par(new = TRUE)
num <- length(results$rel_count)
plot(sort(results$rel_count)*100, 1:num,xlab="",ylab="",
     axes=FALSE,pch=21,bg="gold",cex=1.2)
text(sort(results$rel_count)[1:(num-14)]*100, 1:(num-14),
     labels=countries_4g[order(results$rel_count)][1:(num-14)], cex= 0.7, pos = 4)
text(sort(results$rel_count)[(num-13):num]*100, (num-13):num,
     labels=countries_4g[order(results$rel_count)][(num-13):num], cex= 0.7, pos = 2)
## Plot the cumulative density
d <- density((results$rel_count)*100)
lines(x = d$x, y = cumsum(d$y)/max(cumsum(d$y)) * length(results$rel_count), lwd = 2)
#dev.off()
line_real <- data.frame("x"=d$x, "y"= cumsum(d$y)/max(cumsum(d$y)) * length(results$rel_count))


# WHAT DO WE OBTAIN IF YEARS WERE RANDOM
years1 <- sample(2009:2017,42,replace=T)
results_rand <- data.frame(row.names=countries_4g,stringsAsFactors = FALSE)
results_rand['abs_count'] <- -1
results_rand['rel_count'] <- -1


for(i in 1:length(countries_4g)){
  tmp <- adj[,countries_4g[i]] * years1
  tmp <- tmp[tmp!=0]
  results_rand[i,1] <- sum(tmp<=years1[i])
  results_rand[i,2] <- sum(tmp<=years1[i])/length(tmp) 
}
cum_results <- results_rand[2]

for(j in 1:100){
  years1 <- sample(2009:2017,42,replace=T)
  results_rand <- data.frame(row.names=countries_4g,stringsAsFactors = FALSE)
  results_rand['abs_count'] <- -1
  results_rand['rel_count'] <- -1
  for(i in 1:length(countries_4g)){
    tmp <- adj[,countries_4g[i]] * years1
    tmp <- tmp[tmp!=0]
    results_rand[i,1] <- sum(tmp<=years1[i])
    results_rand[i,2] <- sum(tmp<=years1[i])/length(tmp) 
  }
  cum_results <- rbind(cum_results,results_rand[2])
}


h_rand <- hist(cum_results$rel_count*100,
               main="Histogram: LTE licensing in Latin America",
               breaks=c(0,20,40,60,80,100),
               ylab="# countries (out of 23)",
               xlab="% neighbours having licensed LTE previously/simultaneously",
               col="darkmagenta")

h_rand$counts <- cumsum(h_rand$counts)

#pdf(file="random_europe.pdf")
plot(h_rand,
     main="",#Cumulative histogram: random generating model",
     ylab="# countries (40 x 100 random draws)",
     border="gray",
     xlab="% neighbours having licensed LTE previously/simultaneously")
#lines(density(h$counts)) 
par(new = TRUE)
plot(sort(cum_results$rel_count)*100, 1:length(cum_results$rel_count),xlab="",ylab="",
     axes=FALSE,pch='21',col="red",cex=0.2)
## Plot the cumulative density
d <- density((cum_results$rel_count)*100)
lines(x = d$x, y = cumsum(d$y)/max(cumsum(d$y)) * length(cum_results$rel_count), lwd = 2)
#dev.off()

line_rdm <- data.frame("x"=d$x, "y"= cumsum(d$y)/max(cumsum(d$y)) * length(cum_results$rel_count)/100)

line_rdm['group'] <- 'Random'
line_real['group'] <- 'Real'
combined <- rbind(line_rdm,line_real)

library(ggplot2)

ggplot(data=combined, aes(x=x, y=y, group = group, colour = group)) +
  geom_line()+
  ylab("# countries (out of 40)")+
  xlab("% neighbours having licensed LTE previously/simultaneously")+
  theme_classic()+
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)))+
  xlim(0,120)+
  theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))+
  theme(panel.grid.major = element_line(colour="grey80", linetype="dashed",size=0.2),
        panel.grid.major.x = element_line(colour="white", linetype="dashed",size=0))+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=rel(1.5)),legend.direction="vertical",
        legend.position=c(0.85,0.6),
        legend.key.height=unit(3,"line"),legend.key.size=unit(1,"cm"),
        legend.background = element_rect(fill="transparent"))+
  theme(axis.title.y = element_text(size = rel(1.5)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(size = rel(1.7)))+      
  theme(axis.text.y = element_text(size = rel(1.7)))+
  theme(legend.key.width = unit(1.5,"cm"),plot.margin = unit(c(2,2,1,1), "lines"))

#ggsave("comparison_europe.pdf",width=898/72,height=610/72)


# ASIA & PACIFIC

library(igraph)

adjm <- matrix(c(0,0,1,0,0,0,1,0,0),nrow=3, byrow=TRUE)
g <- graph_from_adjacency_matrix(adjm,mode="undirected")

el <- c("RUS","CHN",
        "RUS","MNG",
        "RUS","PRK",
        "RUS","KAZ",
        "RUS","GEO",
        "RUS","AZE",
        "MNG","CHN",
        "PRK","KOR",
        "KOR","JPN",
        "KAZ","CHN",
        "KAZ","KGZ",
        "KAZ","UZB",
        "KGZ","CHN",
        "KGZ","UZB",
        "KGZ","TJK",
        "UZB","TJK",
        "UZB","TKM",
        "TJK","CHN",
        "TJK","AFG",
        "TKM","AFG",
        "TKM","IRN",
        "AFG","IRN",
        "AFG","PAK",
        "AFG","CHN",
        "PAK","IND",
        "PAK","IRN",
        "IND","CHN",
        "IND","NPL",
        "IND","LKA",
        "IND","BGD",
        "IND","BTN",
        "IND","MMR",
        "NPL","CHN",
        "BGD","MMR",
        "BTN","CHN",
        "MMR","CHN",
        "MMR","LAO",
        "MMR","THA",
        "LAO","CHN",
        "LAO", "THA",
        "LAO", "VNM",
        "LAO", "KHM",
        "KHM", "VNM",
        "KHM", "THA",
        "THA", "MYS",
        "MYS", "SGP",
        "MYS", "IDN",
        "MYS", "PHL",
        "MYS", "BRN",
        "IDN", "PHL",
        "IDN", "PNG",
        "AUS", "NZL",
        "IRN", "AZE",
        "IRN", "ARM",
        "IRN", "IRQ",
        "IRN", "TUR",
        "GEO", "ARM",
        "GEO", "AZE",
        "GEO", "TUR",
        "ARM", "AZE",
        "ARM", "TUR",
        "TUR", "IRQ",
        "TUR", "SYR",
        "SYR", "IRQ",
        "SYR", "LBN",
        "SYR", "JOR",
        "SYR", "ISR",
        "IRQ", "SAU",
        "IRQ", "KWT",
        "IRQ", "JOR",
        "KWT", "SAU",
        "JOR", "SAU",
        "JOR", "ISR",
        "ISR", "LBN",
        "ISR", "EGY",
        "SAU", "BHR",
        "SAU", "ARE",
        "SAU", "OMN",
        "SAU", "YEM",
        "SAU", "QAT",
        "ARE", "OMN",
        "OMN", "YEM",
        "KAZ","TKM",
        "CHN","PRK",
        "CHN","VNM")

elm <- matrix(el,ncol=2, byrow=TRUE)
g <- graph_from_edgelist(elm, directed = FALSE)

lte <- read.csv("~/BGSE_Classes/SocialNetworks/project/maps/LTE_deployments.csv")

years <- lte[match(V(g)$name,lte$ISO),3]

library(RColorBrewer)
color_pal <- data.frame("colors"=brewer.pal(9,"YlOrRd"),"years"=seq(2017,2009,-1), stringsAsFactors = FALSE)
color_pal[10,] <- c("#FFFFFF",1)

colors <- color_pal$colors[match(years,color_pal$years)]

V(g)$color <- colors  
tkplot(g,vertex.size=25,canvas.height=650,edge.width=2,edge.color='black')

adj <- as.matrix(as_adjacency_matrix(g))
countries <- V(g)$name


years[years==1] <- 2020
countries_4g <- countries[years!=2020]
years_4g <- years[years!=2020]
results <- data.frame(row.names=countries_4g,stringsAsFactors = FALSE)
results['abs_count'] <- -1
results['rel_count'] <- -1

for(i in 1:length(countries_4g)){
  tmp <- adj[,countries_4g[i]] * years
  tmp <- tmp[tmp!=0]
  results[i,1] <- sum(tmp<=years_4g[i])
  results[i,2] <- sum(tmp<=years_4g[i])/length(tmp) 
}

#pdf(file="histogram_asia.pdf")
h <- hist(results$rel_count*100,
          main="", 
          #main="Histogram: LTE licensing in the Americas",
          ylab="# countries (out of 45)",
          xlab="% neighbours having licensed LTE previously/simultaneously",
          col="darkmagenta"
)
#dev.off()

h$counts <- cumsum(h$counts)
#pdf(file="cumulative_histogram_asia.pdf")
plot(h,main="", #"Cumulative histogram: LTE licensing in the Americas",
     ylab="# countries (out of 45)",
     border="gray",
     xlab="% neighbours having licensed LTE previously/simultaneously")
#lines(density(h$counts)) 
par(new = TRUE)
num <- length(results$rel_count)
plot(sort(results$rel_count)*100, 1:num,xlab="",ylab="",
     axes=FALSE,pch=21,bg="gold",cex=1.2)
text(sort(results$rel_count)[1:(num-13)]*100, 1:(num-13),
     labels=countries_4g[order(results$rel_count)][1:(num-13)], cex= 0.7, pos = 4)
text(sort(results$rel_count)[(num-12):num]*100, (num-12):num,
     labels=countries_4g[order(results$rel_count)][(num-12):num], cex= 0.7, pos = 2)
## Plot the cumulative density
d <- density((results$rel_count)*100)
lines(x = d$x, y = cumsum(d$y)/max(cumsum(d$y)) * length(results$rel_count), lwd = 2)
#dev.off()
line_real <- data.frame("x"=d$x, "y"= cumsum(d$y)/max(cumsum(d$y)) * length(results$rel_count))


# WHAT DO WE OBTAIN IF YEARS WERE RANDOM
years1 <- sample(2009:2017,49,replace=T)
results_rand <- data.frame(row.names=countries_4g,stringsAsFactors = FALSE)
results_rand['abs_count'] <- -1
results_rand['rel_count'] <- -1


for(i in 1:length(countries_4g)){
  tmp <- adj[,countries_4g[i]] * years1
  tmp <- tmp[tmp!=0]
  results_rand[i,1] <- sum(tmp<=years1[i])
  results_rand[i,2] <- sum(tmp<=years1[i])/length(tmp) 
}
cum_results <- results_rand[2]

for(j in 1:100){
  years1 <- sample(2009:2017,49,replace=T)
  results_rand <- data.frame(row.names=countries_4g,stringsAsFactors = FALSE)
  results_rand['abs_count'] <- -1
  results_rand['rel_count'] <- -1
  for(i in 1:length(countries_4g)){
    tmp <- adj[,countries_4g[i]] * years1
    tmp <- tmp[tmp!=0]
    results_rand[i,1] <- sum(tmp<=years1[i])
    results_rand[i,2] <- sum(tmp<=years1[i])/length(tmp) 
  }
  cum_results <- rbind(cum_results,results_rand[2])
}


h_rand <- hist(cum_results$rel_count*100,
               main="Histogram: LTE licensing in Latin America",
               breaks=c(0,20,40,60,80,100),
               ylab="# countries (out of 45)",
               xlab="% neighbours having licensed LTE previously/simultaneously",
               col="darkmagenta")

h_rand$counts <- cumsum(h_rand$counts)

#pdf(file="random_asia.pdf")
plot(h_rand,
     main="",#Cumulative histogram: random generating model",
     ylab="# countries (45 x 100 random draws)",
     border="gray",
     xlab="% neighbours having licensed LTE previously/simultaneously")
#lines(density(h$counts)) 
par(new = TRUE)
plot(sort(cum_results$rel_count)*100, 1:length(cum_results$rel_count),xlab="",ylab="",
     axes=FALSE,pch='21',col="red",cex=0.2)
## Plot the cumulative density
d <- density((cum_results$rel_count)*100)
lines(x = d$x, y = cumsum(d$y)/max(cumsum(d$y)) * length(cum_results$rel_count), lwd = 2)
#dev.off()

line_rdm <- data.frame("x"=d$x, "y"= cumsum(d$y)/max(cumsum(d$y)) * length(cum_results$rel_count)/100)

line_rdm['group'] <- 'Random'
line_real['group'] <- 'Real'
combined <- rbind(line_rdm,line_real)

library(ggplot2)

ggplot(data=combined, aes(x=x, y=y, group = group, colour = group)) +
  geom_line()+
  ylab("# countries (out of 45)")+
  xlab("% neighbours having licensed LTE previously/simultaneously")+
  theme_classic()+
  theme(axis.title.y=element_text(margin=margin(0,20,0,0)),axis.title.x=element_text(margin=margin(20,0,0,0)))+
  xlim(0,120)+
  theme(axis.line.x = element_line(color="black"),axis.line.y = element_line(color="black"))+
  theme(panel.grid.major = element_line(colour="grey80", linetype="dashed",size=0.2),
        panel.grid.major.x = element_line(colour="white", linetype="dashed",size=0))+
  theme(legend.title=element_blank())+
  theme(legend.text=element_text(size=rel(1.5)),legend.direction="vertical",
        legend.position=c(0.85,0.55),
        legend.key.height=unit(3,"line"),legend.key.size=unit(1,"cm"),
        legend.background = element_rect(fill="transparent"))+
  theme(axis.title.y = element_text(size = rel(1.5)))+
  theme(axis.title.x = element_text(size = rel(1.5)))+
  theme(axis.text.x = element_text(size = rel(1.7)))+      
  theme(axis.text.y = element_text(size = rel(1.7)))+
  theme(legend.key.width = unit(1.5,"cm"),plot.margin = unit(c(2,2,1,1), "lines"))

#ggsave("comparison_asia.pdf",width=898/72,height=610/72)
