## Case study 2
setwd("/Users/zimoz/Downloads")
install.packages("stringi")
library(stringi)
install.packages("stringr")
library(stringr)

## Read 0.5% of the data
dat = read.csv(file ="911_Calls_for_Service.csv" , na.strings = c("","(,)","NA") , as.is = TRUE) 
dim(dat) 
fraction = 0.005
set.seed(5260) 
sample_rows = sample(1:nrow(dat), floor(nrow(dat) * fraction))
dat_sample = dat[sample_rows,]
dat = dat_sample 
dat_sample = NULL 
gc() 


## Data cleaning
dat$priority <- as.factor(dat$priority)
dat$district <-as.factor(dat$district)
dat$incidentLocation <-NULL
dat$callNumber <-NULL
dat$location <- gsub("\\(|\\)", "", dat$location)
dat$priority[which(dat$priority=="Out of Service")]  <- NA


# Dealing with Coordinate 

install.packages("reshape2")
library(reshape2)

cordt <- colsplit(dat$location, "\\,",names = c("lat","log"))
dat$coordlat <- cordt$lat
dat$coordlog <- cordt$log
dat$coordlat[which(dat$coordlat < 39)] = NA
dat$coordlat[which(dat$coordlat > 40)] = NA
dat$coordlog[which(dat$coordlog > -76)] = NA
dat$coordlog[which(dat$coordlog < -77)] = NA

dat$location <- NULL



## Dealing with missing value
install.packages("mice")
library(mice)
imp <- mice(data = dat ,m =1 ,seed = 12345 , method = c("ployreg","polr","polyreg","polyreg","pmm","pmm"))
dat.imp <- complete(imp , action = 1)



# Dealing with time data
install.packages("lubridate")
library(lubridate)
dat.imp$times <- str_extract(dat.imp$callDateTime ,  "[0-9]+\\:[0-9]+\\:[0-9]+ +[A-Z]+")
dat.imp$date <- str_extract(dat.imp$callDateTime , "[0-9]+\\/[0-9]+\\/[0-9]+")
dat.imp$date <- as.Date(dat.imp$date,"%m/%d/%Y")
dat.imp$time24 <- as.POSIXct(dat.imp$times, format='%I:%M:%S %p')
dat.imp$times <- substr(dat.imp$time24,12,19)
dat.imp$times <- substr(dat.imp$times,1,2)
dat.imp$year <- as.factor(year(as.Date(dat.imp$date,"%m/%d/%Y")))
dat.imp$month <- as.factor(month(as.Date(dat.imp$date,"%m/%d/%Y")))
dat.imp$weekday <- as.factor(weekdays(as.Date(dat.imp$date,"%m/%d/%Y")))
dat.imp$count <- 1

dat.imp$date <- NULL
dat.imp$time24 <- NULL
dat.imp$callDateTime <- NULL
row.names(dat.imp) <-c(1:nrow(dat.imp))





## Making sub data 



# Handling with mistype description

dat.imp$description <- tolower(dat.imp$description)
dat.imp$description[which(adist(dat.imp$description,"transpassing") <= 5)] <- "transpassing"
dat.imp$description[which(adist(dat.imp$description,"911/no voice") <= 2)] = "911/no voice"
dat.imp$description[which(adist(dat.imp$description,"wellbeing check") <= 7|str_detect(dat.imp$description,"wellbei"))] = "wellbeing check"
dat.imp$description[which(str_detect(dat.imp$description , "wellbeing|well ck|well being|well bein|ck well|chk  well  being|chk well bwing"))] = "wellbeing check"
dat.imp$description[which(str_detect(dat.imp$description , "unknown|unknoiwn"))] = "unkonwn trouble"
dat.imp$description[which(adist(dat.imp$description,"0") <= 3)] = "other"
dat.imp$description[which(str_detect(dat.imp$description , "larceny|larcency"))] = "larceny"
dat.imp$description[which(str_detect(dat.imp$description , "protection|protective|protect|prot|prtctive"))] = "protection order"
dat.imp$description[which(str_detect(dat.imp$description , "assist|asssit|escort"))] = "assist request"
dat.imp$description[which(str_detect(dat.imp$description , "invest|inves|inv unk|invstigate"))] = "investigate problem"
dat.imp$description[which(str_detect(dat.imp$description , "peace"))] = "peace order"
dat.imp$description[which(str_detect(dat.imp$description , "fire"))] = "fire hazard"
dat.imp$description[which(str_detect(dat.imp$description , "unauthorize|unauthorized|unauth"))] = "unauthorized use"
dat.imp$description[which(str_detect(dat.imp$description , "run|run-|runaway"))] = "runaway"
dat.imp$description[which(str_detect(dat.imp$description , "recover|recovered"))] = "recover property"
dat.imp$description[which(str_detect(dat.imp$description , "warr|warrant|warrants"))] = "warrant service"
dat.imp$description[which(str_detect(dat.imp$description , "supervisor|supv"))] = "complaint"
dat.imp$description[which(str_detect(dat.imp$description , "threat"))] = "threat"
dat.imp$description[which(str_detect(dat.imp$description , "traffic|transport|obstruct"))] = "traffic problem"
dat.imp$description[which(str_detect(dat.imp$description , "open|parking|locked|lock|dumping|dump|belonings|belongings|belongsing|check"))] = "trifle"
dat.imp$description[which(str_detect(dat.imp$description , "vehicle|veh|tow"))] = "vehicle problem"
dat.imp$description[which(str_detect(dat.imp$description , "^\\*|^\\&"))] = "other"
dat.imp$description[which(str_detect(dat.imp$description , "shooting|stabbing|gun|attempt|attempted|body|death|cutting|crisis|armed|nonbreathing|knife|destruct|assault|nonbreathig"))] = "violent crime"
dat.imp$description[which(adist(dat.imp$description,"female screaming") <= 2)] = "other"
dat.imp$description[which( str_detect(dat.imp$description,"complaint|loud"))] = "complaint"
dat.imp$description[which(adist(dat.imp$description,"nitify") <= 2)] = "other"
dat.imp$description[which(adist(dat.imp$description,"follow up") <= 2 | str_detect(dat.imp$description,"follow up*"))] = "follow up"
dat.imp$description[which(adist(dat.imp$description,"narcotics") <= 7)] = "narcotics abuse"
dat.imp$description[which(adist(dat.imp$description,"emergency petit") <= 7 | str_detect(dat.imp$description,"ep|papers"))] = "other"
dat.imp$description[which(str_detect(dat.imp$description,"driver|driving"))] = "problem driver"
dat.imp$description[which(str_detect(dat.imp$description,"auto acc|accident"))] = "accident"
dat.imp$description[which(str_detect(dat.imp$description,"lying in street|suspicious pers|intoxicated"))] = "problem person"
dat.imp$description[which(str_detect(dat.imp$description,"miss|children$|lost child"))] = "missing person"
dat.imp$description[which(str_detect(dat.imp$description,"disturbance")| adist(dat.imp$description,"harassment") <=2 )] = "harrssment"
dat.imp$description[which(str_detect(dat.imp$description,"animal|dog"))] = "animal issue"
dat.imp$description[which(str_detect(dat.imp$description,"patrol"))] = "patrol"
dat.imp$description[which(str_detect(dat.imp$description,"bail"))] = "bailout"
dat.imp$description[which(str_detect(dat.imp$description,"close"))] = "closed call"
dat.imp$description[which(str_detect(dat.imp$description,"change mind|info|wrong|curfew|clsd|lunch|. co.|att|record|lab|mistake|hangup|79|possible|eval|child on phone|ck location"))] = "other"
dat.imp$description[which(str_detect(dat.imp$description,"disturb"))] = "disturb issue"
dat.imp$description[which(adist(dat.imp$description,"exparte")<=2)] = "exparte"
dat.imp$description[which(str_detect(dat.imp$description,"forced entr"))] = "forced entry"
dat.imp$description[which(adist(dat.imp$description,"forced entry") <= 6)] <- "forced entry" 
dat.imp$description[which(str_detect(dat.imp$description,"summns"))] = "other"
dat.imp$description[which(str_detect(dat.imp$description,"phone"))] = "phone problem"
dat.imp$description[which(str_detect(dat.imp$description,"no help"))] = "other"
dat.imp$description[which(str_detect(dat.imp$description,"burglary|robbery"))] = "burglary/robbery"
dat.imp$description[which(str_detect(dat.imp$description,"auto theft|car jacking"))] = "auto theft"
dat.imp$description[which(adist(dat.imp$description,"posititution")<=7)] = "posititution"
dat.imp$description[which(str_detect(dat.imp$description,"dispute"))] = "dispute issue"
dat.imp$description[which(adist(dat.imp$description,"disorderly")<=5)] = "disorder"
dat.imp$description[which(adist(dat.imp$description,"silent alarm")<=5)] = "silent/holdup alarm"

dat.imp$description[which(adist(dat.imp$description,"field interview")<=6)] = "field interview"

dat.imp$description[which(adist(dat.imp$description,"unfounded")<=5)] = "missing property"
dat.imp$description[which(adist(dat.imp$description,"lost property")<=6)] = "missing property"


dat.imp$description[which(adist(dat.imp$description,"mental case")<=6)] = "mental/sick case"

dat.imp$description[which(adist(dat.imp$description,"see text")<=5)] = "see text"

dat.imp$description[which(adist(dat.imp$description,"personal relief")<=10)] = "other"









install.packages("Hmisc")
library(Hmisc)



crimelist = c("transpassing", "911/no voice", 
             "wellbeing check", "unkonwn trouble", "other","larceny", "protection order","assist request","investigate problem","peace order",
             "fire hazard", "unauthorized use","runaway","recover property","warrant service", "complaint","threat","traffic problem","trifle","vehicle problem","violent crime","follow up","narcotics abuse","problem driver",
             "accident","problem person","missing person","harrssment","animal issue","patrol","bailout","closed call","disturb issue","exparte","forced entry","phone problem","burglary/robbery",
             "auto theft","posititution","dispute issue","disorder","silent/holdup alarm","field interview","missing property","mental/sick case","see text","false pretense")

dat.imp$description[(which(dat.imp$description %nin% crimelist))] = "other"


# Subset data with priority
primid <- dat.imp[which(dat.imp$priority == "Medium"),]
prilow <-dat.imp[which(dat.imp$priority == "Low"),]
prihigh <- dat.imp[which(dat.imp$priority == "High"),]
priNE <- dat.imp[which(dat.imp$priority == "Non-Emergency"),]
priE <-dat.imp[which(dat.imp$priority == "Emergency"),]


# Subset data with certain types of crime
larceny <- subset(dat.imp,dat.imp$description =="larceny")
burglary <- subset(dat.imp,dat.imp$description == "burglary/robbery")
violent <- subset(dat.imp,dat.imp$description == "violent crime")
accident <- subset(dat.imp,dat.imp$description == "accident")
naabuse <- subset(dat.imp,dat.imp$description =="narcotics abuse")
fire <- subset(dat.imp,dat.imp$description =="fire hazard")
novoice <- subset(dat.imp,dat.imp$description =="911/no voice")
harrssment <- subset(dat.imp,dat.imp$description == "harrssment")
mp <- subset(dat.imp,dat$description == "missing person")
newdata <- rbind(larceny,burglary,violent,accident,naabuse,fire,novoice,harrssment,mp)


## Visualize Data
install.packages("ggmap")
install.packages("ggplot2")
library(ggmap)
library(ggplot2)

BaltimoreMap1 <- get_map("Baltimore",zoom = 12)



ggmap(BaltimoreMap1)+geom_point(aes(x = coordlog, y =coordlat , color = district , group = district  ),data = violent)+ggtitle("Violent Crime Contour Plot")+
geom_density_2d(data = violent ,aes(x=coordlog,y=coordlat)) +
stat_density2d(data = violent, aes(x = coordlog, y = coordlat, fill = ..level.., alpha = ..level..),size = 0.01, bins = 16, geom = 'polygon') +
scale_fill_gradient(low = "green", high = "red") +scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))



ggmap(BaltimoreMap1)+geom_point(aes(x = coordlog, y =coordlat , color = district , group = district  ),data = accident)+
  geom_density_2d(data = accident ,aes(x=coordlog,y=coordlat)) +
  stat_density2d(data = accident, aes(x = coordlog, y = coordlat, fill = ..level.., alpha = ..level..),size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))

ggmap(BaltimoreMap1)+geom_point(aes(x = coordlog, y =coordlat , color = district , group = district  ),data = burglary)+
  geom_density_2d(data = burglary ,aes(x=coordlog,y=coordlat)) +
  stat_density2d(data = burglary, aes(x = coordlog, y = coordlat, fill = ..level.., alpha = ..level..),size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))

ggmap(BaltimoreMap1)+geom_point(aes(x = coordlog, y =coordlat , color = district , group = district  ),data = harrssment)+
  geom_density_2d(data = harrssment ,aes(x=coordlog,y=coordlat)) +
  stat_density2d(data = harrssment, aes(x = coordlog, y = coordlat, fill = ..level.., alpha = ..level..),size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))

ggmap(BaltimoreMap1)+geom_point(aes(x = coordlog, y =coordlat , color = district , group = district  ),data = fire)+
  geom_density_2d(data = fire ,aes(x=coordlog,y=coordlat)) +
  stat_density2d(data = fire, aes(x = coordlog, y = coordlat, fill = ..level.., alpha = ..level..),size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))

ggmap(BaltimoreMap1)+geom_point(aes(x = coordlog, y =coordlat , color = district , group = district  ),data = larceny)+
  geom_density_2d(data = larceny ,aes(x=coordlog,y=coordlat)) +
  stat_density2d(data = larceny, aes(x = coordlog, y = coordlat, fill = ..level.., alpha = ..level..),size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))

ggmap(BaltimoreMap1)+geom_point(aes(x = coordlog, y =coordlat , color = district , group = district  ),data = naabuse) +
  geom_density_2d(data = naabuse ,aes(x=coordlog,y=coordlat)) +
  stat_density2d(data = naabuse, aes(x = coordlog, y = coordlat, fill = ..level.., alpha = ..level..),size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))

ggmap(BaltimoreMap1)+geom_point(aes(x = coordlog, y =coordlat , color = district , group = district  ),data = novoice)+
  geom_density_2d(data = novoice ,aes(x=coordlog,y=coordlat)) +
  stat_density2d(data = novoice, aes(x = coordlog, y = coordlat, fill = ..level.., alpha = ..level..),size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))


ggmap(BaltimoreMap1)+geom_point(aes(x = coordlog, y =coordlat , color = district , group = district  ),data = newdata)+  geom_density_2d(data = newdata ,aes(x=coordlog,y=coordlat)) +
  stat_density2d(data = newdata, aes(x = coordlog, y = coordlat, fill = ..level.., alpha = ..level..),size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "red") +scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12)) + facet_wrap(~description,ncol = 3)

  



# bar plot of priority
barplot(prop.table(table(dat$priority)))







# bar plot of district
barplot(prop.table(table(dat$district)),main = "Proportion in each district")






# plot violent crime of each district on map
install.packages("dismo")
install.packages("raster")
install.packages("rgdal")
library(dismo)
library(raster)
library(rgdal)


Neighborhoods <- readOGR(".","nhood_2010")
Neighborhoods <- spTransform(Neighborhoods, CRS("+proj=longlat +datum=WGS84"))
Neighborhoods <- fortify(Neighborhoods)

city <- qmap("Baltimore", zoom = 12, source="stamen", maptype="toner",darken = c(.3,"#BBBBBB"))
city2 <- qmap("Baltimore", zoom = 13, source="stamen", maptype="toner",darken = c(.3,"#BBBBBB"),legend ="topleft")
city3 <- qmap("Baltimore", zoom = 14, source="stamen", maptype="toner",darken = c(.3,"#BBBBBB"),legend ="topleft")



city+ 
geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.3,color='white',alpha = "0.8", data=Neighborhoods, alpha=0)+
geom_point(aes(x = coordlog, y =coordlat , color = district , group = district  ),alpha = "1",data = violent)








# plot density on map

city+geom_point(data = newdata,aes(x = coordlog,y = coordlat),color="red",alpha = "0.3")+ facet_wrap(~description , ncol = 3)

city+geom_point(data = violent,aes(x = coordlog,y = coordlat),color="red",alpha = "0.3")+ facet_wrap(~times , ncol = 6)



city +
  geom_point(data=violent, aes(x=coordlog, y=coordlat),color = "red",alpha = "0.3")+ggtitle("Violent Crime")+geom_density_2d(data = violent ,aes(x=coordlog,y=coordlat)) +
  stat_density2d(data = violent, aes(x = coordlog, y = coordlat, fill = ..level.., alpha = ..level..),size = 0.01, bins = 16, geom = 'polygon') +scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))


city +
  geom_point(data=harrssment, aes(x=coordlog, y=coordlat),color = "green",alpha = "0.5")+ggtitle("Harrssment")

city +
  geom_point(data=naabuse, aes(x=coordlog, y=coordlat),color = "blue",alpha = "0.3")+ggtitle("Narcotics Abuse")

city3 +
  geom_point(data=newdata, aes(x=coordlog, y=coordlat , color = description , size = description),alpha = "0.3" ,alpha = 1/2)


city2 +
  stat_density2d(
    aes(x = coordlog, y = coordlat, fill = ..level..,
        alpha = ..level..),
    size = 2, bins = 4, data =newdata,
    geom = "polygon") + facet_wrap(~description,ncol = 3)



city +
  geom_point(data=violent, aes(x=coordlog, y=coordlat),color = "red",alpha = "0.3")+geom_density_2d(data = violent ,aes(x=coordlog,y=coordlat)) +
  stat_density2d(data = violent, aes(x = coordlog, y = coordlat, fill = ..level.., alpha = ..level..),size = 0.01, bins = 16, geom = 'polygon') +
  scale_fill_gradient(low = "green", high = "blue") +scale_alpha(range = c(0.00, 0.25), guide = FALSE) +
  theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12))

#
city +
  stat_density2d(data=violent, aes(x=coordlog, y=coordlat,color=..density..,size=ifelse(..density..<=1,0,..density..),alpha=..density..),geom="tile",contour=F) +
  scale_color_continuous(low="orange", high="red", guide = "none") +
  scale_size_continuous(range = c(0, 3), guide = "none") +
  scale_alpha(range = c(0,.5), guide="none") +
  ggtitle("Baltimore Violent Crime") +
  theme(plot.title = element_text(family="Trebuchet MS", size=36, face="bold", hjust=0, color="#777777")) 




######################################################
#violent crime
ggmap(BaltimoreMap1, extent = "panel", maprange=FALSE) +geom_point(data = violent,aes(x=coordlog,y=coordlat))+geom_density2d(data = violent, aes(x = coordlog, y = coordlat)) +
  stat_density2d(data = violent, aes(x = coordlog, y = coordlat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon')+
  scale_fill_gradient(low = "green", high = "red")+
  scale_alpha(range = c(0.00, 0.25), guide = FALSE)

#harrssment
ggmap(BaltimoreMap1, extent = "panel", maprange=FALSE)+geom_point(data = harrssment,aes(x=coordlog,y=coordlat)) +geom_density2d(data = harrssment, aes(x = coordlog, y = coordlat)) +
  stat_density2d(data = harrssment, aes(x = coordlog, y = coordlat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon')+
  scale_fill_gradient(low = "green", high = "red")+
  scale_alpha(range = c(0.00, 0.25), guide = FALSE)

#narcotics abuse
ggmap(BaltimoreMap1, extent = "panel", maprange=FALSE)+geom_point(data = naabuse,aes(x=coordlog,y=coordlat)) +geom_density2d(data = naabuse, aes(x = coordlog, y = coordlat)) +
  stat_density2d(data = naabuse, aes(x = coordlog, y = coordlat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon')+
  scale_fill_gradient(low = "green", high = "red")+
  scale_alpha(range = c(0.00, 0.25), guide = FALSE)

#novoice
ggmap(BaltimoreMap1, extent = "panel", maprange=FALSE)+geom_point(data = novoice,aes(x=coordlog,y=coordlat)) +geom_density2d(data = novoice, aes(x = coordlog, y = coordlat)) +
  stat_density2d(data = novoice, aes(x = coordlog, y = coordlat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon')+
  scale_fill_gradient(low = "green", high = "red")+
  scale_alpha(range = c(0.00, 0.25), guide = FALSE)

# fire
ggmap(BaltimoreMap1, extent = "panel", maprange=FALSE)+geom_point(data = fire,aes(x=coordlog,y=coordlat)) +geom_density2d(data = fire, aes(x = coordlog, y = coordlat)) +
  stat_density2d(data = fire, aes(x = coordlog, y = coordlat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon')+
  scale_fill_gradient(low = "green", high = "red")+
  scale_alpha(range = c(0.00, 0.25), guide = FALSE)


# burglary
ggmap(BaltimoreMap1, extent = "panel", maprange=FALSE)+geom_point(data = burglary,aes(x=coordlog,y=coordlat)) +geom_density2d(data = burglary, aes(x = coordlog, y = coordlat)) +
  stat_density2d(data = burglary, aes(x = coordlog, y = coordlat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon')+
  scale_fill_gradient(low = "green", high = "red")+
  scale_alpha(range = c(0.00, 0.25), guide = FALSE)


# larceny
ggmap(BaltimoreMap1, extent = "panel", maprange=FALSE)+geom_point(data = larceny,aes(x=coordlog,y=coordlat)) +geom_density2d(data = larceny, aes(x = coordlog, y = coordlat)) +
  stat_density2d(data = larceny, aes(x = coordlog, y = coordlat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon')+
  scale_fill_gradient(low = "green", high = "red")+
  scale_alpha(range = c(0.00, 0.25), guide = FALSE)

# missing person
ggmap(BaltimoreMap1, extent = "panel", maprange=FALSE)+geom_point(data = mp,aes(x=coordlog,y=coordlat)) +geom_density2d(data = mp, aes(x = coordlog, y = coordlat)) +
  stat_density2d(data = mp, aes(x = coordlog, y = coordlat,  fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = 'polygon')+
  scale_fill_gradient(low = "green", high = "red")+
  scale_alpha(range = c(0.00, 0.25), guide = FALSE)

################################################################











# Map of Violent Crime Density in Baltimore for each month

city +
  stat_density2d(aes(x = coordlog, y = coordlat, fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = violent) +
  scale_fill_gradient(low = "black", high = "red")+facet_wrap(~month,ncol = 4)+
  ggtitle("Map of Violent Crime Density in Baltimore for each month")


city +
  stat_density2d(aes(x = coordlog, y = coordlat, fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = violent)+
  scale_fill_gradient(low = "black", high = "red")+facet_wrap(~year,ncol = 2)+
  ggtitle("Map of Violent Crime Density in Baltimore for each year")



city +
  stat_density2d(aes(x = coordlog, y = coordlat, fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = violent) +
  scale_fill_gradient(low = "black", high = "red")+facet_wrap(~weekday,ncol = 3)+
  ggtitle("Map of Violent Crime Density in Baltimore for each weekday")


city +
  stat_density2d(aes(x = coordlog, y = coordlat, fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = violent) +
  scale_fill_gradient(low = "black", high = "red")+facet_wrap(~times,ncol = 6)+
  ggtitle("Map of Violent Crime Density in Baltimore for each hour")
















## plot data
# frequency of priority in each hour
ggplot(data = dat.imp ,aes(times , color = priority, group = priority)) + geom_freqpoly(stat = "count")

# frequency of priority in each district
ggplot(data = dat.imp ,aes(district , color = priority, group = priority)) + geom_freqpoly(stat = "count")

# frequency of priority in each month
ggplot(data = dat.imp ,aes(as.factor(month) , color = priority, group = priority)) + geom_freqpoly(stat = "count")

# frequency of priority in each year
ggplot(data = dat.imp ,aes(as.factor(year) , color = priority, group = priority)) + geom_freqpoly(stat = "count")






##

vcdt <-aggregate(count ~ district+year+ month, 
data = violent, 
FUN = sum, na.rm = TRUE)
vcdt$year <- ordered(vcdt$year)
vcdt$month <- ordered(vcdt$month)
ggplot(vcdt, aes(x=month, y=count,color = year , group = year)) +
  geom_line() + geom_point() + theme(legend.position = "top") + 
  facet_wrap(~ district , ncol = 4) 


vcdt <-aggregate(count ~ district+ month+times, 
                 data = violent, 
                 FUN = sum, na.rm = TRUE)
vcdt$month <- ordered(vcdt$month)
vcdt$times <- ordered(vcdt$times)
ggplot(vcdt, aes(x=times, y=count,color = district , group = district)) +
  geom_line() + geom_point() + theme(legend.position = "top") + 
  facet_wrap(~ month , ncol = 3) 




crimedt <-aggregate(count ~ district+ month+description, 
                 data = newdata, 
                 FUN = sum, na.rm = TRUE)
crimedt$month <- ordered(crimedt$month)
ggplot(crimedt, aes(x=month, y=count,color = district , group = district)) +
  geom_line() + geom_point() + theme(legend.position = "top") + 
  facet_wrap(~ description , ncol = 3) 













##

my_theme <- theme_bw() +
  theme(panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        axis.title = element_text(colour = "grey40"),
        plot.title = element_text(vjust = 2.0))


ggplot(data = vc, aes(x = weekday)) + 
  my_theme +
  geom_bar(stat = "count", fill = "black", width = 0.75) +
  ggtitle("Total Violent Crimes by week") +
  xlab(NULL) +
  ylab("Total Crime Count")

ggplot(data = vc, aes(x = month)) + 
  my_theme +
  geom_bar(stat = "count", fill = "black", width = 0.75) +
  ggtitle("Total Violent Crimes by month") +
  xlab(NULL) +
  ylab("Total Crime Count")

ggplot(data = vc, aes(x = year)) + 
  my_theme +
  geom_bar(stat = "count", fill = "black", width = 0.75) +
  ggtitle("Total Violent Crimes by year") +
  xlab(NULL) +
  ylab("Total Crime Count")


ggplot(data = vc, aes(x = district)) + 
  my_theme +
  geom_bar(stat = "count", fill = "black", width = 0.75) +
  ggtitle("Total Violent Crimes by district") +
  xlab(NULL) +
  ylab("Total Crime Count")








##
dat1 <- data.frame((type = newdata$description ))
type911 <- as.data.frame(table(dat1))
type911s <- type911[order(type911$Freq),]
par(mar=c(5,7,4,1)+.1)
barplot(type911s$Freq,names.arg = type911s$dat1,horiz = T,col = rainbow(20),las =1, xlab = "Count" , main = "Different Crime's Count")






imp2 <-mice(dat,1,seed = 123,nnet.MaxNWts = 10000)
dat.imp2 <- complete(imp2,action = 1)
dat.imp2 = subset(dat.imp2, !(district %in% c("EVT1","EVT2")))


#########predict??   Error in nnet.default(X, Y, w, mask = mask, size = 0, skip = TRUE, softmax = TRUE,  : too many (2028) weights
dat.imp2$times <- str_extract(dat.imp2$callDateTime ,  "[0-9]+\\:[0-9]+\\:[0-9]+ +[A-Z]+")
dat.imp2$time24 <- as.POSIXct(dat.imp2$times, format='%I:%M:%S %p')
dat.imp2$times <- substr(dat.imp2$time24,12,19)
dat.imp2$times <- substr(dat.imp2$times,1,2)

traindt <- dat.imp2[1:1000,]

drop.levels(levels(traindt$district)[which(levels(traindt$district) != "EVT2")])

testdt <- dat.imp2[1001:1300,]
drop.levels(levels(testdt$district)[which(levels(testdt$district) != "EVT2")])


traindt$callDateTime <-NULL
traindt$time24 <- NULL
traindt$date <- NULL
traindt$year <- NULL
traindt$count <- NULL
traindt$description <- as.factor(traindt$description)
traindt$times <- as.factor(traindt$times)

testdt$callDateTime <-NULL
testdt$time24 <- NULL
testdt$date <- NULL
testdt$year <- NULL
testdt$count <- NULL
testdt$description <- as.factor(testdt$description)
testdt$times <- as.factor(testdt$times)

install.packages("nnet")
library(nnet)
install.packages("gdata")
library(gdata)
fit <- multinom(district ~. , data = traindt)



