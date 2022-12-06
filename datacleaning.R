class(Passengers_11_10_2022.7_47_04.PM)
AirData <- Passengers_11_10_2022.7_47_04.PM

Airdata <- AirData[AirData$Month != "TOTAL", ]

DOMts <- ts(Airdata$DOMESTIC, start=c(2002,10), frequency = 12)
INTts <- ts(Airdata$INTERNATIONAL, start=c(2002,10), frequency = 12)
TOTts <- ts(Airdata$TOTAL, start=c(2002,10), frequency = 12)

agg_DOM <- aggregate(DOMts, nfrequency=4) 
agg_INT <- aggregate(INTts, nfrequency=4) 
agg_TOT <- aggregate(TOTts, nfrequency=4) 

QuartAirData <- data.frame(agg_DOM, agg_INT, agg_TOT)

GDPt <- GDP[GDP$DATE >= "2002-10-01",]
GDPg <-head(GDPt, -1)

ProjectData <- data.frame(GDPg$DATE, GDPg$GDP, agg_DOM, agg_INT, agg_TOT)
colnames(ProjectData) <- c("DATE", "GDP", "DOMESTIC", "INTERNATIONAL", "TOTAL")
write.csv(ProjectData, "projectdata.csv")