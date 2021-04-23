
# https://davidcarslaw.github.io/openair/

# https://bookdown.org/david_carslaw/openair/


rm(list=ls())
gc()

library("openair")

kc1 <- importAURN(site = "kc1", year = 2011:2012)

head(kc1)


sub <- selectByDate(kc1, day = "weekday", year = 2012, month = 6:9, hour = 7:19)
sub

sub2 <- timeAverage(kc1, avg.time = "2 week")
sub2


############################################################

kc1.data.frame<-as.data.frame(kc1)
head(kc1.data.frame)
kc1.data.frame$date<-as.Date(kc1.data.frame$date)
kc1.data.frame$date<-as.POSIXct(kc1.data.frame$date)


kc1.tibble<-as_tibble(kc1.data.frame)
windRose(kc1.tibble)
windRose(kc1.tibble, type = "year", layout = c(2, 4))

############################################################

rm(list=ls())
gc()

library("openair")

?mydata

data(mydata)

datos_air<-mydata
rm(mydata)

windRose(datos_air)

windRose(datos_air, type = "year", layout = c(2, 4))

windRose(datos_air, type = "season",layout = c(2, 2))


kc1 <- importAURN(site = "kc1", year = 2011:2012)
windRose(kc1)



##############################################################

rm(list=ls())
gc()

library("openair")
library("tidyverse")
library("leaflet")

aurn_detailed <- importMeta(source = "aurn", all = TRUE) 

aurn_unique <- distinct(aurn_detailed, site, .keep_all = TRUE) # igual que unique.data.frame

# information for map markers
content <- paste(
  paste(
    aurn_unique$site,
    paste("Code:", aurn_unique$code),
    paste("Start:", aurn_unique$start_date),
    paste("End:", aurn_unique$end_date),
    paste("Site Type:", aurn_unique$site_type),
    sep = "<br/>"
  )
)


# plot map
leaflet(aurn_unique) %>%
  addTiles() %>%
  addMarkers(~ longitude, ~ latitude, popup = content,
             clusterOptions = markerClusterOptions())


##############################################################
