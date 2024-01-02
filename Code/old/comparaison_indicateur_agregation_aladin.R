# Libraries
library(readr)
library(dplyr)
library(readxl)
library(stringi)
library(lubridate)
library(tidyr)
library(data.table)
library(leaflet)
library(ggplot2)



# Data aladin 1980 -------------------------------------------------------------
file = "../Data/aladin/CM5_CNRM-ALADIN63_Historical_day_19800101-19801231.txt"
aladin <- read_delim(file, delim = ";", skip=58, col_names =F)


names(aladin) <- c("date", "latitude", "longitude",  "tasminAdjust",
                   "tasmaxAdjust", "tasAdjust", "prtotAdjust")

aladin <- aladin %>% mutate(date = ymd(date),
                            annee = year(date))


aladin_ag <- aladin %>% group_by(latitude,longitude, annee) %>%
  summarise(prcptot = sum(prtotAdjust),
            prcpmean = mean(prtotAdjust),
            tasmax= max(tasmaxAdjust),
            tasmean = mean(tasAdjust))

write_rds(aladin_ag, "../Data/aladin_ag_1980.rds")




# Aladin 1982 ------------------------------------------------------------------
file = "../Data/aladin/CM5_CNRM-ALADIN63_Historical_day_19820101-19821231.txt"
aladin82 <- read_delim(file, delim = ";", skip=58, col_names=F)

names(aladin82) <- c("date", "latitude", "longitude",  "tasminAdjust",
                   "tasmaxAdjust", "tasAdjust", "prtotAdjust")

aladin82 <- aladin82 %>% mutate(date = ymd(date),
                            annee = year(date))

aladin_ag <- aladin82 %>% 
  group_by(latitude,longitude, annee) %>%
  summarise(prcptot = round(sum(prtotAdjust),2),
          prcpmean = round(mean(prtotAdjust),2),
          tasmax_Max= round(max(tasmaxAdjust),2),
          tasmax_Mean= round(mean(tasmaxAdjust),2),
          tas_Mean = round(mean(tasAdjust),2),
          tasmin_Mean = round(mean(tasminAdjust),2))





# Indicateurs construits par DRIAS ---------------------------------------------
file = "../Data/aladin/indicesALADIN63_CNRM-CM5_1982_2005_all.txt"
indicateurs <- read_delim(file, delim = ";", skip = 44)  %>% 
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  filter(! is.na(longitude)) %>%
  select(-starts_with("..")) %>%
  mutate(expo=1,
         annee = as.integer(annee))




# Merge indicateurs et agrégation maison ---------------------------------------
setDT(aladin_ag)
setDT(indicateurs)
t = merge(aladin_ag %>% mutate(longitude = round(longitude,2),
                               latitude = round(latitude,2)), 
          indicateurs %>% mutate(longitude = round(longitude,2),
                                 latitude = round(latitude,2)) %>%
            filter(annee == 1982),
          by = c("latitude", "longitude"))


t <- t %>% mutate(delta_rr = abs(rr-prcptot),
                  delta_pav = abs(pav-prcpmean),
                  delta_tav = abs(tav-tas_Mean),
                  delta_tnav = abs(tnav-tasmin_Mean),
                  delta_txav = abs(txav-tasmax_Mean))
summary(t[,42:46])




# Cartes : représentaion dans le plan des points d extraction ------------------
library(sf)

my_sf <- st_as_sf(aladin_ag, coords = c("longitude", "latitude"))
my_sf <- st_set_crs(my_sf, value = 4326)

ggplot(my_sf) + 
  geom_sf(aes(color = round(tasmax_Max)))


leaflet(aladin_ag) %>% addTiles() %>% 
  addCircleMarkers(radius=1, stroke=F, color="black", fillOpacity=1)

leaflet(indicateurs) %>% addTiles() %>% 
  addCircleMarkers(radius=1, stroke=F, color="black", fillOpacity=1)



