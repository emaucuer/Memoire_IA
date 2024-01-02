library(dplyr)
library(readr)
library(data.table)
library(ks)
library(spatstat.core)
library(sf)



# ---------------------------------------------------------------------------- #
# 1) Import & prepare data -----------------------------------------------------
# ---------------------------------------------------------------------------- #
dossier_zip <- "../../Data/sswi/SWI_Package_1969-2021.zip"
 
all_swi_files <- grep("swi", 
                      unzip(zipfile = dossier_zip, list = TRUE)$Name, 
                      value = TRUE) 


dataCoord <- read_rds("../../Data/Administratif/coordCom2018.rds")
dataCoord <- dataCoord %>% 
  dplyr::rename(id = code) %>% 
  filter(id < 96000) %>%
  arrange(id)


### Import all data ------------------------------------------------------------

all_swi <- data.table()
for (file in all_swi_files){
  current_swi <- read_csv2(unz(dossier_zip, file))
  setDT(current_swi)
  all_swi <- rbind(all_swi, current_swi)
}

### Management of time variables  -----------------------------------------------
names(all_swi) <- tolower(names(all_swi))
all_swi <- all_swi[,mois := as.integer(substr(date,5,6))]
all_swi <- all_swi[,annee := as.integer(substr(date,1,4))]


# ---------------------------------------------------------------------------- #
# 2) Transform to a normal distribution for each point and month ---------------
# ---------------------------------------------------------------------------- #

### Test -----------------------------------------------------------------------
var(all_swi[numero == 2 & mois == 1]$swi_unif_mens3)
hist(all_swi[numero == 2 & mois == 1]$swi_unif_mens3)
swi_test <- all_swi[numero == 2 & mois == 1]
fit <- density(swi_test$swi_unif_mens3)
cdf_fit <- CDF(fit)
cdf_fit(swi_test$swi_unif_mens3)


### Process --------------------------------------------------------------------
start <- Sys.time()
for (var_num in unique(all_swi$numero)){ 
  for (var_mois in 1:12){
    density_fit <- density(all_swi[numero == var_num & mois == var_mois]$swi_unif_mens3)
    cdf_fit <- CDF(density_fit)
    
    all_swi[numero == var_num & mois == var_mois, sswi := qnorm(cdf_fit(swi_unif_mens3))]
  }
}
finish <- Sys.time()
print(finish-start)


# saveRDS(all_swi, "../../Data/sswi/2-Intermediate/all_sswi.rds")



# ---------------------------------------------------------------------------- #
# 3) Yearly aggregation --------------------------------------------------------
# ---------------------------------------------------------------------------- #

all_sswi_yearly <- all_swi[, .(sswi_min = min(sswi),
                              sswi_max = max(sswi),
                              sswi_moy = mean(sswi)), 
                          by = .(numero, annee)]



# ---------------------------------------------------------------------------- #
# 4) Management of localisation variables ---------------------------------------
# ---------------------------------------------------------------------------- #
coord <- unique(all_swi[,.(numero, lambx, lamby)])

coord <- coord %>% 
  st_as_sf(coords = c("lambx", "lamby")) %>%
  st_set_crs(2154)
st_is_longlat(coord)

coord_reproj <- st_transform(coord, 4326)
st_is_longlat(coord_reproj)

coord_reproj <- st_coordinates(coord_reproj) %>%
  bind_cols(coord_reproj) %>%
  rename(longitude = X, latitude = Y) %>%
  select(-geometry)


# test plot
leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)  %>%
  setView(lng = 3, lat = 47, zoom = 4) %>%
  addCircleMarkers(data = coord_reproj, ~as.numeric(longitude), ~as.numeric(latitude),
                   stroke = FALSE, radius = 2, fillOpacity  = 1)


all_sswi_yearly <- merge.data.table(all_sswi_yearly,coord_reproj,
                                    by = c("numero"), all.x = T)



# ---------------------------------------------------------------------------- #
# 5) Save data table -----------------------------------------------------------
# ---------------------------------------------------------------------------- #
all_sswi_yearly_1982 <- all_sswi_yearly[annee >= 1982]
                                        
write_rds(all_sswi_yearly_1982, "../../Data/sswi/sswi_yearly_1982_2021.rds")



# ---------------------------------------------------------------------------- #
# 6) Lissage -------------------------------------------------------------------
# ---------------------------------------------------------------------------- #

### Fonction de lissage
source("../00_lissage.R")


### Premier lissage a 1e4 ------------------------------------------------------
start = Sys.time()
sswi_1e4 <- fct_lissage(1e4, dataCoord, all_sswi_yearly_1982, 
                        1982:2021, c("sswi_min", "sswi_max", "sswi_moy"))
print(Sys.time()-start)

#write_rds(sswi_1e4, "../../Data/sswi/sswi_1982_2021_lissage_1e4.rds")



### Second lissage a 1e3 pour les points problematiques ------------------------
dfCoord_relance <- unique(sswi_1e4 %>% 
                            filter_all(any_vars(is.na(.))) %>% 
                            select(id, longM, latM))

sswi_1e3 <- fct_lissage(1e3, dfCoord_relance, all_sswi_yearly_1982, 
                        1982:2021, c("sswi_min", "sswi_max", "sswi_moy"))


### Base finale ----------------------------------------------------------------
final_sswi <- sswi_1e4 %>% 
  filter_all(all_vars(!is.na(.))) %>%
  bind_rows(sswi_1e3)

write_rds(final_sswi, "../../Data/sswi/sswi_yearly_1982_2021_smooth.rds")





