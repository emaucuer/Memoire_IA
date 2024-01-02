# test data table vs data frame



dataCoord <- read_rds(("../Data/coordCom2018.rds"))
dataCoord <- dataCoord %>% 
  rename(id = code) %>% 
  filter(id < 96000) %>%
  arrange(id)


# Data frame ----------------------------------------------------------------
fct_lissage <- function(P1, dfcoord, dfvars, list_an, list_indic){
  dataFinale = data.frame()
  for (year in list_an) {
    print(year)
    dataVar1Y <- dfvars %>% filter(annee==year)
    dataFinale1Y <- dfcoord %>% mutate(annee = year)
    
    for (var in list_indic){
      dataVar1Y <- dataVar1Y %>% rename(indicateur = var)
      
      dataFinale1Y$indicateur = 
        sapply(1:dim(dataFinale1Y)[1], function(i) sum(dataVar1Y$indicateur*exp(-sqrt((dataFinale1Y$long[i] - dataVar1Y$longitude)^2 + 
                                                                                        (dataFinale1Y$lat[i] - dataVar1Y$latitude)^2)*P1)*dataVar1Y$expo) / 
                 sum(exp(-(sqrt((dataFinale1Y$long[i] - dataVar1Y$longitude)^2 + (dataFinale1Y$lat[i] - dataVar1Y$latitude)^2))*P1)*dataVar1Y$expo))
      
      dataFinale1Y <- dataFinale1Y %>% rename(!!var := indicateur)
      dataVar1Y <- dataVar1Y %>% rename(!!var := indicateur)
    }
    
    dataFinale <- dataFinale %>% bind_rows(dataFinale1Y)
  }
  return(dataFinale)
}

dataVar <- read_delim("../Data/aladin/indicesALADIN63_CNRM-CM5_1982_2005_all.txt",
                      delim = ";", skip = 44) # skip = 27


dataVar <- dataVar %>% 
  rename_with(~ tolower(gsub("#| ", "",.x))) %>%
  filter(! is.na(longitude)) %>%
  select(-starts_with("..")) %>%
  mutate(expo=1)


start = Sys.time()
test1 <- fct_lissage(1e4, dataCoord, dataVar, c(1982), c("tav"))
Sys.time() - start
# Time difference of 12.26768 secs

# Data table -------------------------------------------------------------------
fct_lissageDT <- function(P1, dfcoord, dfvars, list_an, list_indic){
  dataFinale = data.table()
  for (year in list_an) {
    print(year)
    dataVar1Y <- dfvars[annee==year,]
    dataFinale1Y <- dfcoord[,annee := year]
    
    for (var in list_indic){
      setnames(dataVar1Y, var, "indicateur")
      #dataVar1Y <- dataVar1Y %>% rename(indicateur = var)
      
      dataFinale1Y$indicateur = 
        sapply(1:dim(dataFinale1Y)[1], function(i) sum(dataVar1Y$indicateur*exp(-sqrt((dataFinale1Y$long[i] - dataVar1Y$longitude)^2 + 
                                                                                        (dataFinale1Y$lat[i] - dataVar1Y$latitude)^2)*P1)*dataVar1Y$expo) / 
                 sum(exp(-(sqrt((dataFinale1Y$long[i] - dataVar1Y$longitude)^2 + (dataFinale1Y$lat[i] - dataVar1Y$latitude)^2))*P1)*dataVar1Y$expo))
      
      setnames(dataVar1Y, "indicateur", var)
      setnames(dataFinale1Y, "indicateur", var)
      # dataFinale1Y <- dataFinale1Y %>% rename(!!var := indicateur)
      # dataVar1Y <- dataVar1Y %>% rename(!!var := indicateur)
    }
    
    dataFinale <- rbindlist(list(dataFinale, dataFinale1Y))
  }
  return(dataFinale)
}

setDT(dataVar)
setDT(dataCoord)

start = Sys.time()
test1 <- fct_lissageDT(1e4, dataCoord, dataVar, c(1982), c("tav"))
Sys.time() - start

#Time difference of 11.96963 secs