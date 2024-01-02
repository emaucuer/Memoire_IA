library(ggplot2)
library(dplyr)
library(readr)
library(data.table)
library(sf)

# DEPARTEMENTS -------------------------------------------------------

dataFinaleDep <- read_rds("../Data/data_modelisation_dep_1982_2021.rds")
dataFinaleDep <- dataFinaleDep %>%
  bind_rows(dataFinaleDep %>% 
              group_by(dep) %>%
              summarise(secheresse=sum(secheresse),
                        inondation=sum(inondation)) %>%
              mutate(annee = 0))

dep_bound <- read_rds("../Data_GIS/geojson/departements-vs-om.rds")
setDT(dataFinaleDep)
setDT(dep_bound)
dataPlotDep <- merge(dataFinaleDep, dep_bound, by.x="dep", by.y="code")



y = 0
dataPlot1YDep<- dataPlotDep %>% filter(annee == y)
dataPlot1YDep <- st_as_sf(dataPlot1YDep)

ggplot(data=dataPlot1YDep)+
  geom_sf(aes(fill = inondation), colour = NA) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme(legend.position="right",
        panel.background=element_rect(fill="white")) 





# COMMUNES -------------------------------------------------------

dataFinaleCom <- read_rds("../Data/data_modelisation_1982_2021_all_V2.rds")
dataFinaleCom_agg <- dataFinaleCom %>% 
  group_by(id) %>%
  summarise(secheresse=sum(secheresse, na.rm = T), inondation=sum(inondation)) 

com_bound <- read_rds("../Data_GIS/geojson/communes-vs-om.rds")
setDT(dataFinaleCom_agg)
setDT(com_bound)
dataPlot<- merge(dataFinaleCom_agg, com_bound, by.x="id", by.y="code")



# y = 1982
# dataPlot1Y<- dataPlot %>% filter(annee == y)
# dataPlot1Y <- st_as_sf(dataPlot1Y)

ggplot(data=dataPlot) +
  geom_sf(aes(geometry = geometry,fill = inondation), colour = NA) +
  scale_fill_gradient(low = "#E7F6FF", high = "#084594") +
  theme(legend.position="right",
        panel.background=element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Nombre de CATNAT inondation depuis le début du régime")



ggplot(data=dataPlot) +
  geom_sf(aes(geometry = geometry,fill = secheresse), colour = NA) +
  scale_fill_gradient(low = "#FEE0D2", high = "#99000D") +
  theme(legend.position="right",
        panel.background=element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Nombre de CATNAT sécheresse depuis le début du régime")


