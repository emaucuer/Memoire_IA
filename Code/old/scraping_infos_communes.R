library(readr)
library(stringi)

path= "G:/Drive partagés/BU_ACT_FR/4 - Mémoires/Mémoires 2022/Mémoire de MAUCUER Eurydice/"
insee_dep <- read_csv(paste0(path,"Data/cog_ensemble_2021_csv/departement2021.csv"))


communes <- data.frame()
for (id_dep in  insee_dep$DEP[1]){
  # Extraction de la la liste des communes par departement
  lien_dep = paste0("https://www.annuaire-des-mairies.com/", id_dep)
  html <- paste(readLines(lien_dep, encoding="UTF−8"),collapse = "\n")
  extract <- as.data.frame(stri_match_all_regex(html, '<a href=\"(.*?)\\.html\">')[[1]])
  extract$dep <- id_dep
  
  # Extraction des inforamtions par commune
  for (id_com in extract$V2){
    lien = paste0(lien_dep, "/", id_com, ".html")
    
  }
  
  #
  communes <- rbind(communes, extract)
}



content <- stri_match_all_regex(html, "Renseignements sur la commune de ((.|\n)*)")

