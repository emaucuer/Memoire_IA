##Import data----------------------------------------------
data <- read_rds("../Data/modelisation/data_modelisation_1982_2021.rds")
data_all <- read_rds("../Data/modelisation/data_modelisation_1982_2021_all.rds")
data_V2 <- read_rds("../Data/modelisation/data_modelisation_1982_2021_all_V2.rds")

data_tot <- merge(data_all, data, by = c("id", "annee", "longM", "latM"), 
                  suffixes = c("_all", "_Vi"))
