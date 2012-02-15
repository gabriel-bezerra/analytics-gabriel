source("region-function.R")

data = read.csv("salarios-ti-formatted.csv")
data.with.region = cbind(data, Regiao = mapply(regionOf, data$UF))

write.table(data.with.region, file="output-questao2.txt")

