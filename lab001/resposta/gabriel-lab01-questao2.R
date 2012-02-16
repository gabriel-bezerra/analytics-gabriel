source("region-function.R")

data = read.csv("salarios-ti-formatted.csv")
data.with.region = cbind(data, Regiao = mapply(regionOf, data$UF))

aggregate(data.with.region, Regiao)
#average.wage = data.frame()

#write.table(data.with.region, file="output-questao2.txt")

