source("region-function.R")

data = read.csv("salarios-ti-formatted.csv")


write.table(data.with.region, file="output-questao2.txt")

