# Calcule o tempo de experiência profissional médio apenas dos usuários que trabalham na
# Iniciativa Privada. O resultado deve ser apenas um número, referente à média aritmética.

source("region-function.R")

data = read.csv("salarios-ti-formatted.csv")
data.with.region = cbind(data, Regiao = mapply(regionOf, data$UF))

experiencia.iniciativa.privada = data[data$Iniciativa.Privada.ou.Concursado == "Iniciativa Privada", ]$Experiencia.Profissional

experiencia.media = mean(experiencia.iniciativa.privada)

write.table(experiencia.media, file="output-questao3.txt", row.names = FALSE, col.names = FALSE)

