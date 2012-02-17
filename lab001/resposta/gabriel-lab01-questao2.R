# Calcule a média salarial para cada região usando as informações adicionadas na Questão 1. Como
# resultado, apresente uma tabela com o nome da região e a sua média salarial. A tabela deve ser
# ordenada da região com a maior média salarial para a menor.

source("region-function.R")

data = read.csv("salarios-ti-formatted.csv")
data.with.region = cbind(data, Regiao = mapply(regionOf, data$UF))

average.wage = with(data.with.region,
                    aggregate(Salario.Bruto,
                              list(Regiao),
                              mean))
names(average.wage) <- c("Regiao", "Media.Salario")

ordered.average.wage = average.wage[order(average.wage$Media.Salario, decreasing = TRUE), ]
write.table(ordered.average.wage, file="output-questao2.txt")

