# Carregue os dados do arquivo de entrada e adicione uma nova coluna na tabela indicando a região do
# Brasil que o profissional trabalha (Centro-oeste, Nordeste, Norte, Sudeste ou Sul), com base na
# coluna que indica seu estado (UF). O resultado de saída deve ser a tabela de entrada contendo a
# nova coluna.

source("region-function.R")

data = read.csv("salarios-ti-formatted.csv")
data.with.region = cbind(data, Regiao = mapply(regionOf, data$UF))

write.table(data.with.region, file="output-questao1.txt")

