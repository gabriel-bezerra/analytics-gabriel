require("RSNNS")

file.name <- commandArgs(trailingOnly = TRUE)
dados <- read.csv(file = file.name)

summary(dados)

dados.caracteristicas <- dados[1:(ncol(dados) - 1)]
dados.classificacoes <- dados[ncol(dados)]

summary(dados.caracteristicas)
summary(dados.classificacoes)

