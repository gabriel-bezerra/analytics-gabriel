arquivo.de.dados <- commandArgs(trailingOnly = TRUE)[1]

dados <- read.table(file = arquivo.de.dados, header = TRUE)

summary(dados)
