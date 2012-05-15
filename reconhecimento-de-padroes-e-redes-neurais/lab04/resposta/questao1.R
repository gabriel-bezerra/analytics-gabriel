dados.brutos <- read.table(file = "hsales.dat")

max.brutos <- max(dados.brutos)
min.brutos <- min(dados.brutos)

dados.normalizados <- (dados.brutos - min.brutos) / (max.brutos - min.brutos)



# para visualizacao dos dados

plot.seq <- function(sequencia)
    plot(data.frame(seq_len(nrow(sequencia)), sequencia), type = "l")

plot.seq(dados.brutos)
plot.seq(dados.normalizados)
