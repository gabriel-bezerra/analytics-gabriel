dados.brutos <- read.table(file = "hsales.dat")

max.brutos <- max(dados.brutos)
min.brutos <- min(dados.brutos)

dados.normalizados <- (dados.brutos - min.brutos) / (max.brutos - min.brutos)

shift <- function(x) {
    l = nrow(x)
    rbind(tail(x, l-1), c(NA))
}

mes1 <- dados.normalizados
mes2 <- shift(mes1)
mes3 <- shift(mes2)
mes4 <- shift(mes3)

dados.de.venda.com.nas = data.frame("mes1"=mes1[,1],
                                    "mes2"=mes2[,1],
                                    "mes3"=mes3[,1],
                                    "mes4"=mes4[,1])


# Remove NAs no fim do arquivo

tamanho.com.nas = nrow(dados.de.venda.com.nas)
tamanho.sem.nas = tamanho.com.nas - 3

dados.de.venda <- head(dados.de.venda.com.nas, tamanho.sem.nas)

