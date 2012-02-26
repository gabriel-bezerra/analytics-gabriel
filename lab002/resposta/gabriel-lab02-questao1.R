# Leitura do arquivo
data = read.table("atividade-maquinas-dsc.txt", header = TRUE)

# Funcoes de agregacao

aggregate.tempo <- function(ocioso, funcao.de.agregacao, ...) {
    tempo.por.laboratório = with(data[data$ociosa == ocioso, ],
                                 aggregate(intervalo,
                                           list(laboratorio),
                                           funcao.de.agregacao,
                                           ...)
                                 )
    colnames(tempo.por.laboratório) = c("laboratorio", "x")
    return(tempo.por.laboratório)
}

aggregate.tempo.ocupado <- function(funcao.de.agregacao, ...)
    aggregate.tempo(FALSE, funcao.de.agregacao, ...)

aggregate.tempo.ocioso <- function(funcao.de.agregacao, ...)
    aggregate.tempo(TRUE, funcao.de.agregacao, ...)


# Calculo dos dados da questao

summary.tempo <- function(ocioso) {
    #media
    media = aggregate.tempo(ocioso, mean)
    #mediana
    mediana = aggregate.tempo(ocioso, median)
    #minimo
    minimo = aggregate.tempo(ocioso, min)
    #maximo
    maximo = aggregate.tempo(ocioso, max)
    #1o quartil
    primeiro.quartil = aggregate.tempo(ocioso, quantile, prob = c(0.25))
    #3o quartil
    terceiro.quartil = aggregate.tempo(ocioso, quantile, prob = c(0.75))
    #5 percentil
    cinco.percentil = aggregate.tempo(ocioso, quantile, prob = c(0.05))
    #95 percentil
    noventa.e.cinco.percentil = aggregate.tempo(ocioso, quantile, prob = c(0.95))
    #desvio padrao
    desvio.padrao = aggregate.tempo(ocioso, sd)
    #IQR
    iqr = data.frame(primeiro.quartil$laboratorio, x = (terceiro.quartil$x - primeiro.quartil$x))

    summary.data = cbind(laboratorio = media$laboratorio,
                         media = media$x,
                         mediana = mediana$x,
                         minimo = minimo$x,
                         maximo = maximo$x,
                         primeiro.quartil = primeiro.quartil$x,
                         terceiro.quartil = terceiro.quartil$x,
                         cinco.percentil = cinco.percentil$x,
                         noventa.e.cinco.percentil = noventa.e.cinco.percentil$x,
                         desvio.padrao = desvio.padrao$x,
                         iqr = iqr$x)

    return(summary.data)
}


# Plota os histogramas para cada laboratorio
histogramas.tempo <- function(ocioso)
    by(data[data$ociosa == ocioso, c("intervalo", "laboratorio")],
       data[data$ociosa == ocioso, ]$laboratorio,
       function(x) {
           lab.name = levels(factor(x$laboratorio))

           hist(x$intervalo,
                main = paste("Tempo", ifelse(ocioso, "ocioso", "ocupado"), lab.name),
                xlab = "Duração do intervalo",
                breaks = "Scott",
                freq = FALSE)

           abline(v = mean(x$intervalo), col = 1, lty = 2, lwd = 2)
           abline(v = median(x$intervalo), col = 2, lty = 4, lwd = 2)
           legend("topright", c("Média", "Mediana"), col = 1:2, lty = c(2, 4), lwd = 2)
       })


# Duração do intervalo de tempo em que as máquinas estiveram ociosas, agrupadas por laboratório.

# media
# mediana
# minimo e maximo
# 1o e 3o quartis
# 5-percentil e 95-percentil
# desvio padrao
# IQR
summary.tempo.ocioso = summary.tempo(TRUE)

# gerar histograma
##OK

# gerar boxplot


# gerar arquivo de texto com tabela de estatísticas
write.table(summary.tempo.ocioso, file = "output-questao1-tempo-ocioso.txt")

# gerar arquivo de imagem com os histogramas
png(filename = "output-questao1-plot-tempo-ocioso.png", width = 2*720, height = 720)
numero.de.histogramas = nlevels(data$laboratorio)
par(mfcol = c(numero.de.histogramas, 1))
    histogramas.tempo(TRUE)
dev.off()

# gerar arquivo de imagem com boxplots


# Duração do intervalo de tempo em que as máquinas estiveram ocupadas, agrupadas por laboratório.

# media
# mediana
# minimo e maximo
# 1o e 3o quartis
# 5-percentil e 95-percentil
# desvio padrao
# IQR
summary.tempo.ocupado = summary.tempo(FALSE)

# gerar histograma
##OK

# gerar boxplot

# gerar arquivo de texto com tabela de estatísticas
write.table(summary.tempo.ocupado, file = "output-questao1-tempo-ocupado.txt")
# gerar arquivo de imagem com os histogramas
png(filename = "output-questao1-plot-tempo-ocupado.png", width = 2*720, height = 720)
numero.de.histogramas = nlevels(data$laboratorio)
par(mfcol = c(numero.de.histogramas, 1))
    histogramas.tempo(FALSE)
dev.off()

# gerar arquivo de imagem com boxplots


# Proporção do tempo em que as máquinas estiveram ocupadas (considerando os intervalos medidos),
# agrupadas por laboratório

# media
# mediana
# minimo e maximo
# 1o e 3o quartis
# 5-percentil e 95-percentil
# desvio padrao
# IQR

# gerar histograma
# gerar boxplot

# gerar arquivo de texto com tabela de estatísticas
# gerar arquivo de imagem com os histogramas
# gerar arquivo de imagem com boxplots

# Quantidade de vezes que as máquinas mudaram de estado, agrupadas por laboratório.

# media
# mediana
# minimo e maximo
# 1o e 3o quartis
# 5-percentil e 95-percentil
# desvio padrao
# IQR

# gerar histograma
# gerar boxplot

# gerar arquivo de texto com tabela de estatísticas
# gerar arquivo de imagem com os histogramas
# gerar arquivo de imagem com boxplots
