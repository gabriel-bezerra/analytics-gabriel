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

    summary.data = data.frame(laboratorio = media$laboratorio,
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


# Plota os histogramas de intervalos de tempo para cada laboratorio
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

histogramas.tempo.ocioso <- function(ocioso) histogramas.tempo(TRUE)
histogramas.tempo.ocupado <- function(ocioso) histogramas.tempo(FALSE)


# Plota os boxplots de intervalos de tempo para cada laboratorio
boxplots.tempo <- function(ocioso) {
    boxplot(intervalo ~ laboratorio,
            data[data$ociosa == ocioso, c("intervalo", "laboratorio")],
            outline = FALSE,
            sub = paste("Este boxplot desconsidera valores maiores que Q3 + 1.5*IQR,",
                        "para melhor visualização"),
            main = paste("Tempo", ifelse(ocioso, "ocioso", "ocupado"),
                         "das máquinas de cada laboratório"),
            ylab = "Duração do intervalo",
            xlab = "Laboratório",
            cex.axis = 0.8)
}

boxplots.tempo.ocioso <- function(ocioso) boxplots.tempo(TRUE)
boxplots.tempo.ocupado <- function(ocioso) boxplots.tempo(FALSE)



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
##OK

# gerar arquivo de texto com tabela de estatísticas
write.table(summary.tempo.ocioso, file = "output-questao1-tempo-ocioso.txt")

# gerar arquivo de imagem com os histogramas
png(filename = "output-questao1-hist-tempo-ocioso.png", width = 2*720, height = 720)
numero.de.histogramas = nlevels(data$laboratorio)
par(mfcol = c(numero.de.histogramas, 1))
    histogramas.tempo.ocioso()
dev.off()

# gerar arquivo de imagem com boxplots
png(filename = "output-questao1-boxplots-tempo-ocioso.png", width = 720, height = 720)
    boxplots.tempo.ocioso()
dev.off()


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
##OK

# gerar arquivo de texto com tabela de estatísticas
write.table(summary.tempo.ocupado, file = "output-questao1-tempo-ocupado.txt")

# gerar arquivo de imagem com os histogramas
png(filename = "output-questao1-hist-tempo-ocupado.png", width = 2*720, height = 720)
numero.de.histogramas = nlevels(data$laboratorio)
par(mfcol = c(numero.de.histogramas, 1))
    histogramas.tempo.ocupado()
dev.off()

# gerar arquivo de imagem com boxplots
png(filename = "output-questao1-boxplots-tempo-ocupado.png", width = 720, height = 720)
    boxplots.tempo.ocupado()
dev.off()


# Proporção do tempo em que as máquinas estiveram ocupadas (considerando os intervalos medidos),
# agrupadas por laboratório

summary.tempo.por.maquina <- function() {
    summary.tempo.ocupado.por.maquina = with(data[data$ociosa == FALSE, ],
                                             aggregate(data[data$ociosa == FALSE, ]$intervalo,
                                                       by = list(laboratorio, maquina),
                                                       sum))
    names(summary.tempo.ocupado.por.maquina) <- c("laboratorio", "maquina", "tempo.ocupada")

    summary.tempo.ocioso.por.maquina = with(data[data$ociosa == TRUE, ],
                                            aggregate(data[data$ociosa == TRUE, ]$intervalo,
                                                      by = list(laboratorio, maquina),
                                                      sum))
    names(summary.tempo.ocioso.por.maquina) <- c("laboratorio", "maquina", "tempo.ociosa")

    summary.tempo.por.maquina = merge(summary.tempo.ocupado.por.maquina,
                                      summary.tempo.ocioso.por.maquina)


    summary.tempo.por.maquina$prop.ocupada = summary.tempo.por.maquina$tempo.ocupada /
                                             (summary.tempo.por.maquina$tempo.ociosa +
                                              summary.tempo.por.maquina$tempo.ocupada)

    return(summary.tempo.por.maquina)
}
summary.tempo.por.maquina <- summary.tempo.por.maquina()

aggregate.proporcao.por.laboratorio <- function(funcao, ...) {
    aggregate(summary.tempo.por.maquina$prop.ocupada,
              list(laboratorio = summary.tempo.por.maquina$laboratorio),
              funcao, ...)
}

proporcao.media.de.tempo.ocupado <-
    data.frame(laboratorio = aggregate.tempo.ocioso(sum)$laboratorio,
               x = aggregate.tempo.ocupado(sum)$x / (aggregate.tempo.ocioso(sum)$x
                                                     + aggregate.tempo.ocupado(sum)$x))

summary.proporcao.ocupado <- function() {
    #media
    media = proporcao.media.de.tempo.ocupado
    #mediana
    mediana = aggregate.proporcao.por.laboratorio(median)
    #minimo
    minimo = aggregate.proporcao.por.laboratorio(min)
    #maximo
    maximo = aggregate.proporcao.por.laboratorio(max)
    #1o quartil
    primeiro.quartil = aggregate.proporcao.por.laboratorio(quantile, prob = 0.25)
    #3o quartil
    terceiro.quartil = aggregate.proporcao.por.laboratorio(quantile, prob = 0.75)
    #5 percentil
    cinco.percentil = aggregate.proporcao.por.laboratorio(quantile, prob = 0.05)
    #95 percentil
    noventa.e.cinco.percentil = aggregate.proporcao.por.laboratorio(quantile, prob = 0.95)
    #desvio padrao
    desvio.padrao = aggregate.proporcao.por.laboratorio(sd)
    #IQR
    iqr = data.frame(primeiro.quartil$laboratorio, x = (terceiro.quartil$x - primeiro.quartil$x))

    summary.data = data.frame(laboratorio = media$laboratorio,
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

# media
# mediana
# minimo e maximo
# 1o e 3o quartis
# 5-percentil e 95-percentil
# desvio padrao
# IQR
summary.prop.ocupado = summary.proporcao.ocupado()

# gerar histograma
histograma.proporcao.ocupado <- function() {
    by(summary.tempo.por.maquina[, c("laboratorio", "prop.ocupada")],
       summary.tempo.por.maquina$laboratorio,
       function(x) {
           nome.laboratorio = levels(factor(x$laboratorio))

           hist(x$prop.ocupada,
                freq = FALSE,
                main = paste("Proporção de tempo de máquina ocupado em", nome.laboratorio),
                xlab = "Proporção de tempo em que as máquinas estiveram ocupadas")

           # Plota a reta da média
           with(proporcao.media.de.tempo.ocupado[
                    proporcao.media.de.tempo.ocupado$laboratorio == nome.laboratorio, ],
                abline(v = x, col = 1, lty=2, lwd = 2))

           # Plota a reta da mediana
           abline(v = median(x$prop.ocupada), col = 2, lty=4, lwd = 2)

           legend("topright", c("Média", "Mediana"), col = 1:2, lty = c(2, 4), lwd = 2)

           # TODO:
           # deixar os histogramas com eixo X em [0, 1]
           # testar outras funcoes de quantidade de celulas no histograma
       })
}

# gerar boxplot

# gerar arquivo de texto com tabela de estatísticas
write.table(summary.prop.ocupado, file = "output-questao1-prop-ocupado.txt")

# gerar arquivo de imagem com os histogramas
png(filename = "histograma-prop.png", width = 720, height = 720)
numero.de.histogramas = nlevels(summary.tempo.por.maquina$laboratorio)
par(mfrow = c(numero.de.histogramas, 1))
    histograma.proporcao.ocupado()
dev.off()

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
