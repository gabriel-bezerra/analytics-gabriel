library("ggplot2")

source("intervalos-de-confianca.R")


## 3. Gere um gráfico com os intervalos de confiança da média dos gastos anuais
## dos deputados por estado. Você consegue identificar com 90% de confiança, um
## estado cujos deputados gastam, em média, mais que os deputados dos outros
## estados? Existem intervalos com tamanhos maiores que outros? Analise os
## dados para tentar descobrir porque a precisão de alguns intervalos está menor
## do que a de outros.

dados <- read.csv(file = "dados-deputados.csv")

nivel.de.significancia = 0.10


# Intervalos de confiança
ic.gastos.totais.por.estado <- aggregate(dados$gastos.total,
                                         list(estado = dados$estado),
                                         function(x) intervalo.de.confianca.para.a.media(x,
                                                                                         nivel.de.significancia))
names(ic.gastos.totais.por.estado) <- c("estado", "ic")

# Médias
media.gastos.totais.por.estado <-
    aggregate(dados$gastos.total,
              list(estado = dados$estado),
              mean)
names(media.gastos.totais.por.estado) <- c("estado", "media")

# Quantidade
n.gastos.totais.por.estado <-
    aggregate(dados$gastos.total,
              list(estado = dados$estado),
              length)
names(n.gastos.totais.por.estado) <- c("estado", "n")

# Desvio padrão
sd.gastos.totais.por.estado <-
    aggregate(dados$gastos.total,
              list(estado = dados$estado),
              sd)
names(sd.gastos.totais.por.estado) <- c("estado", "desvio.padrao")

# Merge
gastos.totais.por.estado <- merge(media.gastos.totais.por.estado,
                                  ic.gastos.totais.por.estado,
                                  by = c("estado"))

gastos.totais.por.estado <- merge(gastos.totais.por.estado,
                                  n.gastos.totais.por.estado,
                                  by = c("estado"))

gastos.totais.por.estado <- merge(gastos.totais.por.estado,
                                  sd.gastos.totais.por.estado,
                                  by = c("estado"))

gastos.totais.por.estado


# Ordenando os estados pela média de gastos
gastos.totais.por.estado$estado <- reorder(gastos.totais.por.estado$estado,
                                           -gastos.totais.por.estado$media,
                                           order = TRUE)

png(filename = "output-questao3-gastos-por-estado-media-gastos.png", width = 960, height = 480)
ggplot(gastos.totais.por.estado, aes(estado, media, fill = estado)) +
    geom_bar() +
    geom_errorbar(aes(ymin = ic[, 1], ymax = ic[, 2], width = 0.2)) +
    scale_x_discrete(name = "Estado") +
    scale_y_continuous(name = "Média de gastos") +
    opts(legend.position = "none",
         title = "Média de gastos dos deputados por estado em 2011")
dev.off()


# Ordenando os estados pela quantidade de deputados
gastos.totais.por.estado$estado <- reorder(gastos.totais.por.estado$estado,
                                           -gastos.totais.por.estado$n,
                                           order = TRUE)

png(filename = "output-questao3-gastos-por-estado-n-deputados.png", width = 960, height = 480)
ggplot(gastos.totais.por.estado, aes(estado, 1000 * n, fill = estado)) + # Multiplicando por 1000 apenas para que a
                                                                         # quantidade de deputados fique visível no
                                                                         # gráfico
    geom_bar() +
    geom_errorbar(aes(ymin = 0, ymax = ic[, 2] - ic[, 1], width = 0.2)) +
    scale_x_discrete(name = "Estado") +
    scale_y_continuous(name = "1000 * n") +
    opts(legend.position = "none",
    title = "Quantidade de deputados e amplitude do intervalo de confiança")
dev.off()


png(filename = "output-questao3-gastos-por-estado-desvio-padrao.png", width = 960, height = 480)
ggplot(gastos.totais.por.estado, aes(estado, desvio.padrao, fill = estado)) +
    geom_bar() +
    geom_errorbar(aes(ymin = 0, ymax = ic[, 2] - ic[, 1], width = 0.2)) +
    scale_x_discrete(name = "Estado") +
    scale_y_continuous(name = "Desvio padrão") +
    opts(legend.position = "none",
    title = "Desvio padrão e amplitude do intervalo de confiança")
dev.off()


# Quais estados gastam menos que o AP?
gastos.totais.por.estado[gastos.totais.por.estado$ic[, 2] < 123808.80, ]
