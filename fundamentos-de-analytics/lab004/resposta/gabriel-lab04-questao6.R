library("ggplot2")

source("intervalos-de-confianca.R")

## 6. Encontre o estado A do Brasil onde os deputados federais gastam mais em
## média. Encontre também o estado B do Brasil onde os deputados federais
## gastam menos, em média. É possível afirmar com 95% de confiança que o
## deputado menos gastador de A gasta mais que o deputado mais gastador de B?

dados <- read.csv(file = "dados-deputados.csv")

nivel.de.significancia = 0.05


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

# Gráfico
# Ordenando os estados pela média de gastos
gastos.totais.por.estado$estado <- reorder(gastos.totais.por.estado$estado,
                                           -gastos.totais.por.estado$media,
                                           order = TRUE)

png(filename = "output-questao6-gastos-por-estado-media-gastos.png", width = 960, height = 480)
ggplot(gastos.totais.por.estado, aes(estado, media, fill = estado)) +
    geom_bar() +
    geom_errorbar(aes(ymin = ic[, 1], ymax = ic[, 2], width = 0.2)) +
    scale_x_discrete(name = "Estado") +
    scale_y_continuous(name = "Média de gastos") +
    opts(legend.position = "none",
         title = "Média de gastos dos deputados por estado em 2011")
dev.off()


# Análise dos deputados extremos do estado de maior gasto (mínimo) e do estado de menor gasto (máximo)

# Identificação dos deputados
estado.maior.gasto = with(gastos.totais.por.estado,
                             gastos.totais.por.estado[media == max(media),]$estado)
estado.menor.gasto = with(gastos.totais.por.estado,
                             gastos.totais.por.estado[media == min(media),]$estado)

deputados.estado.maior.gasto = with(dados,
                                    dados[estado == as.character(estado.maior.gasto), ])
deputados.estado.menor.gasto = with(dados,
                                    dados[estado == as.character(estado.menor.gasto), ])

deputado.menor.gasto.estado.maior.gasto =
    with(deputados.estado.maior.gasto,
         deputados.estado.maior.gasto[gastos.total == min(gastos.total), ])
deputado.maior.gasto.estado.menor.gasto =
    with(deputados.estado.menor.gasto,
         deputados.estado.menor.gasto[gastos.total == max(gastos.total), ])


# Comparação direta através dos gastos dos deputados

print(paste("Estado de maior gasto:", deputado.menor.gasto.estado.maior.gasto$estado))
print(paste("Seu deputado de menor gasto:", deputado.menor.gasto.estado.maior.gasto$nome))
print(paste("Gastou", deputado.menor.gasto.estado.maior.gasto$gastos.total))

print(paste("Estado de maior gasto:", deputado.maior.gasto.estado.menor.gasto$estado))
print(paste("Seu deputado de maior gasto:", deputado.maior.gasto.estado.menor.gasto$nome))
print(paste("Gastou", deputado.maior.gasto.estado.menor.gasto$gastos.total))

