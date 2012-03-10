library("ggplot2")

## 7. Use sua criatividade para analisar os dados dos deputados como você
## desejar. Relate suas descobertas informando o seu raciocínio para chegar em
## cada conclusão.


dados <- read.csv(file = "dados-deputados.csv")

mais.gastadores.primeiro = dados[order(dados$gastos.total, decreasing = TRUE), ]
top10 = mais.gastadores.primeiro[1:10, ]
top20 = mais.gastadores.primeiro[1:20, ]

menos.gastadores.primeiro = dados[order(dados$gastos.total, decreasing = FALSE), ]
bottom10 = menos.gastadores.primeiro[1:10, ]
bottom20 = menos.gastadores.primeiro[1:20, ]

png(filename = "output-questao7-todos-os-deputados.png")
ggplot(dados, aes(regiao, fill = regiao)) +
    geom_histogram() +
    opts(title = "Todos os deputados",
         legend.position = "none")
dev.off()

# Top
png(filename = "output-questao7-top10.png")
ggplot(top10, aes(regiao, fill = regiao)) +
    geom_histogram() +
    opts(title = "Regiões dos top 10 gastadores",
         legend.position = "none")
dev.off()

png(filename = "output-questao7-top20.png")
ggplot(top20, aes(regiao, fill = regiao)) +
    geom_histogram() +
    opts(title = "Regiões dos top 20 gastadores",
         legend.position = "none")
dev.off()

# Bottom
png(filename = "output-questao7-bottom10.png")
ggplot(bottom10, aes(regiao, fill = regiao)) +
    geom_histogram() +
    opts(title = "Regiões dos bottom 10 gastadores",
         legend.position = "none")
dev.off()

png(filename = "output-questao7-bottom20.png")
ggplot(bottom20, aes(regiao, fill = regiao)) +
    geom_histogram() +
    opts(title = "Regiões dos bottom 20 gastadores",
         legend.position = "none")
dev.off()


proporcao.das.regioes.em <- function(data)
    aggregate(data$regiao,
              list(regiao = data$regiao),
              function(x) {length(x) / length(data$regiao)})

print("Top 10")
proporcao.das.regioes.em(top10)

print("Top 20")
proporcao.das.regioes.em(top20)

print("Bottom 10")
proporcao.das.regioes.em(bottom10)

print("Bottom 20")
proporcao.das.regioes.em(bottom20)

print("Todos")
proporcao.das.regioes.em(dados)

