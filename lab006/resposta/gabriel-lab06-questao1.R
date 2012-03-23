require("ggplot2")

dados <- read.table(file = "velocidade-isp.txt", header = TRUE)
dados.pb <- dados[dados$estado == "PB", ]

## 1. Disseram a Sidnelson que na prática os dois provedores da Paraíba possuem, em média, a
## mesma velocidade de conexão para o plano de 50Mbps. Verifique se o que dizem realmente é
## verdade, usando as medições feitas de hora em hora para os provedores “GZT” e “Ola” da Paraíba
## (estado == “PB”), que foram realizadas no mês de Dezembro de 2011.
## Discuta se existe diferença estatística entre as médias de velocidade de conexão dos dois
## provedores. Caso exista diferença, qual provedor você indicaria a Sidnelson? Faça uma análise
## para diferentes níveis de significância.

dados.pb.gzt <- dados.pb[dados.pb$provedor == "GZT", ]
dados.pb.ola <- dados.pb[dados.pb$provedor == "Ola", ]

source("intervalos-de-confianca.R")

intervalo.de.confianca.gzt =  intervalo.de.confianca.para.a.media(dados.pb.gzt$velocidade, 0.05)
intervalo.de.confianca.ola =  intervalo.de.confianca.para.a.media(dados.pb.ola$velocidade, 0.05)

media = c(mean(dados.pb.gzt$velocidade),
          mean(dados.pb.ola$velocidade))

ics = rbind(intervalo.de.confianca.gzt,
            intervalo.de.confianca.ola)

media.icmin.icmax = data.frame(provedor = c("GZT", "Ola"),
                               media = media,
                               ic.min = ics[, 1],
                               ic.max = ics[, 2])

grafico <- ggplot(media.icmin.icmax, aes(x = provedor,
                                         y = media,
                                         ymin = ic.min,
                                         ymax = ic.max,
                                         fill = provedor)) +
           geom_bar() +
           geom_errorbar(aes(width = 0.2)) +
           opts(title = "Intervalos de confiança para a média de velocidade dos provedores",
                legend.position = "none") +
           ylab("Média") +
           xlab("Provedor")

png(file = "output-questao1.png")
print(grafico)
dev.off()


# A comparação dos intervalos de confiança para as médias não foi conclusiva, há sobreposição de intervalos, porém
# nenhuma das médias pertence ao intervalo de confiança da média da outra operadora. Sendo assim, não podemos concluir
# que sejam diferentes nem que sejam iguais.

# Para comparar as médias de duas amostras pareadas, como é o caso deste exercício, utiliza-se o teste t de Student.
# Suas premissas são:
#    Normalidade dos dados; e
#    Homoscedasticidade.

# Verificando as premissas...

# ...Normalidade
normalidade.gzt <- shapiro.test(dados.pb.gzt$velocidade)
normalidade.ola <- shapiro.test(dados.pb.ola$velocidade)
normalidade.gzt
normalidade.ola

# ...Homoscedasticidade
homoscedasticidade <- bartlett.test(list(dados.pb.gzt$velocidade, dados.pb.ola$velocidade))
homoscedasticidade


# Teste de igualdade de médias
igualdade.de.medias <- t.test(dados.pb.gzt$velocidade, dados.pb.ola$velocidade,
                              paired = TRUE,
                              var.equal = TRUE) # Como o teste de Bartlett forneceu um p-value alto (> 0.7),
                                                # podemos assumir que a variância é a mesma.

source("linha-de-resultados.R")

resultados <- rbind(linha.de.resultados.para(normalidade.gzt),
                    linha.de.resultados.para(normalidade.ola),
                    linha.de.resultados.para(homoscedasticidade),
                    linha.de.resultados.para(igualdade.de.medias))

write.table(resultados, file = "output-questao1.txt")
