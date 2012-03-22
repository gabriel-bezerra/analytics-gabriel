require("ggplot2")

dados <- read.table(file = "velocidade-isp.txt", header = TRUE)

dados.gzt <- dados[dados$provedor == "GZT", ]

## 3. Sidnelson também viu que no contrato da “GZT” eles dizem que a qualidade de serviço oferecida
## em todas regiões do país é a mesma para planos semelhantes, incluindo a velocidade de conexão.
## Com seus contatos nas redes sociais, Sidnelson pediu pra dois amigos, um do Rio de Janeiro e
## outro de São Paulo, fazerem a mesma medição de hora em hora na velocidade da conexão de
## seus provedores, ambos “GZT”. Porém, as medições foram feitas em meses diferentes, como
## consta nos dados.
## Verifique se é possível afirmar estatisticamente que a média da velocidade de conexão da “GZT”
## nos três estados medidos (PB, RJ e SP) realmente não possuem diferença estatística, como é dito
## no contrato da empresa. As diferenças nos valores de velocidade de conexão são variações
## normais de fatores internos, ou realmente há diferença entre a velocidade de conexão da “GZT”
## entre os estados?
## Caso seja identificada diferença de velocidade entre os estados, analise estatisticamente e
## identifique qual o estado possui a melhor conexão, discutindo como você chegou à sua conclusão.


dados.gzt.pb <- dados.gzt[dados.gzt$estado == "PB", ]
dados.gzt.sp <- dados.gzt[dados.gzt$estado == "SP", ]
dados.gzt.rj <- dados.gzt[dados.gzt$estado == "RJ", ]

source("intervalos-de-confianca.R")

intervalo.de.confianca.pb =  intervalo.de.confianca.para.a.media(dados.gzt.pb$velocidade, 0.05)
intervalo.de.confianca.sp =  intervalo.de.confianca.para.a.media(dados.gzt.sp$velocidade, 0.05)
intervalo.de.confianca.rj =  intervalo.de.confianca.para.a.media(dados.gzt.rj$velocidade, 0.05)

media = c(mean(dados.gzt.pb$velocidade),
          mean(dados.gzt.sp$velocidade),
          mean(dados.gzt.rj$velocidade))

ics = rbind(intervalo.de.confianca.pb,
            intervalo.de.confianca.sp,
            intervalo.de.confianca.rj)

media.icmin.icmax = data.frame(estado = c("PB", "SP", "RJ"),
                               media = media,
                               ic.min = ics[, 1],
                               ic.max = ics[, 2])

ggplot(media.icmin.icmax, aes(x = estado,
                              y = media,
                              ymin = ic.min,
                              ymax = ic.max,
                              fill = estado)) +
    geom_bar() +
    geom_errorbar(aes(width = 0.2)) +
    opts(title = "Intervalos de confiança para a média de velocidade do GZT",
         legend.position = "none") +
    ylab("Média") +
    xlab("Estado")


# A comparação dos intervalos de confiança leva a crer que as médias de velocidades dos estados são diferentes, pois os
# intervalos de SP e RJ  não se sobrepõem. Entretanto, para uma análise mais segura, realizamos o teste ANOVA -- o teste
# mais adequado à situação de comparação da média de duas ou mais amostras independentes (pois foram coletadas em meses
# diferentes).

# Suas premissas são:
#    Normalidade dos dados; e
#    Homoscedasticidade.

# Verificando as premissas...

# ...Normalidade
shapiro.test(dados.gzt.pb$velocidade)
shapiro.test(dados.gzt.sp$velocidade)
shapiro.test(dados.gzt.rj$velocidade)

# ...Homoscedasticidade
bartlett.test(list(dados.gzt.pb$velocidade,
                   dados.gzt.sp$velocidade,
                   dados.gzt.rj$velocidade))

# Teste ANOVA
valores.empilhados <- stack(data.frame(pb = dados.gzt.pb$velocidade,
                                       sp = dados.gzt.sp$velocidade,
                                       rj = dados.gzt.rj$velocidade))

oneway.test(values ~ ind,
            valores.empilhados,
            var.equal = TRUE) # Como o teste de Bartlett forneceu um p-value alto (> 0.4),
                              # podemos assumir que a variância é a mesma.


