dados <- read.table(file = "velocidade-isp.txt", header = TRUE)
dados.pb <- dados[dados$estado == "PB", ]

## 2. Lendo os contratos dos provedores “GZT” e “Ola” da Paraíba, Sidnelson viu que ambos
## prometem que a média mensal da conexão será sempre maior ou igual a 50Mbps. Verifique se esta
## promessa se confirma, usando as medições feitas de hora em hora para os dois provedores da
## Paraíba (estado == “PB”).

dados.pb.gzt <- dados.pb[dados.pb$provedor == "GZT", ]
dados.pb.ola <- dados.pb[dados.pb$provedor == "Ola", ]


t.test(dados.pb.gzt$velocidade,
       mu = 50,
       alternative = "less")

t.test(dados.pb.ola$velocidade,
       mu = 50,
       alternative = "less")
