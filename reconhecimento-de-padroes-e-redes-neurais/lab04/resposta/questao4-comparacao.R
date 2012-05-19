require(plyr)
require(ggplot2)

source("pre-treinamento.R")

# Comparação das redes com 1 e 2 camadas escondidas

mlp.1.camada.escondida <- list(size = 3,  maxit = 100, learnFuncParams = 0.3)
elman.1.camada.escondida <- list(size = 1,  maxit = 100, learnFuncParams = 0.3)

mlp.2.camadas.escondidas <- list(size = c(2,2), maxit = 300, learnFuncParams = 0.3)
elman.2.camadas.escondidas <- list(size = c(2,2), maxit = 300, learnFuncParams = 0.3)

n.repeticoes = 15

print("MLP 1 camada escondida")
models.mlp.1.camada.escondida = rdply(n.repeticoes, model.arguments.and.performance(mlp.1.camada.escondida, mlpModel))
print(models.mlp.1.camada.escondida)

print("Elman 1 camada escondida")
models.elman.1.camada.escondida = rdply(n.repeticoes, model.arguments.and.performance(elman.1.camada.escondida, elmanModel))
print(models.elman.1.camada.escondida)

# Aqui se aplica unique porque o size = c(2,2) faz com que apareçam 2 linhas de resultado no data.frame. Isso não
# atrapalha os resultados gerados e coletados
print("MLP 2 camadas escondidas")
models.mlp.2.camadas.escondidas = rdply(n.repeticoes, unique(model.arguments.and.performance(mlp.2.camadas.escondidas, mlpModel)))
print(models.mlp.2.camadas.escondidas)

print("Elman 2 camadas escondidas")
models.elman.2.camadas.escondidas = rdply(n.repeticoes, unique(model.arguments.and.performance(elman.2.camadas.escondidas, elmanModel)))
print(models.elman.2.camadas.escondidas)


# Teste de hipótese de igualdade

significancia = 0.05

teste <- function(rmse.uma.camada.escondida, rmse.duas.camadas.escondidas) {
    wilcox.test(rmse.uma.camada.escondida,
                rmse.duas.camadas.escondidas,
                alternative = "greater",
                paired = FALSE,
                var.equal = FALSE,
                conf.level = 1 - significancia,
                conf.int = TRUE)
}


# RMSE
print("RMSE")

hist(models.mlp.1.camada.escondida$rmse, breaks="FD")
hist(models.mlp.2.camadas.escondidas$rmse, breaks="FD")
hist(models.elman.1.camada.escondida$rmse, breaks="FD")
hist(models.elman.2.camadas.escondidas$rmse, breaks="FD")

hist(sqrt(models.mlp.1.camada.escondida$rmse), breaks="FD")
hist(sqrt(models.mlp.2.camadas.escondidas$rmse), breaks="FD")
hist(sqrt(models.elman.1.camada.escondida$rmse), breaks="FD")
hist(sqrt(models.elman.2.camadas.escondidas$rmse), breaks="FD")

hist(log(models.mlp.1.camada.escondida$rmse), breaks="FD")
hist(log(models.mlp.2.camadas.escondidas$rmse), breaks="FD")
hist(log(models.elman.1.camada.escondida$rmse), breaks="FD")
hist(log(models.elman.2.camadas.escondidas$rmse), breaks="FD")

# bruto
teste(models.mlp.1.camada.escondida$rmse,
      models.mlp.2.camadas.escondidas$rmse)

teste(models.elman.1.camada.escondida$rmse,
      models.elman.2.camadas.escondidas$rmse)

# sqrt
print("Sqrt")
teste(sqrt(models.mlp.1.camada.escondida$rmse),
      sqrt(models.mlp.2.camadas.escondidas$rmse))

teste(sqrt(models.elman.1.camada.escondida$rmse),
      sqrt(models.elman.2.camadas.escondidas$rmse))

# log
print("Log")
teste(log(models.mlp.1.camada.escondida$rmse),
      log(models.mlp.2.camadas.escondidas$rmse))

teste(log(models.elman.1.camada.escondida$rmse),
      log(models.elman.2.camadas.escondidas$rmse))


# SSE
print("SSE")

hist(models.mlp.1.camada.escondida$sse, breaks="FD")
hist(models.mlp.2.camadas.escondidas$sse, breaks="FD")
hist(models.elman.1.camada.escondida$sse, breaks="FD")
hist(models.elman.2.camadas.escondidas$sse, breaks="FD")

hist(sqrt(models.mlp.1.camada.escondida$sse), breaks="FD")
hist(sqrt(models.mlp.2.camadas.escondidas$sse), breaks="FD")
hist(sqrt(models.elman.1.camada.escondida$sse), breaks="FD")
hist(sqrt(models.elman.2.camadas.escondidas$sse), breaks="FD")

hist(log(models.mlp.1.camada.escondida$sse), breaks="FD")
hist(log(models.mlp.2.camadas.escondidas$sse), breaks="FD")
hist(log(models.elman.1.camada.escondida$sse), breaks="FD")
hist(log(models.elman.2.camadas.escondidas$sse), breaks="FD")

# bruto
teste(models.mlp.1.camada.escondida$sse,
      models.mlp.2.camadas.escondidas$sse)

teste(models.elman.1.camada.escondida$sse,
      models.elman.2.camadas.escondidas$sse)

# sqrt
print("Sqrt")
teste(sqrt(models.mlp.1.camada.escondida$sse),
      sqrt(models.mlp.2.camadas.escondidas$sse))

teste(sqrt(models.elman.1.camada.escondida$sse),
      sqrt(models.elman.2.camadas.escondidas$sse))

# log
print("Log")
teste(log(models.mlp.1.camada.escondida$sse),
      log(models.mlp.2.camadas.escondidas$sse))

teste(log(models.elman.1.camada.escondida$sse),
      log(models.elman.2.camadas.escondidas$sse))

