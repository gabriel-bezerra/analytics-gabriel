require(plyr)
require(ggplot2)

source("pre-treinamento.R")

models.2.camadas.escondidas <- list(list(size = c(1,1), maxit = 300, learnFuncParams = 0.3),
                                    list(size = c(2,2), maxit = 300, learnFuncParams = 0.3),
                                    list(size = c(3,3), maxit = 300, learnFuncParams = 0.3),
                                    list(size = c(4,4), maxit = 300, learnFuncParams = 0.3),
                                    list(size = c(5,5), maxit = 300, learnFuncParams = 0.3),
                                    list(size = c(6,6), maxit = 300, learnFuncParams = 0.3),
                                    list(size = c(7,7), maxit = 300, learnFuncParams = 0.3))

models = models.2.camadas.escondidas


# Melhor modelo MLP escolhido a partir da questão 2
print("MLP 1 camada")
mlp.1.camada.model = model.arguments.and.performance(list(size = 0,  maxit = 100, learnFuncParams = 0.3), mlpModel)
print(mlp.1.camada.model)

print("MLP")
mlp.models = ldply(models, model.arguments.and.performance, mlpModel)
print(mlp.models)

print("Elman")
elman.models = ldply(models, model.arguments.and.performance, elmanModel)
print(elman.models)


# 1 entrada

dados.de.treinamento.1.entrada <- splitForTrainingAndTest(dados.de.venda[,3],
                                                          dados.de.venda[,4])
print("MLP 1 camada 1 entrada")
mlp.1.camada.1.entrada.model = model.arguments.and.performance(list(size = 0,  maxit = 100, learnFuncParams = 0.3),
                                                               mlpModel, dados.de.treinamento.1.entrada)
print(mlp.1.camada.1.entrada.model)

print("MLP 1 entrada")
mlp.1.entrada.models = ldply(models, model.arguments.and.performance, mlpModel, dados.de.treinamento.1.entrada)
print(mlp.1.entrada.models)

print("Elman 1 entrada")
elman.1.entrada.models = ldply(models, model.arguments.and.performance, elmanModel, dados.de.treinamento.1.entrada)
print(elman.1.entrada.models)


png(filename = "questao4-rmse.png")
ggplot() +
    geom_hline(aes(yintercept=mlp.1.camada.model$rmse, colour = "MLP 1 camada")) +
    geom_hline(aes(yintercept=mlp.1.camada.1.entrada.model$rmse, colour = "MLP 1 c, 1 e")) +

    geom_line(aes(x = mlp.models$size, y = mlp.models$rmse, colour = "MLP")) +
    geom_line(aes(x = elman.models$size, y = elman.models$rmse, colour = "Elman")) +

    geom_line(aes(x = mlp.1.entrada.models$size, y = mlp.1.entrada.models$rmse, colour = "MLP 1 entrada")) +
    geom_line(aes(x = elman.1.entrada.models$size, y = elman.1.entrada.models$rmse, colour = "Elman 1 entrada")) +

    ylim(limits = c(0.07, min(0.2, max(rbind(mlp.1.camada.model$rmse,
                                             mlp.1.camada.1.entrada.model$rmse,
                                             mlp.models$rmse,
                                             elman.models$rmse,
                                             mlp.1.entrada.models$rmse,
                                             elman.1.entrada.models$rmse))))) +

    ylab("RMSE") +
    xlab("Nº de neurônios em cada camada escondida") +
    scale_colour_discrete(name = "Tipo de rede")
dev.off()

