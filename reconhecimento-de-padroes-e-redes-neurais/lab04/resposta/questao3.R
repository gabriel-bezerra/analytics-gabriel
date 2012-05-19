require(plyr)
require(ggplot2)

source("pre-treinamento.R")

models <- list(list(size = 1,  maxit = 100, learnFuncParams = 0.3),
               list(size = 2,  maxit = 100, learnFuncParams = 0.3),
               list(size = 3,  maxit = 100, learnFuncParams = 0.3),
               list(size = 4,  maxit = 100, learnFuncParams = 0.3),
               list(size = 5,  maxit = 100, learnFuncParams = 0.3),
               list(size = 6,  maxit = 100, learnFuncParams = 0.3),
               list(size = 7,  maxit = 100, learnFuncParams = 0.3),
               list(size = 8,  maxit = 100, learnFuncParams = 0.3),
               list(size = 9,  maxit = 100, learnFuncParams = 0.3),
               list(size = 10, maxit = 100, learnFuncParams = 0.3),
               list(size = 15, maxit = 100, learnFuncParams = 0.3),
               list(size = 20, maxit = 100, learnFuncParams = 0.3))

# 3 entradas

# Melhor modelo MLP escolhido a partir da questão anterior
print("MLP 1 camada")
mlp.1.camada.model = model.arguments.and.performance(list(size = 0,  maxit = 100, learnFuncParams = 0.3), mlpModel)
print(mlp.1.camada.model)

print("MLP")
mlp.models = ldply(models, model.arguments.and.performance, mlpModel)
print(mlp.models)

print("Elman")
elman.models = ldply(models, model.arguments.and.performance, elmanModel)
print(elman.models)

print("Jordan")
jordan.models = ldply(models, model.arguments.and.performance, jordanModel)
print(jordan.models)


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

print("Jordan 1 entrada")
jordan.1.entrada.models = ldply(models, model.arguments.and.performance, jordanModel, dados.de.treinamento.1.entrada)
print(jordan.1.entrada.models)


png(filename = "questao3-rmse.png")
ggplot() +
    geom_hline(aes(yintercept=mlp.1.camada.model$rmse, colour = "MLP 1 camada")) +
    geom_hline(aes(yintercept=mlp.1.camada.1.entrada.model$rmse, colour = "MLP 1 c, 1 e")) +

    geom_line(aes(x = mlp.models$size, y = mlp.models$rmse, colour = "MLP")) +
    geom_line(aes(x = jordan.models$size, y = jordan.models$rmse, colour = "Jordan")) +
    geom_line(aes(x = elman.models$size, y = elman.models$rmse, colour = "Elman")) +

    geom_line(aes(x = mlp.1.entrada.models$size, y = mlp.1.entrada.models$rmse, colour = "MLP 1 entrada")) +
    geom_line(aes(x = elman.1.entrada.models$size, y = elman.1.entrada.models$rmse, colour = "Elman 1 entrada")) +
    geom_line(aes(x = jordan.1.entrada.models$size, y = jordan.1.entrada.models$rmse, colour = "Jordan 1 entrada")) +

    ylim(limits = c(0.07, min(0.2, max(rbind(mlp.1.camada.model$rmse,
                                             mlp.1.camada.1.entrada.model$rmse,
                                             mlp.models$rmse,
                                             jordan.models$rmse,
                                             elman.models$rmse,
                                             mlp.1.entrada.models$rmse,
                                             elman.1.entrada.models$rmse,
                                             jordan.1.entrada.models$rmse))))) +

    ylab("RMSE") +
    xlab("Nº de neurônios na camada escondida") +
    scale_colour_discrete(name = "Tipo de rede")
dev.off()

