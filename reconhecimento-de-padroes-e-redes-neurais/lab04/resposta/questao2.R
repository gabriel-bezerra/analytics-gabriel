source("pre-treinamento.R")

mlp.model <- mlpModel(1, 100, 0.3)

mlp.sse = ssePara(mlp.model, dados.de.treinamento)
mlp.rmse = rmsePara(mlp.model, dados.de.treinamento)

print(mlp.sse)
print(mlp.rmse)

ploteGraficosPara(mlp.model)
