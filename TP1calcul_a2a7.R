
#création de data frame pour stocker les valeurs des commandes regr.eval
result.lm=data.frame()
result.rt=data.frame()

#pour a2,...a7
for (i in 2:7) {
  a=paste0("a",i) #pour que i varie entre 2 et 7
  formula <- paste0(a, " ~ ", ".") #pour créer la formule utilisée dans les deux modèles
  #on applique ensuite la même procédure que pour a1 dans la question précédente
  lm <- lm(as.formula(formula), data=algae[,c(1:11,i+11)])
  rt= rpart(as.formula(formula), data=algae[,c(1:11,i+11)])
  final=step(lm)
  lm.predictions.a=predict(final,test.algae)
  rt.predictions.a=predict(rt,test.algae)
  result.lm=rbind(result.lm,c(regr.eval(algae.sols[, a], lm.predictions.a, train.y = algae[,a])))
  result.rt=rbind(result.rt,c(regr.eval(algae.sols[, a], rt.predictions.a, train.y = algae[,a])))
}

#Donner des noms aux colonnes des data frame
colnames(result.lm)=list("mae","mse","rmse","mape","nmse","nmae")
colnames(result.rt)=list("mae","mse","rmse","mape","nmse","nmae")

# Afficher les résultats
print(result.lm)
print(result.rt)