library(openxlsx)
obj <- roc(Label~CD8+FTH+KLH+MTC+PDK+PTG,data = data_1,levels=c('Disease','Control'),print.thres=TRUE, print.auc=TRUE)
ggroc(obj)

obj <- roc(Label~ACA+MS4+TBC,data = data_2,levels=c('Disease','Control'),print.thres=TRUE, print.auc=TRUE)
ggroc(obj)

roc(Label~CD8+FTH+KLH+MTC+PDK+PTG,data = data_1,levels=c('Disease','Control'),print.thres=TRUE, print.auc=TRUE,plot=TRUE)
roc(Label~ACA+MS4+TBC,data = data_2,levels=c('Disease','Control'),print.thres=TRUE, print.auc=TRUE,plot=TRUE)

model1 <- glm(Label~., data=data_2[,c(-1)], family='binomial')
pre <- predict(model1,type='response')
modelroc <- roc(data_2$Label,pre,levels=c('Disease','Control'),print.thres=TRUE, print.auc=TRUE,plot=TRUE)
ggroc(modelroc)

model1 <- glm(Label~., data=data_1[,c(-1)], family='binomial')
pre <- predict(model1,type='response')
modelroc <- roc(data_1$Label,pre,levels=c('Disease','Control'),print.thres=TRUE, print.auc=TRUE,plot=TRUE)
ggroc(modelroc)