## Welcome to GitHub Pages

You can use the [editor on GitHub](https://github.com/amanbagla/Study/edit/master/README.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for
---
title: "Project"
author: "Aman Bagla"
date: "26 March 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown


```{r}
library(ggplot2)
library('maps')
library(countrycode)
library(corrplot)

world <- map_data('world')
world$codes <- countrycode(world$region, 'country.name', 'iso3c')
world$sq <- seq(1:99338)

setwd("C:/Users/amanb/Desktop/Predictive Project")
GDP <- read.csv("GDP Per Capita.csv")
Emission <- read.csv("Carbon Dioxide Emission Estimates.csv")
Education <- read.csv("Education.csv")
Employment <- read.csv("employment.csv")
Fertility <- read.csv("Fertility and Mortality Indicators.csv")
Growth_rate <- read.csv("Population Growth Rate.csv")
Unemployment <- read.csv("Unemployment.csv")
Geography <- read.csv("Climate_geography.csv")
Birth_rate <- read.csv("BirthRate_per1000.csv")
Death_rate <- read.csv("DeathRate_per1000.csv")
Density <- read.csv("Density.csv")
Religion <- read.csv("Religion.csv")

GDP$codes <- countrycode(GDP$Country, 'country.name', 'iso3c')
Emission$codes <- countrycode(Emission$Country, 'country.name', 'iso3c')
Education$codes <- countrycode(Education$Country, 'country.name', 'iso3c')
Employment$codes <- countrycode(Employment$Country, 'country.name', 'iso3c')
Fertility$codes <- countrycode(Fertility$Country, 'country.name', 'iso3c')
Growth_rate$codes <- countrycode(Growth_rate$Country, 'country.name', 'iso3c')
Unemployment$codes <- countrycode(Unemployment$Country, 'country.name', 'iso3c')
Geography$codes <- countrycode(Geography$Country, 'country.name', 'iso3c')
Density$codes <- countrycode(Density$Country, 'country.name', 'iso3c')
Religion$codes <- countrycode(Religion$Country, 'country.name', 'iso3c')



Final_data <- merge(world,GDP, by="codes",all.x = TRUE)
Final_data <- merge(Final_data,Emission, by="codes",all.x = TRUE)
Final_data <- merge(Final_data,Education, by="codes",all.x = TRUE)
Final_data <- merge(Final_data,Employment, by="codes",all.x = TRUE)
Final_data <- merge(Final_data,Fertility, by="codes",all.x = TRUE)
Final_data <- merge(Final_data,Growth_rate, by="codes",all.x = TRUE)
Final_data <- merge(Final_data,Unemployment, by="codes",all.x = TRUE)
Final_data <- merge(Final_data,Geography, by="codes",all.x = TRUE)
Final_data <- merge(Final_data,Death_rate, by.x="codes",by.y = "Country.Code", all.x = TRUE)
Final_data <- merge(Final_data,Birth_rate, by="codes",by.y = "Country.Code", all.x = TRUE)
Final_data <- merge(Final_data,Density, by="codes",all.x = TRUE)
Final_data <- merge(Final_data,Religion, by="codes",all.x = TRUE)
Final_data[,which(names(Final_data)=="Country.y")] <- NULL
Final_data[,which(names(Final_data)=="Country.x")] <- NULL
Final_data[,which(names(Final_data)=="Country")] <- NULL
Final_data$isocode <- NULL
Final_data$isonum <- NULL
Final_data$order <- NULL
Final_data$subregion <- NULL

#f <- Final_data[complete.cases(Final_data),]

target <- 75
Final_data$Average.Population.Density..per.sqr.Km. <- sapply(Final_data$Average.Population.Density..per.sqr.Km., function(x) { return((x-target)/target)})
Final_data$response <- Final_data$Average.Population.Density..per.sqr.Km. * Final_data$Population.average.annual.rate.of.increase..percent.

Final_data <- Final_data[order(Final_data$sq, decreasing = FALSE),]
col <- function(x) { if (is.na(x)) { return("NA")} else {    if (x<5) {return(x) } else { return(5) } }}
Final_data$col_resp <- as.numeric(sapply(Final_data$response, col))

Final_data$Category <- sapply(Final_data[,37], function(x) { if (is.na(x)) { return("NA")} else {    if (x>=0) {return(1) } else { return(2) } }})

mynames <- c("codes", "long", "lat", "group", "region", "sq","GDP_avg", "Emissions_avg", "No.Education.AVG.%",
             "Primary.Education.AVG.%", "Secondary.or.Higher.Edu.AVG.%", "Unknown.Education.AVG.%",
             "Employment.Agriculture_avg", "Employment.Industry_avg", "Employment.Services_avg", "Infant.mortality.rate_avg", 
             "Life.expectancy_avg","Fertility.rate_avg", "Pop.Rate", "Unemployment.rate_avg", "%.Mountaineous.terrain",
             "%.Fertile.Soil", "%.Desert", "%.Tropical.region", "%.Coastal.region", "Death.Rate_avg", "Birth.Rate_avg",
             "Pop.density", "Christian.%", "Muslim.%", "Atheist.%", "Hindu.%", "Buddhist.%", "Folk.Religion.%", 
             "Other.Religion.%", "Jewish.%","pcri", "PCRI","Category") 

names(Final_data) <- mynames
#Final_data[,c(27,39)] <- NULL
Final_data_updated <- Final_data[which(!is.na(Final_data$codes)),]
Short_data <- Final_data_updated[!duplicated(Final_data_updated$region),] 
Short_data_final <- Short_data[complete.cases(Short_data) ,]
set.seed(12)
test_row <- sample(1:143,28)
Short_data_final_train <- Short_data_final[-test_row,-c(1:6,18,27,37:40)]
Short_data_final_test <- Short_data_final[test_row,-c(1:6,18,27,37:40)]

ggplot(data=Final_data_updated) + geom_polygon(aes(x=long, y=lat, group = group, fill=PCRI), color = 'black') + scale_fill_gradient(low = "green", high = "blue", limits=c(-5, 5)) + 
  ggtitle("Heat Map - Population Control Requirement Index (PCRI)") + 
  theme(legend.position = c(0.05,0.5), plot.title = element_text(hjust = 0.5), axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

```


```{r}
# Corrplot

f <- Short_data_final_train
f <- f[complete.cases(f),]
png(height=1200, width=1500, pointsize=15)
corrplot(cor(f), order = "AOE", tl.cex = 0.7)
```


```{r}
# Violin Plots
brk <- quantile(Short_data_final_train$Pop.Rate)
Short_data_final_train$Pop_Growth_Rate_Levels <- cut(Short_data_final_train$Pop.Rate, 
                                              breaks = c(brk[1]-1, brk[2:5]), 
                           labels = c("1st Quartile", "2nd Quartile","3rd Quartile", "4th Quartile"))
for (i in 1:1) {
  print(ggplot(data=Short_data_final_train, aes(x=Pop_Growth_Rate_Levels, y=Short_data_final_train[,i])) +
          geom_violin(aes(fill=Pop_Growth_Rate_Levels)) + ylab(names(Short_data_final_train[i])) + 
          theme(legend.position = "none", plot.title = element_text(hjust = 0.5), text = element_text(size = 15)) + 
          xlab("Population Growth Rate") + geom_boxplot(width=0.1) +
          ggtitle(paste("Population Growth Rate vs",names(Short_data_final_train)[i])))
}
Short_data_final_train$Pop_Growth_Rate_Levels <- NULL

```



```{r}
# Linear Model
RMSE <- function (fitted_val,actual_val) {
  rmse_val <- sqrt(sum((actual_val-fitted_val)^2)/length(actual_val))
  return(rmse_val)
}

model_lm <- lm(data=Short_data_final_train, Pop.Rate~.)
fitted_train_lm <- predict(model_lm, newdata = Short_data_final_train[,-12])
fitted_test_lm <- predict(model_lm, newdata = Short_data_final_test[,-12])
rmse_train_lm <- RMSE(fitted_train_lm, Short_data_final_train[,12])
rmse_test_lm <- RMSE(fitted_test_lm, Short_data_final_test[,12])
#plot(fitted_lm, Short_data_final[,12])
```


```{r}
# Ridge
library(glmnet)

set.seed(1234)
row_fold_number <- sample(rep(1:5, each = 23), 115)
model_ridge_ <- cv.glmnet(x = as.matrix(Short_data_final_train[,-12]),
                      y = as.matrix(Short_data_final_train[,12]), standardize = TRUE, 
                                foldid = row_fold_number, alpha = 0, nfold = 5)
lambda.min.ridge <- model_ridge_$lambda.min

plot.cv.glmnet(model_ridge_, cex.lab=1.5) 
title(main = "Tuning Lambda For Ridge", cex.main =2,  outer = TRUE, line=-1.8)

model_ridge <- glmnet(x = as.matrix(Short_data_final_train[,-12]),
                      y = as.matrix(Short_data_final_train[,12]), standardize = TRUE, 
                                alpha = 0, lambda = lambda.min.ridge)
fitted_train_ridge <- predict(model_ridge, newx = as.matrix(Short_data_final_train[,-12]))
fitted_test_ridge <- predict(model_ridge, newx = as.matrix(Short_data_final_test[,-12]))
rmse_train_ridge <- RMSE(fitted_train_ridge, Short_data_final_train[,12])
rmse_test_ridge <- RMSE(fitted_test_ridge[], Short_data_final_test[,12])
#plot(fitted_ridge, Short_data_final[,19])
```

```{r}
# Lasso
set.seed(123)
model_lasso_ <- cv.glmnet(x = as.matrix(Short_data_final_train[,-12]),
                      y = as.matrix(Short_data_final_train[,12]), standardize = TRUE, 
                                foldid = row_fold_number, alpha = 1, nfold = 5)
lambda.min.lasso <- model_lasso_$lambda.min

plot.cv.glmnet(model_lasso_) 
title(main = "Tuning Lambda For Lasso", cex.main =2,  outer = TRUE, line=-1.8)

model_lasso <- glmnet(x = as.matrix(Short_data_final_train[,-12]),
                      y = as.matrix(Short_data_final_train[,12]), standardize = TRUE, 
                                alpha = 1, lambda = lambda.min.lasso)
fitted_train_lasso <- predict(model_lasso, newx = as.matrix(Short_data_final_train[,-12]))
fitted_test_lasso <- predict(model_lasso, newx = as.matrix(Short_data_final_test[,-12]))
rmse_train_lasso <- RMSE(fitted_train_lasso, Short_data_final_train[,12])
rmse_test_lasso <- RMSE(fitted_test_lasso, Short_data_final_test[,12])

#plot(fitted_lasso, Short_data_final[,19])
```

```{r}
# GAM

library(gam)

set.seed(1)
data_gam_train <- Short_data_final_train
names(data_gam_train)<-make.names(names(data_gam_train))
gam.fit<- gam(Pop.Rate~ 1 + s(GDP_avg)+s(Primary.Education.AVG..)+ Employment.Agriculture_avg +
                s(Infant.mortality.rate_avg)+s(Unemployment.rate_avg)+s(X..Desert)+s(Death.Rate_avg)+s(Muslim..)+s(Buddhist..)+
                s(Jewish..)+s(Emissions_avg)+s(Secondary.or.Higher.Edu.AVG..)+Employment.Industry_avg+
                s(Life.expectancy_avg)+s(X..Mountaineous.terrain)+s(X..Tropical.region)+s(Atheist..)+s(Folk.Religion..)+
                s(No.Education.AVG..)+s(Unknown.Education.AVG..)+Employment.Services_avg +s(X..Fertile.Soil)+s(X..Coastal.region)+
                s(Christian..)+s(Hindu..)+s(Other.Religion..),data=data_gam_train)

data_gam_test <- Short_data_final_test
names(data_gam_test)<-make.names(names(data_gam_test))

fitted_train_gam <- predict.Gam(gam.fit, newdata = data_gam_train[,-12])
fitted_test_gam <- predict.Gam(gam.fit, newdata = data_gam_test[,-12])
rmse_train_gam <- RMSE(fitted_train_gam, data_gam_train[,12])
rmse_test_gam <- RMSE(fitted_test_gam[-13], data_gam_test[-13,12])
```

```{r}
# MARS 

library(earth)

set.seed(1)

  model_MARS <-earth(Pop.Rate~.,data=Short_data_final_train, nfold = 5, pmethod = "cv");
  
  plot.earth.models(model_MARS, which = 1, col.vline = "red")
  
  fitted_train_mars <- predict(model_MARS, newdata=Short_data_final_train[,-12])
  fitted_test_mars <- predict(model_MARS, newdata=Short_data_final_test[,-12])
  rmse_train_MARS <- RMSE(fitted_train_mars, Short_data_final_train[,12])
  rmse_test_MARS <- RMSE(fitted_test_mars, Short_data_final_test[,12])

```

```{r}
# Random Forest

library(randomForest)

Short_data_final.train <- Short_data_final_train
Short_data_final.test <- Short_data_final_test

names(Short_data_final.train) <- make.names(names(Short_data_final.train))
names(Short_data_final.test) <- make.names(names(Short_data_final.test))
oob.err = matrix(0,5,27)

for (mtry in 1:27) {
  for (i in 1:5) {
    set.seed(1)
    model_rf <- randomForest(Pop.Rate~., data = Short_data_final.train[!row_fold_number==i,], 
                             mtry=mtry,ntree=500)
    oob.err[i,mtry]=model_rf$mse[500]
  }
}

oob.err.avg <- apply(oob.err, 2, mean)

matplot(1:mtry, oob.err.avg, pch=19, col = "red", type = "b", ylab = "Mean Squared Error", 
        xlab = "Number of Predictor Sampled for Splitting at Each Node (mtry)", 
        main = "Selection of Tuning Parameter (mtry)", cex.main =1.8, cex.lab = 1.4)
legend("topright", legend = "OOB", pch=19, col = "red")


#mtry = 15 best oob error
model_rf_tuned <- randomForest(Pop.Rate~., data = Short_data_final.train, mtry=which.min(oob.err.avg), ntree=500 )

#Training set error
fitted_train_rf <- predict(model_rf_tuned, newdata = Short_data_final.train)
rmse_train_rf <- RMSE(fitted_train_rf,Short_data_final.train$Pop.Rate)
#Test set error
fitted_test_rf <- predict(model_rf_tuned, newdata = Short_data_final.test)
rmse_test_rf <- RMSE(fitted_test_rf, Short_data_final.test$Pop.Rate)
```


```{r}
# BART

options(java.parameters = "-Xmx12g") 
library(rJava)
library('bartMachine')

rmse_bart_ <- matrix(0,5,18)
parameter <- matrix(0,4,18)
z <- 1
nu <- c(3,3,10)
q <- c(0.9, 0.99, 0.75)
k <- c(1,2,3)
tree <- c(50,100)

for (tr in 1:2) {
  for (rr in 1:3) {
    for (j in 1:3) {
      for (i in 1:5) {
        model_bart_ <- bartMachine(X= Short_data_final_train[!(row_fold_number==i),-12], 
                            y= Short_data_final_train[!(row_fold_number==i),12], k = k[rr], q = q[j], nu = nu[j], 
                            num_trees = tree[tr], seed = 10)
        fitted_val_oos <- predict(model_bart_, new_data = Short_data_final_train[(row_fold_number==i),-12])
        rmse_bart_[i,z] <- RMSE(fitted_val_oos, Short_data_final_train[(row_fold_number==i),12])  
      }
      parameter[,z] <- c(k[rr], q[j], nu[j], tree[tr])
      z <- z+1
    }
  }
}


rmse_bart_avg <- colSums(rmse_bart_)/5
parameter <- rbind(parameter, rmse_bart_avg)
parameter.df <- as.data.frame(t(parameter))
names(parameter.df) <- c("k", "q", "nu", "m", "rmse")
parameter.df[,"(q,nu)"] <- paste("(",q,",",nu,")")
parameter.df[,"(q,nu)"] <- as.factor(parameter.df[,"(q,nu)"])
for (i in 1:4) {
  parameter.df[,i] <- as.factor(parameter.df[,i])
}

ggplot(data= parameter.df, group = m) + geom_point(aes(x=parameter.df[,"(q,nu)"], y = rmse, col = k, shape=m), size = 5) + 
  ggtitle("Tuning Hyperparameters (k, q, nu, m) for BARTMachine") + ylab("RMSE") + 
          theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 17)) + 
          xlab("(q, nu)")

plot(rmse_bart_avg)
par <- parameter[,which.min(rmse_bart_avg)]

model_bart_final <- bartMachine(X= Short_data_final_train[,-12], y= Short_data_final_train[,12], k=par[1], q=par[2], nu=par[3], num_trees = par[4], seed=10)

fitted_train_bart <- predict(model_bart_final, new_data = Short_data_final_train[,-12])
fitted_test_bart <- predict(model_bart_final, new_data = Short_data_final_test[,-12])
rmse_train_bart <- RMSE(fitted_train_bart, Short_data_final_train[,12])
rmse_test_bart <- RMSE(fitted_test_bart, Short_data_final_test[,12])
set.seed(10)
investigate_var_importance(model_bart_final)
```

```{r}
# SVM

library(e1071)

set.seed(10)
model_svr <- svm(data=Short_data_final_train, Pop.Rate~.,kernel="polynomial", cost=1)

tune.out = tune(svm, data=Short_data_final_train, Pop.Rate~.,
                ranges=list(epsilon=seq(0,1,0.1), cost=seq(1,20,1)))

plot(tune.out, main = "Tuning Hyperameters for SVM", cex.main = 1.8, cex.lab =1.5 )

model_svr_final <- svm(data=Short_data_final_train, Pop.Rate~.,kernel="polynomial", 
                       cost=tune.out$best.parameters[2], epsilon =tune.out$best.parameters[1])
fitted_train_svr <- predict(model_svr_final, newdata = Short_data_final_train[,-12])
fitted_test_svr <- predict(model_svr_final, newdata = Short_data_final_test[,-12])
rmse_train_svm <- RMSE(fitted_train_svr, Short_data_final_train[,12])
rmse_test_svm <- RMSE(fitted_test_svr[], Short_data_final_test[,12])
# plot(x=Short_data_final[,19], y=predict(model_svr_final, Short_data_final[,-c(1:6,18,19,27,37:40)]))

```

```{r}
# Gradient Boosting

library(gbm)
library(plyr)

set.seed(10) 
model_gbm_ <- gbm(Pop.Rate~., data = Short_data_final_train,  
                          distribution = "gaussian", n.trees = 20000, shrinkage = 0.01, 
                          interaction.depth = 3, cv.folds = 5) 
fitted_train_gbm <- predict(model_gbm_, n.trees = seq(from=1, to=5000, by=10)) 
fitted_test_gbm <- predict(model_gbm_, newdata = Short_data_final_test, 
                              n.trees = seq(from=1, to=5000, by=10))

rmse_train_gbm_ <- laply(as.data.frame(fitted_train_gbm), RMSE, Short_data_final_train[,12])
rmse_test_gbm_ <- laply(as.data.frame(fitted_test_gbm), RMSE, Short_data_final_test[,12])

df <- data.frame(n=seq(from=1, to=5000, by=10), traind=rmse_train_gbm_, test=rmse_test_gbm_)
ggplot(df, aes(x=n)) + geom_point(aes(y=traind), pch='o', col='red') + geom_point(aes(y=test), pch='o', col="blue") + scale_y_continuous(name="accuracy") + ggtitle("Choosing number of trees for GBM (Boosting)") + 
  ylab("RMSE") + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 17)) + 
  xlab("Number of Trees") + geom_vline(xintercept=1500,linetype=6,color="red", size = 1)

rmse_train_gbm <- df[df$n == 1501, "traind"]
rmse_test_gbm <- df[df$n == 1501, "test"]
 
```

```{r}
final_rmse <- data.frame(val=c(rmse_train_lm, rmse_train_ridge, rmse_train_lasso, rmse_train_rf, 
                           rmse_train_gam, rmse_train_gbm, rmse_train_bart, rmse_train_MARS, rmse_train_svm,
                           rmse_test_lm, rmse_test_ridge, rmse_test_lasso, rmse_test_rf, rmse_test_gam, rmse_test_gbm,
                           rmse_test_bart, rmse_test_MARS, rmse_test_svm),
                           model = rep(c("lm","Ridge","Lasso","RF","GAM", "Boosting", "BART", "MARS", "SVM"),2), 
                           type = rep(c("Train", "Test"), each=9))

ggplot(data = final_rmse) + geom_point (aes(y=val, x=model, col=type), size =4) + ggtitle("Model Comparison") + ylab("RMSE") + geom_text(aes(y=val, x=model, label=round(val,3)),hjust=-0.3, vjust=0, size=4) + xlab("") +
   theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 17)) 
```




```{r}
# Final Model Analysis
set.seed(10)
model_gbm_final <- gbm(Pop.Rate~., data = data_gam_train, distribution = "gaussian", n.trees = 1501, shrinkage = 0.01, 
                          interaction.depth = 3)
a <- summary.gbm(model_gbm_final)
a$var <- factor(a$var, levels=a$var[order(a$var)])

ggplot(data=a[1:10,], aes(x = var, y = rel.inf, fill = rel.inf)) + geom_col() + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank()) + scale_fill_gradient(high = "darkblue", low = "skyblue") + 
  geom_text(aes(label=var), size = 5, angle = 90, hjust = 0,vjust = 0) + ylim(0,25) + ggtitle("Variable Relative Influence") + 
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 17), legend.position = "none") + xlab("") + ylab("Relative Influence %")
 
for (i in c(3,18)) {
  print(plot(model_gbm_final, i=names(data_gam_train[,-12])[i], main = names(data_gam_train[,-12])[i]))
}

plot.gbm(model_gbm_final, i=names(data_gam_train[,-12])[3], main = "Percentage of Uneducated People", 
         ylab="Influence on response %" )
plot.gbm(model_gbm_final, i=names(data_gam_train[,-12])[18], main = "Death Rate %", 
         ylab="Influence on response %" )


```



```{r}
# Other Visualization
vec <- names(Final_data)
for (i in 7:8) {
  ggplot(data=Final_data)  + geom_polygon(aes(x=long, y=lat, group = group,fill=Emissions.per.capita..metric.tons.of.carbon.dioxide...2005), color = 'black') +
   scale_fill_gradient(low = "green", high = "blue")
}
```



```{r}
for (i in c(3,19)) {
  plot(Short_data$Pop.Rate~Short_data[,i], main=names(Short_data)[i])
}
```

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/amanbagla/Study/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://help.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.
