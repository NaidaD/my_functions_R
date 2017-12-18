```# my_functions_R
############################
#удаляем выбросы
outliers.rm <- function(x){
  quantiles <- quantile(x, probs = c(0.25, 0.75), na.rm=T) 
  x <- x[x>(as.numeric(quantiles[1])-1.5*IQR(x, na.rm=T))&
           x<(as.numeric(quantiles[2])+1.5*IQR(x, na.rm=T))] 
  print(x)
} 

###############################


read_all_files <- function(){
  counter <- 0
  combined <- data.frame()
  for(i in dir(pattern = ".csv")){
    temp_df <- read.csv(i)
    combined <- rbind(temp_df, combined) 
    counter <- counter+1
  }
  return(combined)
  print(paste(as.character(counter), "files have been combined"))
}

############################
#how many NAs in x

NA.counter <- function(x){
  length(which(is.na(x)))
} 

##################################
#show all positions with NA value in vector x (x indices)

NA.position  <- function(x){
  if (is.numeric(x)){
    s <- c(which(is.na(x)))
    return(s)
  }
  else {
    print("Warning: X is not numeric!")
  }
} 
###################################

zamena_na_mean_median <- function(x){
  if(is.numeric(x)){
    stat_test <- shapiro.test(x)
    if(stat_test$p.value>0.05){
      x[is.na(x)] <- mean(x, na.rm=T)
      print("NA have been replaced by mean")
    }
    else {
      x[is.na(x)] <- median(x, na.rm=T)
      print("NA have been replaced by median")
    }
    return(x)}
  else {
    print ("Warning: X is not numeric!")
  }
}

#####################################
filtered.cor <- function(df){
  numerics <- sapply(df, is.numeric) 
  newdf <- df[,numerics]
  c <- cor(newdf) 
  diag(c) <- 0 
  i <- which.max(abs((c))) 
  return(c[i]) 
}

###################################
#dataframe with 2 numeric variables - choose Pearson or Spearman method for cor.test

smart_cor <- function(df){ 
  stat1 <- shapiro.test(df[,1])
  stat2 <- shapiro.test(df[,2])
  if(stat1$p.value<0.05 | stat2$p.value<0.05){ 
    print(cor.test(df[,1],df[,2], method = "spearman")$estimate)
  }
  else
    print(cor.test(df[,1],df[,2], method = "pearson")$estimate)
}

######################################
#NA positions in 2 vectors are on the same places?
NA_position  <- function(x, y){
    all(is.na(x) == is.na(y))
} 
####################################
#Fisher or Chi - if observations n<5 or n>=5
smart_test <- function(df){
  test_df <- table(df)
  min <- min(test_df)
 if (min<5){
 result <- fisher.test(test_df)$p.value  
 } else {
   result <- c(chisq.test(test_df)$statistic, chisq.test(test_df)$parameter, chisq.test(test_df)$p.value)
}
return(result)
  }
##############################################
#Функция возвращает вектор с названием переменной (или переменных), в которой был получен минимальный p - уровень значимости при #проверке гипотезы о равномерном распределении частот в одной переменной при помощи критерия хи - квадрат. 
most_significant <- function(df){
  m <- sapply(df, function(x) chisq.test(table(x))$p.value)
  print(c(colnames(df[which(m==min(m))])))
}

##########################################
Если в нашей модели есть количественные предикторы, то в intercept мы будем иметь значение, соответствующее базовому уровню категориальных предикторов и нулевому уровню количественных. Это не всегда осмысленно. Например, нам не интересен прогноз для людей нулевого возраста или роста. В таких ситуациях количественную переменную имеет смысл предварительно центрировать так, чтобы ноль являлся средним значением переменной. Самый простой способ центрировать переменную — отнять от каждого наблюдения среднее значение всех наблюдений.

xcenteredi=xi−x¯xcenteredi=xi−x¯ 
Функция centered получает на вход датафрейм и имена переменных, которые необходимо центрировать так, как это описано выше. Функция возвращает этот же датафрейм, только с центрированными указанными переменными.
~~~~~

centered <- function(df, vars){ 
center <- as.data.frame(apply(df[sort(vars)], 2, function(x){x-mean(x)}))
df[sort(vars)] <- NULL
cbind(df, center)
}
##################################################
функция normality_test получает на вход dataframe с произвольным количеством переменных разных типов (количественные, строки, факторы) и проверяет нормальность распределения количественных переменных. Функция возвращает вектор значений p-уровней значимости теста shapiro.test для каждой количественной переменной.

normality_test <- function(df){
  nums <- df[sapply(df, is.numeric)]
  sapply(nums, function(x) shapiro.test(x)$p.value)
}

##############################################
is.numeric.data.frame <- function(x) sapply(x, is.numeric) - #show which vars are numeric in dataframe x

#################################################

На вход - dataframe с двумя переменными x и y. Переменная x — это количественная переменная, переменная y - фактор, разбивает наблюдения на три группы.
Если распределения во всех группах значимо не отличаются от нормального, а дисперсии в группах гомогенны, функция должна сравнить три группы при помощи дисперсионного анализа и вернуть именованный вектор со значением p-value, имя элемента — "ANOVA".
Если хотя бы в одной группе распределение значимо отличается от нормального или дисперсии негомогенны, функция сравнивает группы при помощи критерия Краскела — Уоллиса и возвращает именованный вектор со значением p-value, имя вектора  — "KW".

smart_anova <- function(df){
   
  a <- aggregate(unlist(df[1]), df[2], function(x) shapiro.test(x)$p.value)
  b <- bartlett.test(x~y, df)$p.value
  
  if((all(a[2]>=0.05)) & (b>=0.05)){ 
    fit <- aov(unlist(df[1])~unlist(df[2]), df)
    pvalue <- summary(fit)[[1]]$'Pr(>F)'[1]
    c <- c("ANOVA"= pvalue)
 
     } else {
      
       c <-  c("KW"= kruskal.test(x~y, data=df)$p.value)
  }
    return(c)
  }
  ######################################################
smart_hclust - функция, которая получает на вход dataframe  с произвольным числом количественных переменных и число кластеров, которое необходимо выделить при помощи иерархической кластеризации.
Функция в исходный набор данных добавляет новую переменную фактор - cluster  - номер кластера, к которому отнесено каждое из наблюдений.

smart_hclust <- function(df, n){
  clusters <- hclust(dist(df))
  df$cluster <- as.factor(cutree(clusters, n))
  return(df)
}

#########################################################
чтобы вытянуть p-value из результатов ANOVA
fit <- aov(..., df)
pvalue <- summary(fit)[[1]]$'Pr(>F)'[1]

###########################################################
Функция делит данные на n кластеров и выдает названия переменных, по которым значимо различаются выделенные кластеры.

get_difference <- function(df, n){
  clusters <- hclust(dist(df))
  df$cluster <- as.factor(cutree(clusters, n))
  pvalues<- sapply(df[, colnames(df)!="cluster"], function(x) summary(aov(x~cluster, df))[[1]]$'Pr(>F)'[1])
 return(names(pvalues[pvalues<0.05]))
}
###############################################################



```
