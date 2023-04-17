
library(dplyr)



#K-means###################################################################################################################

euclidean <- function(x,y){
  dis = sqrt(sum((x - y)^2))
  return(dis)
}


a <- c(2, 6, 7, 7, 5, 13, 14, 17, 11, 8)
b <- c(3, 5, 5, 3, 7, 12, 13, 19, 22, 7)

euclidean(a,b)

randCen <- function(df, k){
  
  m = dim(df)[1]
  n = dim(df)[2]
  centroids = as.data.frame(matrix(0,k,n))
  s = sample(c(1:m),size=k,replace=F)
  for (i in 1:k){
    centroids[i,] = df[s[i],]
  }
  return(centroids)
}

km <- function(df,k){
  m = dim(df)[1]
  
  clusterAssment = as.data.frame(matrix(0,m,2))
  clusterChange = TRUE
  
  centroids = randCen(df,k)
  
  while (clusterChange){
    
    clusterChange = FALSE
    
    for (i in 1:m){
      mindis = 100000000000
      mincluster = -1
      for (j in 1:k){
        distance = euclidean(df[i,],centroids[j,])
        if (distance < mindis){
          mindis = distance
          mincluster = j 
        }
        
      }
      if (clusterAssment[i,1]  != mincluster){
        clusterChange = TRUE
        clusterAssment[i,] = c(mincluster,mindis)
      }
    }
    for (l in 1:k) {
      pointsInCluster = df[which(clusterAssment[,1] == l),]
      centroids[l,] = colMeans(pointsInCluster)
      
    }
    
  }
  
  return(clusterAssment)
  
  
  
  
}



df <- iris[,1:4]
cluster <- km(df,3)
plot(df[c("Sepal.Length","Petal.Width")], col=cluster[,1])




 set.seed(0)
df1 <- rbind(matrix(rnorm(100, 0.5, 4.5), ncol = 2),
            +   matrix(rnorm(100, 0.5, 0.1), ncol = 2))
colnames(df1) <- c("x", "y")
head(df1)
cl <- kmeans(df1,2)
plot(df, col = cl$cluster, main="Kmeans Cluster")
km(df1,2)



#KMCMD#############################################################################################################

library(arules)

findMax <- function(x,y,Ai,Aj){
  dis = 0
  w = c()
  wc = c()
  
  indexX = which(Ai == x)
  indexY = which(Ai == y)
  
  ajX = Aj[indexX]
  ajY = Aj[indexY]
  
  uniAj = unique(Aj)
  
  

  for (i in 1:length(unique(Aj))){


    indexU = which(Aj == uniAj[i])
    amountX = length(which(Ai[indexU]==x))

    amountY = length(which(Ai[indexU]==y))
    
    if (length(indexX) == 0) {
      pX = 0
    }else {
      pX = amountX/length(indexX)
    }
    
    
    if (length(indexY) == 0) {
      pY = 0
    }else {
      pY = amountY/length(indexY)
    }
    
    
    

    if (pX >= pY) {
      w = append(w,pX)
      dis = dis + pX
    } else {
      wc = append(wc,pY)
      dis = dis + pY
    }
  }
  dis = dis - 1
  
  return(dis)
}

#test findMax##########################################

Director <- c("Scorsese","Coppola", "Hitchcock", "Hitchcock", "Koster","Koster")
Actor <- c("De Niro","De Niro","Stewart","Grant","Grant","Stewart")
Genre <- c("Crime","Crime","Thriller","Thriller","Comedy","Comedy")

movie <- data.frame(Director,Actor,Genre)


findMax("De Niro","Stewart",movie[,2],movie[,1])
findMax("De Niro","Stewart",movie[,2],movie[,3])

#######################################################



distance <- function(x,y,i,df) {
  
  
  
  m = dim(df)[1]
  n = dim(df)[2]
  
  sum = 0
  
  for (j in 1:n) {
    if (j == i) next
    d_ij = findMax(x,y,df[,i],df[,j])
    sum = sum + d_ij
  }
  
  tol = sum/(n-1)
  
  return(tol)
}


#test distance ###############################################

distance("Scorsese","Coppola",1,movie)
distance("Scorsese","Hitchcock",1,movie)
distance("Scorsese","Koster",1,movie)
distance("Hitchcock","Coppola",1,movie)
distance("Koster","Coppola",1,movie)
distance("Koster","Hitchcock",1,movie)

distance("De Niro","Stewart",2,movie)
distance("De Niro","Grant",2,movie)
distance("Stewart","Grant",2,movie)

distance("Crime","Comedy",3,movie)
distance("Crime","Thriller",3,movie)
distance("Thriller","Comedy",3,movie)

################################################################




algoDistance <- function(df) {
  m = dim(df)[1]
  n = dim(df)[2]
  disMat = matrix(0,m*(m-1)/2,n)
  
  for (i in 1:n) {  #for every attribute
    for (j in 1:m-1) {   #for each pair in attribute i
      for (k in j+1:m) {
        sum = 0
        for (l in 1:n) {  #for every other attributes Ai != Aj
          if (l == i) next
          d_ij = findMax(df[j,i],df[k,i],df[,i],df[,l])
          sum = sum + d_ij
        }
        tol = sum/(n-1)
        
        disMat[j*k-1,i] = tol
        
      }
    }
  }
  
  return(disMat)
}



sig <- function(Ai, i, df) {
  sum = 0
  s = unique(Ai)
  m = length(s)
  for (l in 1:m-1) {
    for (j in l+1:m) {
      
      tol = distance(s[l],s[j],i,df)
      
      sum = sum + tol
    }
  }
  
  w = sum/ (m*(m-1)/2)
  
  return(w)
  
}


#test sig##########################################################

alpha <- c("A","A","A","B","B","B","A")
beta <- c("C","C","D","D","C","D","D")
#gamma <- c(1.1,2.9,3.1,4.9,4.0,3.2,4.8)
gamma <- c("a","a","b","b","b","b","b")

arti <- data.frame(alpha,beta,gamma)

sig(arti[,3],3,arti)
sig(movie[,1],1,movie)
sig(movie[,2],2,movie)
sig(movie[,3],3,movie)

###################################################################

 

disBetTwo <- function(d1,d2,m,df){
  len = length(d1)
  
  vrd1 = d1[1:m]
  vcd1 = d2[m:len]
  
  vrd2 = d2[1:m]
  vcd2 = d2[m+1:len]
  
  sumr = 0
  sumc = 0
  
  
  for (i in 1:m) {
    w = sig(df[,i],i,df)
    d = (w*(vrd1[i] - vrd2[i]))^2
    sumr = sumr + d
  }
  
  for (j in m+1:len) {
    d = 0
    for (k in 1:len) {
      if (k == j) next 
      d = d + findMax(d1[j],d2[j],df[,j],df[,k])
    }
    tol = (d/(len-1))^2
    sumc = sumc + tol
  }
  
  dis = sumr + sumc
  
  return(dis)
  
}






library(caret)




clusterCenter <- function(cr,data,df) {
  
  m = dim(data)[1]
  n = dim(data)[2]
  c = cr + 1
  
  
  centroid = list()
  
  # if (m == 0){
  #   for (i in 1:cr) {
  #     centroid = append(centroid, 0)
  #   }
  #   for (j in c:n) {
  #     
  #     centroid = append(centroid, list())
  #   }
  # }
  
  
  if(cr != 0){
    for (i in 1:cr){
      mini = min(df[,i])
      maxi = max(df[,i])
      if(m==0){
        mi = 0
        mini = 0
        maxi =0
      }else{
        mi = mean(data[,i])
        
        
      }
      
      
      if ((maxi - mini) == 0) {
        normMean = 0
      }else {
        normMean = (mi - mini)/(maxi - mini)
      }
      
      centroid = append(centroid,normMean)
    }
  }
  
  
  
  
  
  if (c <= n){
    for (k in c:n){
      t = data.frame(table(data[,k]))
      #centroid[j] = as.numeric(as.character(t[,1]))/n
      
      t[,2] = t[,2]/m
      centroid = append(centroid,list(t))
    }
  }
  
  
  
  
  return(centroid)
  
  
}



#test cluster centers###########################################################

a1 <- c("A","A","A","B")
a2 <- c("C","C","D","D")
a3 <- c(1.1,2.9,3.1,4.9)

c1 <- data.frame(a3,a2,a1)



cluster1 <- clusterCenter(1,c1,artiT)

cluster1
unlist(cluster1[1])+1




################################################################################

sigDf <- function(discdf,cr) {
  sigmas = c()
  
  if (cr != 0){
    for (i in 1:cr) {
      sigmas = append(sigmas,sig(discdf[,i],i,discdf))
    }
  }
  
  return(sigmas)
  
}






disBetOC <- function(x,c,cr,df,discdf,significance) {
  
  
  
  sumr = 0
  sumc = 0
  
  len = length(x)
  n = dim(df)[2]
  
  #sigificance = sigDf(discdf,cr)
  
  
  if (cr != 0) {
    for (t in 1:cr) { #numerical attributes
      
      rt = unlist(c[t])
      #wt = sig(discdf[,t],t,discdf)
      #wt=1
      wt = significance[t]
      d = (wt*(x[1,t] - rt))^2
      sumr = sumr + d
    }
  }
  
  
  
  catestart = cr + 1
  
  
  if (catestart <= n){
    for (t in catestart:len) {
      ct = c[t][[1]]
      nct = dim(ct)[1]
      
      omega = 0
      
      for (i in 1:nct){
        coe = ct[i,2]
        d = distance(x[1,t],ct[i,1],t,discdf)
        omega = omega + coe*d
      }

      omega = omega^2
      
      sumc = sumc + omega
      
    }
  }
  
  
  
  tol = sumr + sumc
  
  return(tol)
  
}


#test distance between object and cluster center##########################################

alphaT <- c("A","A","A","B","B","B","A")
betaT <- c("C","C","D","D","C","D","D")
gammaT <- c(1.1,2.9,3.1,4.9,4.0,3.2,4.8)

artiT <- data.frame(gammaT,betaT,alphaT)


alphaD <- c("A","A","A","B","B","B","A")
betaD <- c("C","C","D","D","C","D","D")
gammaD <- c("a","a","b","b","b","b","b")

artiD <- data.frame(gammaD,betaD,alphaD)


#function
d1 <- data.frame((1.1-1.1)/(4.9-1.1),"C","A")
significanceArtiT <- sigDf(artiD,1)

disBetOC(d1,cluster1,1,artiT,artiD,significanceArtiT)

#calculation
w1 <- sig(artiD[,1],1,artiD)
dd1 <- (w1*(0-0.5))^2
oo1 <- (3/4)*distance("A","A",3,artiD) + (1/4)*distance("A","B",3,artiD)
oo2 <- (2/4)*distance("C","C",2,artiD) + (2/4)*distance("C","D",2,artiD)
disArti <- dd1 + oo1^2 + oo2^2
disArti

#function = calculation



############################################################################################









randCenKMCMD <- function(df, k, cr){
  
  m = dim(df)[1]
  n = dim(df)[2]
  centroids = list()
  folds = createFolds(c(1:m), k=k)
  
  #print(folds)
  #s = sample(1:k,size=m,replace = T)
  #dfs = split(artiT,sample(1:k,size=m,replace = T))
  for (i in 1:k){
    centroids[i] = list(clusterCenter(cr,df[folds[i][[1]],],df))
  }
  #centroids[2] = list(clusterCenter(cr,dfs$'2'))
  return(centroids)
}


#test random centers##############################################################

randCenKMCMD(artiT,3,1)
randCenKMCMD(iris[,1:4],3,4)
randCenKMCMD(movie,3,0)






discretizeDf <- function(df,cr) {
  m = dim(df)[1]
  n = dim(df)[2]
  
  mat = matrix(0,m,n)
  discdf = as.data.frame(mat)
  
  if (cr != 0) {
    for (i in 1:cr) {
      discdf[,i] = discretize(df[,i],method="interval")
    }
  }
  
  
  
  catestart = cr + 1
  
  if (catestart <= n){
    for (j in catestart:n){
      discdf[,j] = df[,j]
    }
  }
  
  return(discdf)
  
}

discretizeDf(iris[,1:4],4)
discretizeDf(movie,0)





normalizeDf <- function(df,cr){
  m = dim(df)[1]
  n = dim(df)[2]
  
  mat = matrix(0,m,n)
  nordf = as.data.frame(mat)
  
  if (cr != 0) {
    for (i in 1:cr) {
      mini = min(df[,i])
      maxi = max(df[,i])
      nordf[,i] = (df[,i] - mini) / (maxi - mini)
    }
  }
  
  
  
  catestart = cr + 1
  
  if (catestart <= n){
    for (j in catestart:n){
      nordf[,j] = df[,j]
    }
  }
  
  return(nordf)
}

normalizeDf(iris[,1:4],4)  

normalizeDf(movie,0)

















kmcmd <- function(df,k,cr){
  m = dim(df)[1]
  n = dim(df)[2]
  
  
  discdf = discretizeDf(df,cr)
  df = normalizeDf(df,cr)
  significane = sigDf(discdf,cr)
  
  
  clusterAssment = as.data.frame(matrix(0,m,2))
  clusterChange = TRUE
  centroids = randCenKMCMD(df,k,cr)
  
  temp = list()
  
  while (clusterChange){

    clusterChange = FALSE

    for (i in 1:m){
      mindis = 100000000000
      mincluster = -1
      for (j in 1:k){
        distance = disBetOC(df[i,],centroids[j][[1]],cr,df,discdf,significane)
        if (distance < mindis){
          mindis = distance
          mincluster = j
        }

      }
      if (clusterAssment[i,1] != mincluster){
        clusterChange = TRUE
        clusterAssment[i,] = c(mincluster,mindis)
      }

    }
    #print(clusterAssment)
    
    for (l in 1:k) {
      pointsInCluster = df[which(clusterAssment[,1] == l),]
      centroids[l] = list(clusterCenter(cr,pointsInCluster,df))
      temp[l] = list(pointsInCluster)
    }

  }
  
  return(clusterAssment)
  
}


#kmcmd centroids:

kmcmd.center <- function(df,k,cr){
  m = dim(df)[1]
  n = dim(df)[2]
  
  
  discdf = discretizeDf(df,cr)
  df = normalizeDf(df,cr)
  significane = sigDf(discdf,cr)
  
  
  clusterAssment = as.data.frame(matrix(0,m,2))
  clusterChange = TRUE
  centroids = randCenKMCMD(df,k,cr)
  
  temp = list()
  
  while (clusterChange){
    
    clusterChange = FALSE
    
    for (i in 1:m){
      mindis = 100000000000
      mincluster = -1
      for (j in 1:k){
        distance = disBetOC(df[i,],centroids[j][[1]],cr,df,discdf,significane)
        if (distance < mindis){
          mindis = distance
          mincluster = j
        }
        
      }
      if (clusterAssment[i,1] != mincluster){
        clusterChange = TRUE
        clusterAssment[i,] = c(mincluster,mindis)
      }
      
    }
    #print(clusterAssment)
    
    for (l in 1:k) {
      pointsInCluster = df[which(clusterAssment[,1] == l),]
      centroids[l] = list(clusterCenter(cr,pointsInCluster,df))
      temp[l] = list(pointsInCluster)
    }
    
  }
  
  return(centroids)
  
}


#kmcmd whole

kmcmd.all <- function(df,k,cr){
  m = dim(df)[1]
  n = dim(df)[2]
  
  
  discdf = discretizeDf(df,cr)
  df = normalizeDf(df,cr)
  significane = sigDf(discdf,cr)
  
  
  
  clusterAssment = as.data.frame(matrix(0,m,2))
  clusterChange = TRUE
  centroids = randCenKMCMD(df,k,cr)
  
  temp = list()
  
  while (clusterChange){
    
    clusterChange = FALSE
    
    for (i in 1:m){
      mindis = 100000000000
      mincluster = -1
      for (j in 1:k){
        distance = disBetOC(df[i,],centroids[j][[1]],cr,df,discdf,significane)
        if (distance < mindis){
          mindis = distance
          mincluster = j
        }
        
      }
      if (clusterAssment[i,1] != mincluster){
        clusterChange = TRUE
        clusterAssment[i,] = c(mincluster,mindis)
      }
      
    }
    #print(clusterAssment)
    
    for (l in 1:k) {
      pointsInCluster = df[which(clusterAssment[,1] == l),]
      centroids[l] = list(clusterCenter(cr,pointsInCluster,df))
      temp[l] = list(pointsInCluster)
    }
    
  }
  
  
  all = list("result" = clusterAssment, "center" = centroids)
  
  return(all)
  
}









#Test########################################################################################


diag_weight <- read.csv("D:/STATS4T06/SOM/df_neuron_diag.csv")

diag_weight <- diag_weight[,2:7]

diag_weight

kmcmd.diag_weight <- kmcmd(diag_weight,2,1)


re_diag_weight <- cbind(diag_weight,kmcmd.diag_weight[,1])

write.csv(re_diag_weight, file = "D:\\STATS4T06\\Datasets\\diag_weight_result.csv")














diag_weight <- read.csv("D:/STATS4T06/Datasets/diag_weight.csv")

diag_weight <- diag_weight[,2:7]

diag_weight

kmcmd.diag_weight <- kmcmd(diag_weight,2,1)


re_diag_weight <- cbind(diag_weight,kmcmd.diag_weight[,1])

write.csv(re_diag_weight, file = "D:\\STATS4T06\\Datasets\\diag_weight_result.csv")























#Sa_heart

df.sa_heart <- read.arff("D:\\STATS4T06\\Datasets\\sa_heart.arff")
write.csv(df.sa_heart, file = "D:\\STATS4T06\\Datasets\\sa_heart.csv")


summary(df.sa_heart)
class.sa_heart <- df.sa_heart[,10]
class.sa_heart <- ifelse(class.sa_heart == 2,1,2)

df.sa_heart <- df.sa_heart[,c(1,2,3,4,6,7,8,9,5)]


kmcmd.df.sa_heart <- kmcmd(df.sa_heart,2,8)

sa_heat.cm <- table(kmcmd.df.sa_heart[,1],class.sa_heart)

sa_heat.cm
# 157 33
# 145 127
adj.rand.index(kmcmd.df.sa_heart[,1],class.sa_heart)




#kidney

df.kidney <- read.arff("D:\\STATS4T06\\Datasets\\kidney.arff")

class.df.kidney <- df.kidney[,6]

df.kidney <- df.kidney[,c(2,4,7,3,5)]
df.kidney[,4] <- as.character(df.kidney[,4])
df.kidney[,5] <- as.character(df.kidney[,5])

kmcmd.df.kidney <- kmcmd(df.kidney,4,3)

kidney.cm <- table(kmcmd.df.kidney[,1],class.df.kidney)

kidney.cm


#Credi aproval
library("tidyr")
df.ca <- read.table("D:\\STATS4T06\\Datasets\\crx.data", sep = ",")
df.ca <- df.ca %>% drop_na()

for (i in 1:16) {df.ca<-subset(df.ca,df.ca[,i]!="?")}
  
write.csv(df.ca, file = "D:\\STATS4T06\\Datasets\\crx.csv")


class.ca <- df.ca[,16]
class.ca <- ifelse(class.ca == '+',1,2)

df.ca <- df.ca[,c(2,3,8,14,15,1,4,5,6,7,9,10,11,12,13)]
df.ca[,1] <- as.numeric(df.ca[,1])
df.ca[,2] <- as.numeric(df.ca[,2])
df.ca[,3] <- as.numeric(df.ca[,3])
df.ca[,4] <- as.numeric(df.ca[,4])
df.ca[,5] <- as.numeric(df.ca[,5])




kmcmd.df.ca <- kmcmd(df.ca,2,5)
ca.cm <- table(kmcmd.df.ca[,1],class.ca)
ca.cm
#203 84
#93  273

adj.rand.index(kmcmd.df.ca[,1],class.ca)




#Irish
library(dplyr)
library(tidyr)
df.irish <- read.arff("D:\\STATS4T06\\Datasets\\irish.arff")
df.irish <- df.irish %>% drop_na()
head(df.irish)

csv.irish <- df.irish[,c(2,5,1,3,6,4)]
write.csv(csv.irish, file = "D:\\STATS4T06\\Datasets\\irish.csv")

class.irish<- df.irish[,4]
class.irish <- ifelse(class.irish =="taken",1,2)

df.irish <- df.irish[,c(2,5,1,3,6)]

kmcmd.df.irish <- kmcmd(df.irish,2,2)
irish.cm <- table(kmcmd.df.irish[,1],class.irish)
irish.cm

adj.rand.index(kmcmd.df.irish[,1],class.irish)

#211 101
#0   156






######### Cancer
library(multimix)
data(cancer.df)
cancer.df <- cancer.df[,c(1,2,5,6,8,9,10,11,3,4,7,12)]


class.cancer<- cancer.df[,12]

cancer.df <- cancer.df[,1:11]


kmcmd.df.cancer <- kmcmd(cancer.df,2,8)
cancer.cm <- table(kmcmd.df.cancer[,1],class.cancer)
cancer.cm

#20 27
#57 371



#Byar cancer
library(clustMD)
data(Byar)

head(Byar)

df.byar <- Byar[,c(1,2,5,6,8,9,10,11,3,4,7,12)]

class.byar<- Byar[,13]

class.byar <- ifelse(class.byar ==3,2,1)

csv.byar <- Byar[,c(1,2,5,6,8,9,10,11,3,4,7,12,13)]
write.csv(csv.byar, file = "D:\\STATS4T06\\Datasets\\byar.csv")


kmcmd.df.byar <- kmcmd(df.byar,2,8)

byar.cm <- table(kmcmd.df.byar[,1],class.byar)
byar.cm

adj.rand.index(kmcmd.df.byar[,1],class.byar)








#credit

library(foreign)
data.credit <- read.arff("D:\\STATS4T06\\Datasets\\dataset_31_credit-g.arff")

class.credit <- data.credit[1:300,21]

data.credit <- data.credit[1:300,c(2,5,8,11,13,16,18,1,3,4,6,7,9,10,12,14,15,17,19,20)]

head(data.credit)

kmcmd.data.credit <- kmcmd(data.credit,2,7)



credit.cm <- table(kmcmd.data.credit[,1],class.credit)

rand.index(kmcmd.data.credit[,1],class.credit.temp)





#k-prot
library(clustMixType)

credit.kpro <- kproto(data.credit, 2)

clprofiles(credit.kpro, data.credit)

as.character(credit.kpro$cluster) 

asd <- table(as.character(credit.kpro$cluster),class.credit)

max(sum(diag(asd))/sum(asd), 1-sum(diag(asd))/sum(asd))

class.credit.temp <- as.numeric(class.credit)
class.credit.temp[is.na(class.credit.temp)] <- 2


rand.index(as.numeric(credit.kpro$cluster),class.credit.temp)


#Acute Inflammations
data.Inflammations <- read.csv("D:/STATS4T06/Datasets/diagnosis.csv")

class1.Inflammations <- data.Inflammations[,7]
class1.Inflammations[class1.Inflammations == "yes"] <- 1
class1.Inflammations[class1.Inflammations == "no"] <- 2
class1.Inflammations <- as.numeric(class1.Inflammations)



class2.Inflammations <- data.Inflammations[,8]
class2.Inflammations[class2.Inflammations == "yes"] <- 1
class2.Inflammations[class2.Inflammations == "no"] <- 2
class2.Inflammations <- as.numeric(class2.Inflammations)


data.Inflammations <- data.Inflammations[,1:6]

kmcmd.infla <- kmcmd(data.Inflammations,2,1)

length(kmcmd.infla[,1])
length(class1.Inflammations)

table(kmcmd.infla[,1], class1.Inflammations)
table(kmcmd.infla[,1], class2.Inflammations)

rand.index(kmcmd.infla[,1], class1.Inflammations)
rand.index(kmcmd.infla[,1], class2.Inflammations)
adj.rand.index(kmcmd.infla[,1], class1.Inflammations)


#k-prot

data.Inflammations[, 2] <- as.factor(data.Inflammations[, 2])
data.Inflammations[, 3] <- as.factor(data.Inflammations[, 3])
data.Inflammations[, 4] <- as.factor(data.Inflammations[, 4])
data.Inflammations[, 5] <- as.factor(data.Inflammations[, 5])
data.Inflammations[, 6] <- as.factor(data.Inflammations[, 6])

infla.kpro <- kproto(data.Inflammations, 2)

table(infla.kpro$cluster,class1.Inflammations)
table(infla.kpro$cluster,class2.Inflammations)
rand.index(as.numeric(infla.kpro$cluster),class1.Inflammations)
rand.index(as.numeric(infla.kpro$cluster),class2.Inflammations)
adj.rand.index(as.numeric(infla.kpro$cluster),class1.Inflammations)








#iris 
kmcmdCluster <- kmcmd(iris[,1:4],3,4)
plot(iris[,1:4][c("Sepal.Length","Petal.Width")], col=kmcmdCluster[,1])
table(kmcmdCluster[,1],iris[,5])

km <- km(iris[,1:4],3)
table(km[,1],iris[,5])


#movie
kmcmd(movie,2,0)


#heart disease data

heartdf <- read.csv("C:\\Users\\99506\\Desktop\\heart.csv")


heartdf <- heartdf[,c(1,4,5,8,10,2,3,6,7,9,11,12,13,14)]
heartdf[,6] <- as.character(heartdf[,6])
heartdf[,7] <- as.character(heartdf[,7])
heartdf[,8] <- as.character(heartdf[,8])
heartdf[,9] <- as.character(heartdf[,9])
heartdf[,10] <- as.character(heartdf[,10])
heartdf[,11] <- as.character(heartdf[,11])
heartdf[,12] <- as.character(heartdf[,12])
heartdf[,13] <- as.character(heartdf[,13])
heartdf[,14] <- as.character(heartdf[,14])

heartdf <- heartdf[,1:14]

heartdf
write.csv(heartdf, file = "D:\\STATS4T06\\Datasets\\heartdf.csv")

heartTest <- kmcmd(heartdf,2,5)


table(heartTest[,1],heartdf[,14])
adj.rand.index(heartTest[,1],heartdf[,14])
# 403  76
# 96   450











 dfiris <- iris[,1:4]
cen <- randCenKMCMD(dfiris,3,4)
ddd <- discretizeDf(dfiris,4)



disBetOC(dfiris[1,],cen[1][[1]],4,dfiris,ddd)
disBetOC(dfiris[1,],cen[2][[1]],4,dfiris,ddd)
disBetOC(dfiris[1,],cen[3][[1]],4,dfiris,ddd)


disBetOC(dfiris[2,],cen[1][[1]],4,dfiris,ddd)
disBetOC(dfiris[2,],cen[2][[1]],4,dfiris,ddd)
disBetOC(dfiris[2,],cen[3][[1]],4,dfiris,ddd)


disBetOC(dfiris[3,],cen[1][[1]],4,dfiris,ddd)
disBetOC(dfiris[3,],cen[2][[1]],4,dfiris,ddd)
disBetOC(dfiris[3,],cen[3][[1]],4,dfiris,ddd)


disBetOC(dfiris[4,],cen[1][[1]],4,dfiris,ddd)
disBetOC(dfiris[4,],cen[2][[1]],4,dfiris,ddd)
disBetOC(dfiris[4,],cen[3][[1]],4,dfiris,ddd)

disBetOC(dfiris[5,],cen[1][[1]],4,dfiris,ddd)
disBetOC(dfiris[5,],cen[2][[1]],4,dfiris,ddd)
disBetOC(dfiris[5,],cen[3][[1]],4,dfiris,ddd)


disBetOC(dfiris[120,],cen[1][[1]],4,dfiris,ddd)
disBetOC(dfiris[120,],cen[2][[1]],4,dfiris,ddd)
disBetOC(dfiris[120,],cen[3][[1]],4,dfiris,ddd)


disBetOC(dfiris[121,],cen[1][[1]],4,dfiris,ddd)
disBetOC(dfiris[121,],cen[2][[1]],4,dfiris,ddd)
disBetOC(dfiris[121,],cen[3][[1]],4,dfiris,ddd)


#coffee data
library(pgmm)
library(dplyr)
library(clustMixType)

data(coffee)
head(coffee)


coffee_df <- coffee

#coffee_df <- df[, c(3,4,5,6,7,8,9,10,11,12,13,14,2,1)]

coffee_df <- select(coffee_df,3,4,5,6,7,8,9,10,11,12,13,14,2,1)

coffee_km <- kmcmd(coffee_df[,1:12],2,12)

table(coffee_km[,1],coffee_df[,14])



#crx

crxdf <- read.csv("C:\\Users\\99506\\Desktop\\crx.csv")

crxdf <- na.omit(crxdf)

crxdf <- select(crxdf,2,3,8,11,14,15,1,4,5,6,7,9,10,12,13,16)

dim(crxdf)

crx_km <- kmcmd(crxdf[,1:15],2,6)

table(crxdf[,16],crx_km[,1])



#Artificial data 1: 2 normal (overlap)
library(clusterGeneration)
library(fossil)

clusters1 <- genRandomClust(numClust=2, sepVal=0.01, numNonNoisy=2,
               numNoisy=0, numOutlier=0, numReplicate=1, fileName="Eq1",
               clustszind=1,
               clustSizeEq=300,
               outputDatFlag=F,
               outputLogFlag=F,
               outputEmpirical=F,
               outputInfo=F)
normal2 <- clusters1$datList$Eq1_1

plot(clusters1$datList$Eq1_1)


art1 <- kmcmd(clusters1$datList$Eq1_1,2,2)
art1

plot(normal2, col=art1[,1])

#accuracy
art1_CM <- table(art1[,1],clusters1$memList$Eq1_1)
1 - sum(diag(art1_CM))/sum(art1_CM)


rand.index(art1[,1],clusters1$memList$Eq1_1)
adj.rand.index(art1[,1],clusters1$memList$Eq1_1)


#Artificial data 1: 2 normal (seqarate)

clusters1s <- genRandomClust(numClust=2, sepVal=0.34, numNonNoisy=2,
                            numNoisy=0, numOutlier=0, numReplicate=1, fileName="Eq1",
                            clustszind=1,
                            clustSizeEq=100,
                            outputDatFlag=F,
                            outputLogFlag=F,
                            outputEmpirical=F,
                            outputInfo=F)
normal2s <- clusters1s$datList$Eq1_1

art1s <- kmcmd(normal2s,2,2)

plot(normal2s, col=art1s[,1])

table(art1s[,1],clusters1s$memList$Eq1_1)

rand.index(art1s[,1],clusters1s$memList$Eq1_1)
adj.rand.index(art1s[,1],clusters1s$memList$Eq1_1)



#Artificial data 2: 2 normal with 1 cat (overlap)


x3 <- clusters1$memList$Eq1_1

n2c1 <- cbind(normal2,x3)

art2 <- kmcmd(n2c1,2,2)

plot(normal2, col=art2[,1])

table(art2[,1],clusters1$memList$Eq1_1)
art2_CM <- table(art2[,1],clusters1$memList$Eq1_1)
1 - sum(diag(art2_CM))/sum(art2_CM)

rand.index(art2[,1],clusters1$memList$Eq1_1)
adj.rand.index(art2[,1],clusters1$memList$Eq1_1) #result is better



#Artificial data 2: 2 normal with 1 cat (separate)


x3s <- clusters1s$memList$Eq1_1

n2c1s <- cbind(normal2s,x3s)

art2s <- kmcmd(n2c1s,2,2)

plot(normal2s, col=art2s[,1])

table(art2s[,1],clusters1s$memList$Eq1_1)

rand.index(art2s[,1],clusters1s$memList$Eq1_1)
adj.rand.index(art2s[,1],clusters1s$memList$Eq1_1) #result is better



#Artificial data 3: 2 normal with 1 cat (overlap), 1000 X 1000

clusters3o <- genRandomClust(numClust=2, sepVal=0.01, numNonNoisy=2,
                             numNoisy=0, numOutlier=0, numReplicate=1, fileName="Eq1",
                             clustszind=1,
                             clustSizeEq=1000,
                             outputDatFlag=F,
                             outputLogFlag=F,
                             outputEmpirical=F,
                             outputInfo=F)
normal3o <- clusters3o$datList$Eq1_1

x3o_3 <- clusters3o$memList$Eq1_1

n2c1o_3 <- cbind(normal3o,x3o_3)


art3o <- kmcmd(n2c1o_3,2,2)

plot(normal3o, col=art3o[,1])

table(art3o[,1],clusters3o$memList$Eq1_1)

rand.index(art1s[,1],clusters1s$memList$Eq1_1)
adj.rand.index(art1s[,1],clusters1s$memList$Eq1_1)

#kp

n2c1o_3_replace <- as.data.frame(n2c1o_3)  
n2c1o_3_replace$x3o_3[n2c1o_3_replace$x3o_3 == 1] <- "a"
n2c1o_3_replace$x3o_3[n2c1o_3_replace$x3o_3 == 2] <- "b"

n2c1o_3_replace <- cbind(n2c1o_3_replace,n2c1o_3_replace$x3o_3)
n2c1o_3_replace$`n2c1o_3_replace$x3o_3`[n2c1o_3_replace$`n2c1o_3_replace$x3o_3` == "a"] <- "c"
n2c1o_3_replace$`n2c1o_3_replace$x3o_3`[n2c1o_3_replace$`n2c1o_3_replace$x3o_3` == "b"] <- "d"

n2c1o_3_replace$x3o_3 <- as.factor(n2c1o_3_replace$x3o_3)
n2c1o_3_replace$`n2c1o_3_replace$x3o_3` <- as.factor(n2c1o_3_replace$`n2c1o_3_replace$x3o_3`)




a <- lambdaest(n2c1o_3_replace)
kpres <- kproto(n2c1o_3_replace, 2,lambda = a)
summary(kpres)



art3o11 <- kmcmd(n2c1o_3_replace,2,2)

plot(normal3o, col=art3o11[,1])

table(art3o11[,1],clusters3o$memList$Eq1_1)



#Artificial data 4: 3 normal 1 binary (overlap)
library(clusterGeneration)
library(fossil)

clusters5 <- genRandomClust(numClust=3, sepVal=0.01, numNonNoisy=2,
                            numNoisy=0, numOutlier=0, numReplicate=1, fileName="Eq1",
                            clustszind=1,
                            clustSizeEq=300,
                            outputDatFlag=F,
                            outputLogFlag=F,
                            outputEmpirical=F,
                            outputInfo=F)
normal5 <- clusters5$datList$Eq1_1

x3_5 <- clusters5$memList$Eq1_1

n3c1 <- cbind(normal5,x3_5)



plot(clusters5$datList$Eq1_1)


art5 <- kmcmd(n3c1,3,2)
art5

plot(normal5, col=art5[,1])

table(art5[,1],clusters5$memList$Eq1_1)


#Artificial data 5: 3 normal 1 binary(overlap)



