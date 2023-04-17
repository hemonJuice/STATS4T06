


library(usedist)

euclidean <- function(x,y){
  dis = sqrt(sum((x - y)^2))
  return(dis)
}


x <- matrix(sin(1:30), nrow=5)



disMat <- dist_make(x,euclidean)


Hierar_cl <- hclust(disMat, method = "average")







a1 <- c("A","A","A","B")
a2 <- c("C","C","D","D")
a3 <- c(1.1,2.9,3.1,4.9)

c1 <- data.frame(a3,a2,a1)
x <- matrix(c1)





dist_make1 <- function (x, distance_fcn, ...) {
  distance_from_idxs <- function (idxs) {
    i1 <- idxs[1]
    i2 <- idxs[2]
    distance_fcn(x[i1,], x[i2,], ...)
  }
  size <- nrow(x)
  ##future::plan(future::multicore) should we assume the users will do this on their end?
  if (is.element("future.apply", loadedNamespaces())) {
    d <- future.apply::future_apply(utils::combn(size, 2), 2, distance_from_idxs)
  } else {
    d <- apply(utils::combn(size, 2), 2, distance_from_idxs)
  }
  attr(d, "Size") <- size
  xnames <- rownames(x)
  if (!is.null(xnames)) {
    attr(d, "Labels") <- xnames
  }
  attr(d, "Diag") <- FALSE
  attr(d, "Upper") <- FALSE
  class(d) <- "dist"
  d
}





irisdf <- iris[1:10,1:4]
dist_make1(irisdf,euclidean)






#similarity for categorical attribute################################

library("plyr")
MSFVS <- function(Ak,vik){
  freq = plyr::count(Ak)
  vikFreq = freq[which(freq[,1] == vik),2]
  lessSimilar = freq[freq[,2] <= vikFreq,]
  # if (length(lessSimilar) == 0){
  #   return(c())
  # }
  
  return(lessSimilar)
}

f1 <- c('a','b','b','a','c','c','c','c','a','b')

MSFVS(f1,'a')
MSFVS(f1,'b')
MSFVS(f1,'c')


simlarity_cat <- function(i,j,Ak){
  s = 0
  d = 0
  len = length(Ak)
  if (i!=j) {
    d = 1
    s = 0
    
  }else {
    msfvs = MSFVS(Ak,i)
    n = dim(msfvs)[1]
    if (n != 0){
      for (i in 1:n){
        d = d + (msfvs[i,2]*(msfvs[i,2]-1))/(len*(len-1))
      }
    }
  }
  s = 1 - d
  return(d)
}

simlarity_cat('a','a',f1) 
simlarity_cat('b','b',f1)
simlarity_cat('c','c',f1)
simlarity_cat('a','c',f1)


attriSimilarity_cat <- function(Ak){
  
  u = sort(unique(Ak))
  
  len = length(u)
  
  df = data.frame(matrix(ncol = 3, nrow = 0))
  
  
  
  for (i in 1:len){
    for (j in i:len){
      df[nrow(df) + 1,] = c(u[i],u[j],simlarity_cat(u[i],u[j],Ak))
    }
  }
  
  return(df)
}

attriSimilarity_cat(f1)



#Similarity for numerica attributes###########################################


MSFSS <- function(Ak, Vik, Vjk){
  
  freq = plyr::count(Ak)
  
  len = length(Ak)
  
  
  s = c()
  dis = abs(Vik - Vjk)
  VikL = min(c(Vik,Vjk))
  VjkH = max(c(Vik,Vjk))
  
  
  
  u = sort(unique(Ak))
  lenu = length(u)
  countu = 0
  
  
  
  for (g in 1:length(Ak)) {
    if (Ak[g] >= VikL && Ak[g] <= VjkH){
      countu = countu + 1
    }
  }
  
  
  
  df = data.frame(matrix(ncol = 3, nrow = 0))
  
  for (i in 1:lenu){
      for (j in i:lenu){
        interval = abs(u[i] - u[j])
        fi = freq[which(freq[,1] == u[i]),2]
        fj = freq[which(freq[,1] == u[j]),2]
        if (interval < dis) {
          if (interval == 0){
            
            df[nrow(df) + 1,] = c(u[i],u[j],(fi*(fi-1))/(len*(len-1))  )
          }else {
            df[nrow(df) + 1,] = c(u[i],u[j],(fi*fj*2)/(len*(len-1)) )
          }
          
          
        } else if (interval == dis) {
          count = 0
          for (k in 1:length(Ak)){
            
            if (Ak[k] >= u[i] && Ak[k] <= u[j]){
              count = count + 1
            }
          }
          if (count <= countu) {
            if (interval == 0){
              
              df[nrow(df) + 1,] = c(u[i],u[j],(fi*(fi-1))/(len*(len-1))  )
            }else {
              df[nrow(df) + 1,] = c(u[i],u[j],(fi*fj*2)/(len*(len-1)) )
            }
          }
        }
      }
  }
  
  return(df)
  
  
}


f2 <- c(6,5.5,7.5,6,10.5,9,7.5,9,7.5,7.5)
MSFSS(f2,7.5,9)
MSFSS(f2,9,10.5)
MSFSS(f2,9,9)


simlarity_num <- function(i,j,Ak){
  
  msfss = MSFSS(Ak,i,j)
  d = sum(msfss[,3])
  s = 1-d
  return(d)
}


simlarity_num(7.5,9,f2)
simlarity_num(9,10.5,f2)


attriSimilarity_num <- function(Ak){
  
  u = sort(unique(Ak))
  
  len = length(u)
  
  df = data.frame(matrix(ncol = 3, nrow = 0))
  
  
  
  for (i in 1:len){
    for (j in i:len){
      df[nrow(df) + 1,] = c(u[i],u[j],simlarity_num(u[i],u[j],Ak))
    }
  }
  
  return(df)
}

attriSimilarity_num(f2)





nextSimilar <- function(Ak,x){
  u = unique(Ak)
  len = length(u)
  
  df = attriSimilarity_cat(Ak)
  
  s = as.numeric(df[,3])
  
  
  
  
  
  if (identical(s[s<x],numeric(0)) == TRUE) {
    nextSmaller = 0
  }else {
    #print(x)
    nextSmaller = max(as.numeric(s[s<x]))
  }
  
  
  return(nextSmaller)

}

nextSimilar(f2,1)






allSimilarity <- function(df,cr){
  
  n = dim(df)[2]
  
  startCat = cr + 1
  
  similarity_list = list()
  
  
  for (i in 1:cr){
    a = attriSimilarity_num(df[,i])
    
    similarity_list[i] = list(a)
    
  }
  
  for (j in startCat:n){
    b = attriSimilarity_cat(df[,j])
    
    similarity_list[j] = list(b)
  }
  
  return(similarity_list)
  
}

gg <- allSimilarity(tc1,1)
gg[[1]]

ggg <- allSimilarity(sample1,1)

ggg<-ggg[[2]]
ggg[ggg[,1]=="a" & ggg[,2]=="c",3]




sss <- allSimilarity(n2c1s,2)





simiBetTwo <- function(d1,d2,s = sss,df = data.Inflammations,cr=1) {
  m = dim(df)[1]
  n = dim(df)[2]
  sumN = 0
  sumC = 0
  startCat = cr + 1
  
  for (k in 1:cr) {
    sim_tab = s[[k]]
    
    v1 = d1[1,k]
    
    v2 = d2[1,k]
    
    

    if (identical(sim_tab[sim_tab[,1]==v1 & sim_tab[,2]==v2,3],numeric(0))==FALSE){
      dijN = sim_tab[sim_tab[,1]==v1 & sim_tab[,2]==v2,3]
    }else if(identical(sim_tab[sim_tab[,1]==v2 & sim_tab[,2]==v1,3],numeric(0))==FALSE){
      dijN = sim_tab[sim_tab[,1]==v2 & sim_tab[,2]==v1,3]
    }
    #print(dijN)
    
    #if(is.null(sim_tab[sim_tab[,1]==v1 & sim_tab[,2]==v2,3])==TRUE){
       
    #}
    
    #dijN = simlarity_num(d1[1,k],d2[1,k],df[,k]) 
    sumN = sumN + log(dijN)
    #print(sumN)
  }
  
  chiN = sumN*(-2)
  
  #print(chiN)
  
  if (startCat <= n){

    

    for (j in startCat:n) {
      #dijC = simlarity_cat(d1[1,j],d2[1,j],df[,j])
      
      sim_tabc = s[[j]]
      
      v3 = d1[1,j]
      
      v4 = d2[1,j]
      
      
      if (identical(sim_tabc[sim_tabc[,1]==v3 & sim_tabc[,2]==v4,3],character(0))==FALSE){
        dijC = as.numeric(sim_tabc[sim_tabc[,1]==v3 & sim_tabc[,2]==v4,3])
      }else if(identical(sim_tabc[sim_tabc[,1]==v4 & sim_tabc[,2]==v3,3],character(0))==FALSE){
        dijC = as.numeric(sim_tabc[sim_tabc[,1]==v4 & sim_tabc[,2]==v3,3])
      }

      print(dijC)



      #print(j)
      dijCP = nextSimilar(df[,j],dijC)

      if (dijCP == 0) {
        logdijCP = 0
      }else{
        logdijCP = log(dijCP)
      }

      d = 1 - (dijC*log(dijC) - dijCP*logdijCP)/(dijC-dijCP)
      #print(dijCP)
      sumC  = sumC + d
    }
  }
  
  #print(sumC)
  
  
  chiC = 2*sumC
  
  chi = chiC + chiN
  
  
  sumchi = 0
  for (i in 1:n){
    sumchi = sumchi + (((1/2)*chi)^(i-1))/factorial(i-1)
  }
  tolDij = exp(-chi/2)*sumchi
  
  return(tolDij)
  
  
}



sample1 <- data.frame(f2,f1)
sample1



simiBetTwo(sample1[6,],sample1[9,],allSimilarity(sample1,1),sample1,1)
simiBetTwo(heartdf[2,],heartdf[3,])


irisHAC <- dist_make1(iris[,1:4],simiBetTwo)


Hierar_cl_iris <- hclust(irisHAC, method = "average")


fit <- cutree(Hierar_cl_iris, k = 3 )
table(fit)







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

heartTest1 <- dist_make1(heartdf,simiBetTwo)


Hierar_cl_heart <- hclust(heartTest1, method = "average")


cutree(Hierar_cl_heart, 2)

SBAC.heart <- data.frame(cutree(Hierar_cl_heart, 2))[,1]

table(SBAC.heart,heartdf [1:100,14])



#credit

library(foreign)
data.credit <- read.arff("D:\\STATS4T06\\Datasets\\dataset_31_credit-g.arff")

class.credit <- data.credit[1:100,21]

data.credit.SBAC <- data.credit[1:100,c(2,5,8,11,13,16,18,1,3,4,6,7,9,10,12,14,15,17,19,20)]

sss <- allSimilarity(data.credit.SBAC,5)
credit.distmat <- dist_make1(data.credit.SBAC,simiBetTwo(s = sss,df = data.credit.SBAC,cr=5))








#categorical artificial data set

c <- c('a','a','a','a','a','a','a','a','a','a','b','b','b','b','b','b','b','b','b','c','c','c','c','c','c','c','c','c')


b <- c('a','a','a','a','a','a','a','a','a','a','b','b','b','b','b','b','b','b','b','c','c','c','c','c','c','c','c','c')

a <- c(1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3)

tc1 <- data.frame(a,b,c)

tc1_r <- dist_make1(tc1,simiBetTwo)


simiBetTwo(tc1[1,],tc1[11,],tc1,1)

Hierar_cl_tc1 <- hclust(tc1_r, method = "average")
fit_tc1 <- cutree(Hierar_cl_tc1, k = 3 )
table(fit_tc1)

plot(hclust(tc1_r, method = "average"))





#Artificial data 1: 2 normal (seqarate)

clusters1s <- genRandomClust(numClust=2, sepVal=0.34, numNonNoisy=2,
                             numNoisy=0, numOutlier=0, numReplicate=1, fileName="Eq1",
                             clustszind=1,
                             clustSizeEq=50,
                             outputDatFlag=F,
                             outputLogFlag=F,
                             outputEmpirical=F,
                             outputInfo=F)
normal2s <- clusters1s$datList$Eq1_1

x3 <- clusters1s$memList$Eq1_1

n2c1s <- cbind(normal2s,x3)

n2c1s <- as.data.frame(n2c1s)

n2c1s$x3[n2c1s$x3 == 1] <- "a"
n2c1s$x3[n2c1s$x3 == 2] <- "b"



#rand.index(art1s[,1],clusters1s$memList$Eq1_1)
#adj.rand.index(art1s[,1],clusters1s$memList$Eq1_1)




art1_SBAC <- dist_make1(n2c1s,simiBetTwo)

Hierar_art1_SBAC <- hclust(art1_SBAC, method = "average")

fit_art1 <- cutree(Hierar_art1_SBAC, k = 2 )

plot(normal2s, col=fit_art1)

table(fit_art1,x3)




#Artificial data 2: 2 normal (seqarate)

library(clusterGeneration)

c2o <- genRandomClust(numClust=2, sepVal=0.01, numNonNoisy=2,
                             numNoisy=0, numOutlier=0, numReplicate=1, fileName="Eq1",
                             clustszind=1,
                             clustSizeEq=50,
                             outputDatFlag=F,
                             outputLogFlag=F,
                             outputEmpirical=F,
                             outputInfo=F)
n2o <- c2o$datList$Eq1_1

x3o <- c2o$memList$Eq1_1

n2c1o <- cbind(n2o,x3o)

n2c1o <- as.data.frame(n2c1o)

n2c1o$x3[n2c1o$x3 == 1] <- "a"
n2c1o$x3[n2c1o$x3 == 2] <- "b"



#rand.index(art1s[,1],clusters1s$memList$Eq1_1)
#adj.rand.index(art1s[,1],clusters1s$memList$Eq1_1)




art1o_SBAC <- dist_make1(n2c1o,simiBetTwo)

Hierar_art1o_SBAC <- hclust(art1o_SBAC, method = "average")

fit_art1 <- cutree(Hierar_art1_SBAC, k = 2 )

plot(normal2s, col=fit_art1)

table(c(1,1,2,1,1, 2,  1,  1,  2,  2,  2,  1,  2,  1,  2,  2,  2,  2,  1,  1),x3)




