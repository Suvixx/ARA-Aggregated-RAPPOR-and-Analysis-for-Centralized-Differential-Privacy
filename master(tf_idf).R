#Reading data 
df1 <- read.csv('/home/sudipta/Downloads/master.csv', header = TRUE , sep = ",")
df_n <- df1
#separation of prr and irr
library(tidyverse)
df_nn <- separate(df1,prr, c("a","b"), sep = "e")
df_nn1 <- separate(df_nn,irr, c("c","d"), sep = "e")
#for the cohort
df1[,2] <- as.integer(df1[,2])
df01 <- df1[,2]
dec2bin <- function(x) { paste(as.integer(rev(intToBits(x))), collapse = "") }
df01 <- as.matrix(df01)
for(i in 1:nrow(df01))
{
      df01[i,1] = dec2bin(df01[i,1])
    
}
df1[,2] <- df01[,1]

#for the prr
df01 <- df_nn1[,4]
df01 <- as.matrix(df01)
for (c in 1:nrow(df01)) 
{
  
  k <- df01[c,1]
  k <- as.character(k)
  l <- substr(k,3,str_length(k))
  m <- paste(1,l,sep = "")
  n <- rep(0,32-str_length(m))
  n <- as.character(n)
  n <- strsplit(n, "", fixed = FALSE) 
  n <- paste(n, collapse = "")
  a <- paste(m,n,sep = "")
  df01[c,1] <- a
}
df1[,4] <- df01[,1]

#for the irr
df01 <- df_nn1[,6]
df01 <- as.matrix(df01)
for (c in 1:nrow(df01)) 
{
  
  k <- df01[c,1]   
  k <- as.character(k)
  l <- substr(k,3,str_length(k))
  m <- paste(1,l,sep = "")
  n <- rep(0,32-str_length(m))
  n <- as.character(n)
  n <- strsplit(n, "", fixed = FALSE) 
  n <- paste(n, collapse = "")
  a <- paste(m,n,sep = "")
  df01[c,1] <- a
}
df1[,5] <- df01[,1]

#for the bloom filter
df01 <- df1[,3]
df01 <- as.matrix(df01)
for(c in 1:nrow(df01))
{
  k <- df01[c,1]
  k <- as.character(k)
  i <- str_length(k)-1
  j <- str_length(k)
  
  if(i != 0)
  {
   
    n <- as.integer(sapply(strsplit(k, "", fixed = TRUE), '[',i ) ) 
    m <-  as.integer(sapply(strsplit(k, "", fixed = TRUE), '[', j))
    n <- 10*n + m 
    
    
    
    t <- rep(0,n)
    t<- as.character(t)
    t <- strsplit(t, "", fixed = FALSE) 
    t <- paste(t, collapse = "")
    
    l <- rep(0,(32-n-1))
    l <- as.character(l)
    l <- strsplit(l, "", fixed = FALSE) 
    l <- paste(l, collapse = "") 
    
    a <- paste(l,1,sep = "")
    b <- paste(a,t, sep = "")
    df01[c,1] <- b
  } else
  {
    t <- rep(0,31)
    t<- as.character(t)
    t <- strsplit(t, "", fixed = FALSE) 
    t <- paste(t, collapse = "")
    a <- paste(t,1,sep = "")
    
    
    
    df01[c,1] <- a
  }
  
}
df1[,3] <- df01[,1]

for(c in 1:nrow(df1))
{
  d <- df1[c,2]
  e <- df1[c,4]
  f <- df1[c,5]
  g <- paste(d,e,f, sep = "")
  df01[c,1] <- g
}
df1[,6] <- as.character(df1[,6])
df02 <- cbind.data.frame(df1[,3],df01[,1],df1[,6])

df02 <- df1
df2 <- subset(df02,df1[,6] == 'v1')
df3 <- subset(df02,df1[,6] == 'v2')
df4 <- subset(df02,df1[,6] == 'v3')
df5 <- subset(df02,df1[,6] == 'v4')
df6 <- subset(df02,df1[,6] == 'v5')
df7 <- subset(df02,df1[,6] == 'v6')
df8 <- subset(df02,df1[,6] == 'v7')
df9 <- subset(df02,df1[,6] == 'v8')
df10 <- subset(df02,df1[,6] == 'v9')
df11 <- subset(df02,df1[,6] == 'v10')
df12 <- subset(df02,df1[,6] == 'v11')
df13 <- subset(df02,df1[,6] == 'v12')
df14 <- subset(df02,df1[,6] == 'v13')
df15 <- subset(df02,df1[,6] == 'v14')
df16 <- subset(df02,df1[,6] == 'v15')
df17 <- subset(df02,df1[,6] == 'v16')
df18 <- subset(df02,df1[,6] == 'v17')
df19 <- subset(df02,df1[,6] == 'v18')
df20 <- subset(df02,df1[,6] == 'v19')
df21 <- subset(df02,df1[,6] == 'v20')
df22 <- subset(df02,df1[,6] == 'v21')
df23 <- subset(df02,df1[,6] == 'v22')
df24 <- subset(df02,df1[,6] == 'v23')
df25 <- subset(df02,df1[,6] == 'v24')
df26 <- subset(df02,df1[,6] == 'v25')
df27 <- subset(df02,df1[,6] == 'v26')
df28 <- subset(df02,df1[,6] == 'v27')
df29 <- subset(df02,df1[,6] == 'v28')
df30 <- subset(df02,df1[,6] == 'v29')
df31 <- subset(df02,df1[,6] == 'v30')
df32 <- subset(df02,df1[,6] == 'v31')
df33 <- subset(df02,df1[,6] == 'v32')
df34 <- subset(df02,df1[,6] == 'v33')
df35 <- subset(df02,df1[,6] == 'v34')
df36 <- subset(df02,df1[,6] == 'v35')
df37 <- subset(df02,df1[,6] == 'v36')
df38 <- subset(df02,df1[,6] == 'v37')
df39 <- subset(df02,df1[,6] == 'v38')
df40 <- subset(df02,df1[,6] == 'v39')
df41 <- subset(df02,df1[,6] == 'v40')
df42 <- subset(df02,df1[,6] == 'v41')
df43 <- subset(df02,df1[,6] == 'v42')
df44 <- subset(df02,df1[,6] == 'v43')
df45 <- subset(df02,df1[,6] == 'v44')
df46 <- subset(df02,df1[,6] == 'v45')
df47 <- subset(df02,df1[,6] == 'v46')
df48 <- subset(df02,df1[,6] == 'v47')
df49 <- subset(df02,df1[,6] == 'v48')
df50 <- subset(df02,df1[,6] == 'v49')
df51 <- subset(df02,df1[,6] == 'v50')
df52 <- subset(df02,df1[,6] == 'v51')
df53 <- subset(df02,df1[,6] == 'v52')
df54 <- subset(df02,df1[,6] == 'v53')
df55 <- subset(df02,df1[,6] == 'v54')
df56 <- subset(df02,df1[,6] == 'v55')
df57 <- subset(df02,df1[,6] == 'v56')
df58 <- subset(df02,df1[,6] == 'v57')
df59 <- subset(df02,df1[,6] == 'v58')
df60 <- subset(df02,df1[,6] == 'v59')
df61 <- subset(df02,df1[,6] == 'v60')
df62 <- subset(df02,df1[,6] == 'v61')
df63 <- subset(df02,df1[,6] == 'v62')
df64 <- subset(df02,df1[,6] == 'v63')
df65 <- subset(df02,df1[,6] == 'v64')
df66 <- subset(df02,df1[,6] == 'v65')
df67 <- subset(df02,df1[,6] == 'v66')
df68 <- subset(df02,df1[,6] == 'v67')
df69 <- subset(df02,df1[,6] == 'v68')
df70 <- subset(df02,df1[,6] == 'v69')
df71 <- subset(df02,df1[,6] == 'v70')
df72 <- subset(df02,df1[,6] == 'v71')
df73 <- subset(df02,df1[,6] == 'v72')
df74 <- subset(df02,df1[,6] == 'v73')
df75 <- subset(df02,df1[,6] == 'v74')
df76 <- subset(df02,df1[,6] == 'v75')
df77 <- subset(df02,df1[,6] == 'v76')
df78 <- subset(df02,df1[,6] == 'v77')
df79 <- subset(df02,df1[,6] == 'v78')
df80 <- subset(df02,df1[,6] == 'v79')
df81 <- subset(df02,df1[,6] == 'v80')
df82 <- subset(df02,df1[,6] == 'v81')
df83 <- subset(df02,df1[,6] == 'v82')
df84 <- subset(df02,df1[,6] == 'v83')
df85 <- subset(df02,df1[,6] == 'v84')
df86 <- subset(df02,df1[,6] == 'v85')
df87 <- subset(df02,df1[,6] == 'v86')
df88 <- subset(df02,df1[,6] == 'v87')
df89 <- subset(df02,df1[,6] == 'v88')
df90 <- subset(df02,df1[,6] == 'v89')
df91 <- subset(df02,df1[,6] == 'v90')
df92 <- subset(df02,df1[,6] == 'v91')
df93 <- subset(df02,df1[,6] == 'v92')
df94 <- subset(df02,df1[,6] == 'v93')
df95 <- subset(df02,df1[,6] == 'v94')
df96 <- subset(df02,df1[,6] == 'v95')
df97 <- subset(df02,df1[,6] == 'v96')
df98 <- subset(df02,df1[,6] == 'v97')
df99 <- subset(df02,df1[,6] == 'v98')
df100 <- subset(df02,df1[,6] == 'v99')
df101 <- subset(df02,df1[,6] == 'v100')



df_0 <- (df2[,5])
df_0 <- as.list(df_0)

library(compositions)
for(i in 1: nrow(df2) )
{
  k <- df_0[[i]]
  c <- list(unbinary(substr(k,1,1)),
            unbinary(substr(k,2,2)),
            unbinary(substr(k,3,3)),
            unbinary(substr(k,4,4)),
            unbinary(substr(k,5,5)),
            unbinary(substr(k,6,6)),
            unbinary(substr(k,7,7)),
            unbinary(substr(k,8,8)),
            unbinary(substr(k,9,9)),
            unbinary(substr(k,10,10)),
            unbinary(substr(k,11,11)),
            unbinary(substr(k,12,12)),
            unbinary(substr(k,13,13)),
            unbinary(substr(k,14,14)),
            unbinary(substr(k,15,15)),
            unbinary(substr(k,16,16)),
            unbinary(substr(k,17,17)),
            unbinary(substr(k,18,18)),
            unbinary(substr(k,19,19)),
            unbinary(substr(k,20,20)),
            unbinary(substr(k,21,21)),
            unbinary(substr(k,22,22)),
            unbinary(substr(k,23,23)),
            unbinary(substr(k,24,24)),
            unbinary(substr(k,25,25)),
            unbinary(substr(k,26,26)),
            unbinary(substr(k,27,27)),
            unbinary(substr(k,28,28)),
            unbinary(substr(k,29,29)),
            unbinary(substr(k,30,30)),
            unbinary(substr(k,31,31)),
            unbinary(substr(k,32,32)))
  df_0[[i]] <- c
}
#ij <- df_0
df_0 <- data.frame(matrix(unlist(df_0), nrow=length(df_0), byrow=T))
df_0[is.na(df_0)] <- 0
write.csv(df_0, file = 'irr_v1.csv', sep = ",")
c_freq <- list()
for(i in 1:32)
{
  c_freq[[i]] = table(df_0[,i])
}
c <- c_freq
write.csv(df_0,file = "df_0.csv", sep = "," )
#freq <- read.csv("./c_freq.csv", header = TRUE , sep = ",")

c <- data.frame(matrix(unlist(c), nrow=length(c), byrow=T))
