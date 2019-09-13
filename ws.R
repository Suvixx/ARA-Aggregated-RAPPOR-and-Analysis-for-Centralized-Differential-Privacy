#tf_idf <- read.csv("/home/sudipta/real_tf-idf.csv", header = TRUE, sep = ",")
v1 <- read.csv("/home/sudipta/prr_df/V10.csv", header = TRUE, sep = ",")
v <- v1[,2:33]
v[is.na(v)] <- 0
ws <- matrix(data = rep(0,nrow(v1)), nrow = nrow(v1), ncol = 1)
for(i in 1:nrow(v))
{
  sum = 0
  for(j in 1:ncol(v))
  {
    if(v[i,j] == 1)
    {
      sum = (sum + v[i,j]*tf_idf[j,2])
    }
  }
  ws[i,1] = sum * v1[i,1]
}
write.csv(ws, file = "/home/sudipta/ws10.csv", sep = ",")