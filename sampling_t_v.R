df_0 <-read.csv("/home/sudipta/prr_df/V10.csv", header = TRUE, sep = ",")
df_0 <- df_0[,2:33]
df_0 <- df_0[sample(nrow(df_0), 25000, replace = FALSE),]
df_0[is.na(df_0)] <- 0

count_matrix <- matrix(rep(0,64), nrow = 32, ncol = 2, byrow = TRUE)
for(i in 1:ncol(df_0))
{
  count_0 = 0
  count_1 = 0
  for(j in 1:nrow(df_0))
  {
    if(df_0[j,i] == 0)
    {
      count_0 = count_0 + 1
    }
    else count_1 = count_1 + 1
  }
  count_matrix[i,1] <- count_0
  count_matrix[i,2] <- count_1
}

num_row = 0
for(i in 1:32)
{
  if(count_matrix[i,2] > 0)
    num_row = num_row + 1
}

idf = log10(32/num_row)
idf_matrix <- matrix(rep(0,64), nrow = 32, ncol = 2, byrow = TRUE)
for(i in 1:nrow(count_matrix))
{
  for(j in 1:ncol(count_matrix))
  {
    idf_matrix[i,j] <- (count_matrix[i,j]/(nrow(df_0)))*idf
  }
}
count_matrix <- cbind(count_matrix,idf_matrix)
colnames(count_matrix) <- c("count_0","count_1","tf-idf_0","tf_idf_1")

write.csv(count_matrix, file = "/home/sudipta/Desktop/samplev10/sample_25000_v10_10.csv", sep = ",")