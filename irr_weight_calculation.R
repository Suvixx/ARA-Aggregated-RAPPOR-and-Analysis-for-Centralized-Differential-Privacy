df <-  read.csv('/home/sudipta/prr_df/true_value/new.csv', header = TRUE , sep = ",")
df_s <- df[,1:33]
seed(200)
df_s <- df_s[sample(nrow(df_s), 25000), ]
ws <- matrix(data = rep(0,nrow(df_s)), nrow = nrow(df_s), ncol = 1)
weight <- c(1.20201279,1.092738,0.993399, 0.90309, 0.80618, 0.727, 0.66052, 0.60206,
            0.550907, 0.50515, 0.4637573, 0.425969, 0.3912066, 0.3590219,0.329059,
            0.30103)

for(i in 1:nrow(df_s))
{
  sum = 0
  for(j in 2:ncol(df_s))
  {
    if(df_s[i,j] == 1)
    {
      sum = sum + 1
    }
  }
  if(df_s[i,1] == 0)
  {
    ws[i,1] <- sum * (weight[sum])
  }
  else
  {
    ws[i,1] <- sum *(weight[sum])* df_s[i,1]
  }
  
}