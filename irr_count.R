cohort <-  read.csv('/home/sudipta/v9_cohort.csv', header = TRUE , sep = ",")
count_irr_df <- df10[,4]
count_irr_df <- as.data.frame(count_irr_df)
irr_count <- matrix(data = rep(0,nrow(count_irr_df)), nrow = nrow(count_irr_df ), ncol = 1)
ws <- matrix(data = rep(0,nrow(count_irr_df)), nrow = nrow(count_irr_df), ncol = 1)
weight <- c(1.20201279,1.092738,0.993399, 0.90309, 0.80618, 0.727, 0.66052, 0.60206,
            0.550907, 0.50515, 0.4637573, 0.425969, 0.3912066, 0.3590219,0.329059,
            0.30103)

for(i in 1:nrow(count_irr_df))
{
  m <- count_irr_df[i,1]
  m <- as.character(m)
  sum = 0
  for(j in 1:str_length(m))
  {
    l = substr(m,j,j)
    if(l == "1")
    {
      sum = sum + 1
      ws[i,1] <- ws[i,1] + weight[j]
    }
  }
  irr_count[i,1] = sum
  if(cohort[i,2] == 0)
  {
    ws[i,1] <- ws[i,1]* 1
  }
  else
  {
    ws[i,1] <- ws[i,1]* cohort[i,2]
  }
}
irr <- cbind(irr_count, ws)

write.csv(irr, file = "v9_prr_count_ws.csv", sep =",")
