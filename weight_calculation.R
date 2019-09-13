cohort <-  read.csv('/home/sudipta/v10_cohort.csv', header = TRUE , sep = ",")
df <-  read.csv('/home/sudipta/Desktop/irr_count/v10_irr_count.csv', header = TRUE , sep = ",")
ws <- matrix(data = rep(0,nrow(df)), nrow = nrow(df), ncol = 1)
weight <- c(1.20201279,1.092738,0.993399, 0.90309, 0.80618, 0.727, 0.66052, 0.60206,
            0.550907, 0.50515, 0.4637573, 0.425969, 0.3912066, 0.3590219,0.329059,
            0.30103)

for(i in 1:nrow(df))
{
 
  if(cohort[i,2] == 0)
  {
    ws[i,1] <- df[i,2]* weight[df[i,2]]
  }
  else
  {
    ws[i,1] <- df[i,2]* weight[df[i,2]]* cohort[i,2]
  }

}


write.csv(ws, 'v10_irr_ws.csv', sep = ",")

