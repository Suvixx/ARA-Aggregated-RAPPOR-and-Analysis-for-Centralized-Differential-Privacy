cohort <-  read.csv('D:/Rappor/cohort/v4_cohort.csv', header = TRUE , sep = ",")
df <-  read.csv('D:/Rappor/prr_df/V4.csv', header = TRUE , sep = ",")
ws <- matrix(data = rep(0,nrow(df)), nrow = nrow(df), ncol = 1)
weight <- c(1.20201279,1.092738,0.993399, 0.90309, 0.80618, 0.727, 0.66052, 0.60206,
            0.550907, 0.50515, 0.4637573, 0.425969, 0.3912066, 0.3590219,0.329059,
            0.30103)

for(i in 1:nrow(df))
{
  sum = 0
  for(j in 1:ncol(df))
  {
    if(df[i,j] == 1)
    {
      sum = sum + 1
    }
  }
  if(cohort[i,2] == 0)
  {
    ws[i,1] <- sum * weight[sum]
  }
  else
  {
    ws[i,1] <- sum * weight[sum]* cohort[i,2]
  }

}

library(openxlsx)
write.xlsx(ws, 'D:/Rappor/ws_new/v4_a_ws.xlsx', asTable = FALSE)

