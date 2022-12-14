set.seed(1)



data1 <- list(
  array1 = data.frame(matrix(
    round(runif(n=30, min=1, max=20), 0), nrow=5),
    row.names = paste0('row_',1:5)
  ),
  array2 = data.frame(
    matrix(round(runif(n=30, min=1, max=20), 0), nrow=5),
    row.names = paste0('row_',1:5)
  )
  )
colnames(data1$array1) <- paste0('col_',1:6)
colnames(data1$array2) <- paste0('col_',1:6)


###----------------------------------------------
###
data_na <- list(
  array1 = data.frame(matrix(
    round(runif(n=30, min=1, max=20), 0), nrow=5),
    row.names = paste0('row_',1:5)
  ),
  array2 = data.frame(
    matrix(round(runif(n=30, min=1, max=20), 0), nrow=5),
    row.names = paste0('row_',1:5)
    )
)
colnames(data_na$array1) <- paste0('col_',1:6)
colnames(data_na$array2) <- paste0('col_',1:6)

for (i in 1:8)
  data_na$array1[sample.int(5,1), sample.int(6,1)] <- NA


save(data1, file='data1.rda', compress = "xz", compression_level = 9)
save(data_na, file='data_na.rda', compress = "xz", compression_level = 9)

saveRDS(data1, file='data1.RData')
saveRDS(data_na, file='data_na.RData')

