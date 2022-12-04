## code to prepare `my_dataset` dataset goes here
my_dataset <- read.csv(file="D:/documents/stage/stage g.sapriel/Dossier_NOE/stage 1/Data/matrix_fold_spe_full.csv", header = TRUE,row.names =1,sep="")
usethis::use_data(my_dataset, overwrite = TRUE)
