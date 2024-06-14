########################################
##### Barrel Treatment assignments #####
########################################

trt <- c("BA_1", "BA_A", "BE_1", "BE_A", "BL_1", "BL_A", "LE_1", "LE_A", "AL_1", "AL_A")
trt <- rep(trt, 16)

barrels <- seq(160)

final <- sample(trt, 160, replace = F)
write.csv(final, file ="barrel treatment ID.csv")

rep_trt <- c("BA_A", "BE_A", "BL_A", "LE_A", "AL_A")
rep_seed <- final[rep_trt]
