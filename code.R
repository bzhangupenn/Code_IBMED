################################################################################
################################################################################
# Smooth digitized .csv data; see ``smooth_extracted_curve.R"

################################################################################
################################################################################
# Convert smoothed digitized .csv data to functional data objects

################################################################################
# Malawi LMCA
dir = '/Users/bzhan/Dropbox/TCD project/2018-2020 Day1 Malawi Digitized (smoothed)/'
setwd(dir)

LMCA_all = NULL
for (folder in list.files()[1:length(list.files())]){
  LMCA = read.csv(paste(dir, folder, '/', folder, '_LMCA.csv', sep = ''))
  LMCA_all = cbind(LMCA_all, LMCA$v)
}

t_range = c(0,5)
fbasis <- create.bspline.basis(t_range, nbasis=50)
fd = Data2fd(seq(0, 5, length.out = 500), LMCA_all, basisobj = fbasis)
LMCA_all = eval.fd(fd, seq(0, 5, length.out = 500))

# Save adjusted curves to a RDS file
fd_left = Data2fd(seq(0, 5, length.out = 500), LMCA_all, basisobj = fbasis)
saveRDS(fd_left, '/Users/bzhan/Dropbox/TCD project/four cycle Malawi/fd_four_cycle_Malawi_left.rds')

################################################################################
# Malawi Right
dir = '/Users/bzhan/Dropbox/TCD project/2018-2020 Day1 Malawi Digitized (smoothed)/'
setwd(dir)

RMCA_all = NULL
for (folder in list.files()[1:length(list.files())]){
  cat(folder, '\n')
  RMCA = read.csv(paste(dir, folder, '/', folder, '_RMCA.csv', sep = ''))
  RMCA_all = cbind(RMCA_all, RMCA$v)
}

t_range = c(0,5)
fbasis <- create.bspline.basis(t_range, nbasis=50)
fd = Data2fd(seq(0, 5, length.out = 500), RMCA_all, basisobj = fbasis)
RMCA_all = eval.fd(fd, seq(0, 5, length.out = 500))

# Save adjusted curves to a RDS file
fd_right = Data2fd(seq(0, 5, length.out = 500), RMCA_all, basisobj = fbasis)
saveRDS(fd_right, '/Users/bzhan/Dropbox/TCD project/four cycle Malawi/fd_four_cycle_Malawi_right.rds')

################################################################################
# Malawi BA
dir = '/Users/bzhan/Dropbox/TCD project/2018-2020 Day1 Malawi Digitized (smoothed)/'
setwd(dir)

BA_all = NULL
for (folder in list.files()[1:length(list.files())]){
  cat(folder, '\n')
  BA = read.csv(paste(dir, folder, '/', folder, '_BA.csv', sep = ''))
  BA_all = cbind(BA_all, BA$v)
}

t_range = c(0,5)
fbasis <- create.bspline.basis(t_range, nbasis=50)
fd = Data2fd(seq(0, 5, length.out = 500), BA_all, basisobj = fbasis)
BA_all = eval.fd(fd, seq(0, 5, length.out = 500))

# Save adjusted curves to a RDS file
fd_ba = Data2fd(seq(0, 5, length.out = 500), BA_all, basisobj = fbasis)
saveRDS(fd_ba, '/Users/bzhan/Dropbox/TCD project/four cycle Malawi/fd_one_cycle_Malawi_ba.rds')


################################################################################
# Normal LMCA/RMCA curves: first batch 29 curves

dir = '/Users/bzhan/Dropbox/TCD project/added-normal curves (smooth)/'
setwd(dir)
LMCA_all = NULL
for (folder in list.files()[1:length(list.files())]){
  LMCA = read.csv(paste(dir, folder, '/', folder, '_LMCA.csv', sep = ''))
  LMCA_all = cbind(LMCA_all, LMCA$v)
}

t_range = c(0,5)
fbasis <- create.bspline.basis(t_range, nbasis=50)
fd = Data2fd(seq(0, 5, length.out = 500), LMCA_all, basisobj = fbasis)
LMCA_all = eval.fd(fd, seq(0, 5, length.out = 500))

# Save adjusted curves to a RDS file
fd_left = Data2fd(seq(0, 5, length.out = 500), LMCA_all, basisobj = fbasis)
saveRDS(fd_left, '/Users/bzhan/Dropbox/TCD project/four cycle normal/fd_four_cycle_normal_left.rds')

# Save adjusted curves to a RDS file
fd_right = Data2fd(seq(0, 5, length.out = 500), LMCA_all, basisobj = fbasis)
saveRDS(fd_right, '/Users/bzhan/Dropbox/TCD project/four cycle normal/fd_four_cycle_normal_right.rds')


################################################################################
# Normal LMCA/RMCA curves: second batch 49 curves

dir = '/Users/bzhan/Dropbox/TCD project/added-normal curves (smooth 2)/'
setwd(dir)
LMCA_all = NULL
for (folder in list.files()[1:length(list.files())]){
  cat(folder, '\n')
  LMCA = read.csv(paste(dir, folder, '/', folder, '_LMCA.csv', sep = ''))
  LMCA_all = cbind(LMCA_all, LMCA$v)
}

t_range = c(0,5)
fbasis <- create.bspline.basis(t_range, nbasis=50)
fd = Data2fd(seq(0, 5, length.out = 500), LMCA_all, basisobj = fbasis)
LMCA_all = eval.fd(fd, seq(0, 5, length.out = 500))

# Save adjusted curves to a RDS file
fd_left = Data2fd(seq(0, 5, length.out = 500), LMCA_all, basisobj = fbasis)
saveRDS(fd_left, '/Users/bzhan/Dropbox/TCD project/four cycle normal/fd_four_cycle_normal_left_2.rds')

# Save adjusted curves to a RDS file
fd_right = Data2fd(seq(0, 5, length.out = 500), LMCA_all, basisobj = fbasis)
saveRDS(fd_right, '/Users/bzhan/Dropbox/TCD project/four cycle normal/fd_four_cycle_normal_right_2.rds')


################################################################################
# Normal BA curves
dir = '/Users/bzhan/Dropbox/TCD project/added-normal BA curves (smooth)/'
setwd(dir)
BA_all = NULL
for (folder in list.files()[1:length(list.files())]){
  cat(folder, '\n')
  BA = read.csv(paste(dir, folder, '/', folder, '_BA.csv', sep = ''))
  BA_all = cbind(BA_all, BA$v)
}

t_range = c(0,5)
fbasis <- create.bspline.basis(t_range, nbasis=50)
fd = Data2fd(seq(0, 5, length.out = 500), BA_all, basisobj = fbasis)
BA_all = eval.fd(fd, seq(0, 5, length.out = 500))

# Save adjusted curves to a RDS file
fd_ba = Data2fd(seq(0, 5, length.out = 500), BA_all, basisobj = fbasis)
saveRDS(fd_ba, '/Users/bzhan/Dropbox/TCD project/four cycle normal/fd_one_cycle_ba.rds')


################################################################################
################################################################################

# For LMCA/RMCA data, we first extracted four cycles for each individual
# For code that extracts one cycle from four cycles, see ``extract_one_cycle.R"

# For one-cycle BA curves, we also use ``extract_one_cycle.R" to adjust
# left and right endpoints.

################################################################################
################################################################################
# Curve registration: aligning 98 (Malawi) + 78 (normal) LMCA/RMCA curves 
# using landmark registration
################################################################################

# Read in 98 Malawi LMCA/RMCA curves
M_left = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_left_one_cycle.rds')
M_right = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_right_one_cycle.rds')
M_ba = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_one_cycle_Malawi_ba.rds')




# Add a name to each LMCA/RMCA/BA curve
dir = '/Users/bzhan/Dropbox/TCD project/2018-2020 Day1 Malawi Digitized (smoothed)/'
M_left$fdnames$reps = list.files(dir)
M_right$fdnames$reps = list.files(dir)
M_ba$fdnames$reps = list.files(dir)

# "MP3481" "MP3482" "MP3483" "MP3484" "MP3485" "MP3486" "MP3489" "MP3491" "MP3493" "MP3495" "MP3497"
# "MP3498" "MP3499" "MP3505" "MP3507" "MP3508" "MP3509" "MP3510" "MP3511" "MP3512" "MP3513" "MP3514"
# "MP3515" "MP3517" "MP3518" "MP3519" "MP3520" "MP3521" "MP3522" "MP3523" "MP3524" "MP3525" "MP3526"
# "MP3527" "MP3528" "MP3529" "MP3530" "MP3531" "MP3532" "MP3533" "MP3534" "MP3535" "MP3536" "MP3537"
# "MP3538" "MP3539" "MP3540" "MP3541" "MP3542" "MP3543" "MP3545" "MP3546" "MP3547" "MP3548" "MP3549"
# "MP3550" "MP3551" "MP3552" "MP3553" "MP3554" "MP3555" "MP3556" "MP3557" "MP3558" "MP3559" "MP3560"
# "MP3561" "MP3563" "MP3564" "MP3565" "MP3566" "MP3567" "MP3568" "MP3569" "MP3570" "MP3571" "MP3572"
# "MP3573" "MP3574" "MP3575" "MP3576" "MP3577" "MP3578" "MP3579" "MP3580" "MP3581" "MP3582" "MP3583"
# "MP3584" "MP3585" "MP3586" "MP3587" "MP3588" "MP3589" "MP3590" "MP3591" "MP3592" "MP3593"

# Read in 29 normal LMCA/RMCA curves
normal_left = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle normal/fd_normal_left_one_cycle.rds')
normal_right = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle normal/fd_normal_right_one_cycle.rds')

# Read in 49 additional normal LMCA/RMCA curves
normal_left_2 = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle normal/fd_normal_left_one_cycle_2.rds')
normal_right_2 = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle normal/fd_normal_right_one_cycle_2.rds')

# Pool 98 + 49 + 29 LMCA/RMCA curves together
M_left$coefs = cbind(M_left$coefs, normal_left$coefs, normal_left_2$coefs)
M_left$fdnames$reps = c(M_left$fdnames$reps, rep('Normal', 78))
M_right$coefs = cbind(M_right$coefs, normal_right$coefs, normal_right_2$coefs)
M_right$fdnames$reps = c(M_right$fdnames$reps, rep('Normal', 78))


# Read in 32 normal BA curves
normal_ba = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle normal/fd_normal_one_cycle_ba.rds')
M_ba$coefs = cbind(M_ba$coefs, normal_ba$coefs)
M_ba$fdnames$reps = c(M_ba$fdnames$reps, rep('Normal', 32))


# Register LMCA first
landmark = NULL
for (i in 1:(98+29+49)){
  plot(M_left[i])
  landmark = rbind(landmark, locator(1)$x)
}

# One dropped during LMCA registration
landmark = landmark[-53,]
M_left = M_left[-53]
landmark_w_end = cbind(0.01, landmark, 4.99)

# Landmark registration
fd_reg_obj = landmarkreg(fdobj =  M_left, ximarks = landmark_w_end,
                         WfdPar = create.bspline.basis(c(0,5), nbasis = 4),
                         monwrd = FALSE)

# Obtain registered curves and save them (Malawi LMCA plus normal)
# to 'TCD project/one cycle Malawi/fd_Malawi_and_normal_reg_left.rds'
M_left_reg = fd_reg_obj$regfd
saveRDS(M_left_reg, 
        '/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_and_normal_reg_left.rds')


# Register RMCA

# Delte 53 
M_right = M_right[-c(53)]

landmark = NULL
for (i in 1:(97+29+49)){
  plot(M_right[i])
  landmark = rbind(landmark, locator(1)$x)
}

# Include two endpoints
landmark_w_end = cbind(0.01, landmark, 4.99)

# Landmark registration
fd_reg_obj = landmarkreg(fdobj =  M_right, ximarks = landmark_w_end,
                         WfdPar = create.bspline.basis(c(0,5), nbasis = 4),
                         monwrd = FALSE)

# Obtain registered curves and save them (Malawi LMCA plus normal)
# to 'TCD project/one cycle Malawi/fd_Malawi_and_normal_reg_right.rds'
M_right_reg = fd_reg_obj$regfd
saveRDS(M_right_reg, 
        '/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_and_normal_reg_right.rds')



# Register BA

# Delte 53 
M_ba = M_ba[-c(53)]

landmark = NULL
for (i in 1:(97+32)){
  plot(M_ba[i])
  landmark = rbind(landmark, locator(1)$x)
}

# Include two endpoints
landmark_w_end = cbind(0.01, landmark, 4.99)

# Landmark registration
fd_reg_obj = landmarkreg(fdobj =  M_ba, ximarks = landmark_w_end,
                         WfdPar = create.bspline.basis(c(0,5), nbasis = 4),
                         monwrd = FALSE)

# Obtain registered curves and save them (Malawi LMCA plus normal)
# to 'TCD project/one cycle Malawi/fd_Malawi_and_normal_reg_right.rds'
M_ba_reg = fd_reg_obj$regfd
saveRDS(M_ba_reg, 
        '/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_and_normal_reg_ba.rds')




##############################
# Make plots
M_left_reg = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_left_reg.rds')
M_right_reg = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_right_reg.rds')
M_ba_reg = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_ba_reg.rds')


par(mfcol = c(2,3), mar = c(0.5,0.5,2,0.5), oma = c(2,5,0,0))
plot(fdata(M_left[-53]), ylim = c(10, 220), xaxt = 'n', yaxt = 'n', 
     main = 'LMCA Before Registration', cex.main = 1.5)
axis(2, at = seq(50, 200, 50),
     labels = seq(50, 200, 50), cex.axis = 1.2)
mtext('Velocity (cm/s)', side=2, cex = 1.5, line = 3)
box()

plot(fdata(M_left_reg), ylim = c(10, 220), xaxt = 'n', yaxt = 'n', 
     main = 'LMCA After Registration', cex.main = 1.5)
axis(2, at = seq(50, 200, 50),
     labels = seq(50, 200, 50), cex.axis = 1.2)
mtext('Velocity (cm/s)', side=2, cex = 1.5, line = 3)
box()

plot(fdata(M_right[-53]), ylim = c(10, 220), xaxt = 'n', yaxt = 'n',
     main = 'RMCA Before Registration', cex.main = 1.5)

plot(fdata(M_right_reg), ylim = c(10, 220), xaxt = 'n', yaxt = 'n', 
     main = 'RMCA After Registration', cex.main = 1.5)


plot(fdata(-M_ba[-53]), ylim = c(10, 220), yaxt = 'n', xaxt = 'n',
     main = 'BA Before Registration', cex.main = 1.5)

plot(fdata(-M_ba_reg), ylim = c(10, 220), yaxt = 'n', xaxt = 'n',
     main = 'BA After Registration', cex.main = 1.5)



################################################################################
# Obtain 78 normal LMCA curves after registration
normal_left_reg = M_left_reg[which(M_left_reg$fdnames$reps == 'Normal')]

# Obtain 97 Malawi LMCA curves after registration
Malawi_left_reg = M_left_reg[which(M_left_reg$fdnames$reps != 'Normal')]

# Obtain 32 normal BA curves after registration
normal_ba_reg = M_ba_reg[which(M_ba_reg$fdnames$reps == 'Normal')]

# Obtain 97 Malawi BA curves after registration
Malawi_ba_reg = M_ba_reg[which(M_ba_reg$fdnames$reps != 'Normal')]


# Obtain 78 normal LMCA curves after registration
normal_right_reg = M_right_reg[which(M_right_reg$fdnames$reps == 'Normal')]

# Obtain 97 Malawi RMCA curves after registration
Malawi_right_reg = M_right_reg[which(M_right_reg$fdnames$reps != 'Normal')]

# Save results separately
saveRDS(normal_left_reg, 
        '/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_normal_reg.rds')
saveRDS(normal_ba_reg, 
        '/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_normal_ba_reg.rds')
saveRDS(Malawi_left_reg, 
        '/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_left_reg.rds')
saveRDS(Malawi_right_reg, 
        '/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_right_reg.rds')
saveRDS(Malawi_ba_reg, 
        '/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_ba_reg.rds')




################################################################################
################################################################################
# Plot descriptive statistics
################################################################################
################################################################################

# Read in registered Malawi LMCA/RMCA curves
M_left = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_left_reg.rds')
M_right = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_right_reg.rds')
M_ba = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_ba_reg.rds')
M_ba = -M_ba


par(mfrow = c(2,3), mar = c(2,2,2,2), oma = c(4,4,0,0))
plot(fdata(mean.fd(M_left)), main = 'LMCA Mean', ylim = c(40, 130),
     lwd = 2, xaxt = 'n', cex.main = 2.5, cex.axis = 2.5)
mtext("Velocity (cm/s)", side=2, cex = 2, line = 4)
box()

plot(fdata(mean.fd(M_right)), main = 'RMCA Mean', ylim = c(40, 130),
     lwd = 2, xaxt = 'n', cex.main = 2.5, cex.axis = 2.5)
box()

plot(fdata(mean.fd(M_ba)), main = 'BA Mean', ylim = c(40, 130),
     lwd = 2, xaxt = 'n', cex.main = 2.5, cex.axis = 2.5)
box()

plot(fdata(std.fd(M_left)), main = 'LMCA SD', ylim = c(15, 35),
     lwd = 2, xaxt = 'n', cex.main = 2.5, cex.axis = 2.5)
mtext("Velocity (cm/s)", side=2, cex = 2, line = 4)
box()

plot(fdata(std.fd(M_right)), main = 'RMCA SD', ylim = c(15, 35),
     lwd = 2, xaxt = 'n', cex.main = 2.5, cex.axis = 2.5)
box()

plot(fdata(std.fd(M_ba)), main = 'BA SD', ylim = c(15, 35),
     lwd = 2, xaxt = 'n', cex.main = 2.5, cex.axis = 2.5)
box()



################################################################################
################################################################################
# FPCA
################################################################################
################################################################################
# Read in registered Malawi LMCA/RMCA curves
M_left = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_left_reg.rds')
M_right = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_right_reg.rds')
M_ba = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_ba_reg.rds')
M_ba = -M_ba

# LMCA
pca_left = pca.fd(M_left, nharm = 3)
par(mfrow = c(3,1))
plot(pca_left, cex.main = 2.5, xlab = "", xaxt = 'n', 
     cex.axis = 1.5, cex.lab = 1.3)
# RMCA
pca_right = pca.fd(M_right, nharm = 3)
par(mfrow = c(3,1))
plot(pca_right, cex.main = 2.5, xlab = "", xaxt = 'n', 
     cex.axis = 1.5, cex.lab = 1.3)


# BA
pca_ba = pca.fd(M_ba, nharm = 3)
par(mfrow = c(3,1))
plot(pca_ba, cex.main = 2.5,
     xlab = "", xaxt = 'n', cex.axis = 1.5, cex.lab = 1.3)



################################################################################
################################################################################
# STANDARDIZE LEFT AND RIGHT AND BA MALAWI CURVES
################################################################################
################################################################################
# From fd_Malawi_left_reg.rds & fd_normal_reg.rds to 
# age-standardized fd_Malawi_left_reg and fd_Malawi_right_reg 
################################################################################
# Helper functions
standardize_curves <- function(M, mean_fd, sd_fd){
  M_mat = eval.fd(M, seq(0, 5, length.out = 500))
  normal_mean_mat = eval.fd(mean_fd,
                            seq(0, 5, length.out = 500))
  sd_mat = eval.fd(sd_fd, seq(0, 5, length.out = 500))
  
  M_mat_std = apply(M_mat, 2, function(x, a, b) {x - a}/b, 
                    a = normal_mean_mat, b = sd_mat)
  
  # Concert matrix to fd objects
  fbasis <- create.bspline.basis(c(0, 5), nbasis=50)
  M_std = Data2fd(seq(0, 5, length.out = 500), M_mat_std, basisobj = fbasis)
  return(M_std)
}

library(fda)
# Age of Malawi kids
age = c(60, 6, 56, 44, 96, 78, 62, 71, 32, 74, 25,
        35, 26, 65, 72, 71, 57, 57, 36, 77, 108, 45,
        26, 29, 85, 49, 90, 35, 81, 72, 51, 46, 6,
        27, 25, 98, 78, 73, 65, 27, 120, 65, 68, 36,
        124, 22, 95, 42, 77, 74, 54, 120, 70, 95,
        38, 29, 41, 31, 11, 31, 79, 144, 44, 39, 11,
        86, 58, 40, 27, 84, 94, 42, 128, 41, 74, 13,
        57, 45, 82, 15, 26, 42, 110, 66, 18, 51, 32,
        78, 109, 26, 53, 76, 53, 63, 55, 115, 47)

age = floor(age/12)
Malawi_age_by_group = cut(age, breaks = c(0,2,3,4,5,6,8,10, 15), 
                          include.lowest = TRUE)

#Malawi_age_by_group
#[0,1]   (1,2]   (2,3]   (3,4]   (4,5]   (5,6]   (6,7]   (7,8]   (8,9]  (9,10] (10,15]
#  8      17      16      12      11      15       7       2       4       4       1

# Read in registered Malawi LMCA/RMCA curves
M_left = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_left_reg.rds')
M_right = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_right_reg.rds')
M_ba = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_ba_reg.rds')
M_ba = -M_ba

# Read in registered normal LMCA/RMCA/BA curves
# We have 29 + 49 = 78 normal LMCA/RMCA curves in total
# We do not differentiate left or right
# We have 32 normal BA curves

normal_left = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_normal_reg.rds')
normal_right = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_normal_reg.rds')
normal_ba = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_normal_ba_reg.rds')
normal_ba = -normal_ba


# Age of the first batch of 29 normal curves
age_normal = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 0, 1, 2.5, 2.9, 2.9, 3.5, 3,
               4, 5, 5, 6, 6, 8, 10, 11, 12, 12, 13)

# Age of the second batch of 49 normal curves
age_normal_2 = c(rep(3.5, 3), rep(3, 14), rep(4.5, 13), rep(4, 6),
                 rep(5, 6), rep(6, 4), rep(12, 3))

age_normal_all = c(age_normal, age_normal_2)

age_normal_ba = c(rep(2,3), rep(3,4), rep(4,4), rep(5,4),
                  rep(6,4), rep(7,4), rep(8,4), rep(9,2), rep(10,3))

# Take a look at age groups
age_by_group = cut(age_normal_all, breaks = c(0,2,3,4,5,6,8,10, 15), 
                   include.lowest = TRUE)

################################################################################
# Compute the normal mean LMCA/RMCA curves for each age bracket
normal_LMCA_02 = normal_left[which(age_by_group %in% c('[0,2]'))]
normal_LMCA_mean_02 = mean.fd(normal_LMCA_02)

normal_LMCA_23 = normal_left[which(age_by_group %in% c('(2,3]'))]
normal_LMCA_mean_23 = mean.fd(normal_LMCA_23)

normal_LMCA_34 = normal_left[which(age_by_group %in% c('(3,4]'))]
normal_LMCA_mean_34 = mean.fd(normal_LMCA_34)

normal_LMCA_45 = normal_left[which(age_by_group %in% c('(4,5]'))]
normal_LMCA_mean_45 = mean.fd(normal_LMCA_45)

normal_LMCA_56 = normal_left[which(age_by_group %in% c('(5,6]'))]
normal_LMCA_mean_56 = mean.fd(normal_LMCA_56)

normal_LMCA_68 = normal_left[which(age_by_group %in% c('(6,8]'))]
normal_LMCA_mean_68 = mean.fd(normal_LMCA_68)

normal_LMCA_810 = normal_left[which(age_by_group %in% c('(8,10]'))]
normal_LMCA_mean_810 = mean.fd(normal_LMCA_810)

normal_LMCA_above10 = normal_left[which(age_by_group %in% c('(10,15]'))]
normal_LMCA_mean_above10 = mean.fd(normal_LMCA_above10)

# Put normal LMCA/RMCA curves in a list
normal_mean_curves = list('[0,2]' = normal_LMCA_mean_02,
                          '(2,3]' = normal_LMCA_mean_23,
                          '(3,4]' = normal_LMCA_mean_34,
                          '(4,5]' = normal_LMCA_mean_45,
                          '(5,6]' = normal_LMCA_mean_56,
                          '(6,8]' = normal_LMCA_mean_68,
                          '(8,10]' = normal_LMCA_mean_810,
                          '(10,15]' = normal_LMCA_mean_above10)

# Pool all normal curves together and compute a sd curve
sd_LMCA = sd.fd(normal_left)

################################################################################
# Compute the normal mean BA curves for each age bracket
age_by_group_ba = cut(age_normal_ba, breaks = c(0,2,3,4,5,6,8,10, 15), 
                   include.lowest = TRUE)

# Compute the normal mean curves for each age bracket
normal_BA_02 = normal_ba[which(age_by_group_ba %in% c('[0,2]'))]
normal_BA_mean_02 = mean.fd(normal_BA_02)

normal_BA_23 = normal_ba[which(age_by_group_ba %in% c('(2,3]'))]
normal_BA_mean_23 = mean.fd(normal_BA_23)

normal_BA_34 = normal_ba[which(age_by_group_ba %in% c('(3,4]'))]
normal_BA_mean_34 = mean.fd(normal_BA_34)

normal_BA_45 = normal_ba[which(age_by_group_ba %in% c('(4,5]'))]
normal_BA_mean_45 = mean.fd(normal_BA_45)

normal_BA_56 = normal_ba[which(age_by_group_ba %in% c('(5,6]'))]
normal_BA_mean_56 = mean.fd(normal_BA_56)

normal_BA_68 = normal_ba[which(age_by_group_ba %in% c('(6,8]'))]
normal_BA_mean_68 = mean.fd(normal_BA_68)

normal_BA_810 = normal_ba[which(age_by_group_ba %in% c('(8,10]'))]
normal_BA_mean_810 = mean.fd(normal_BA_810)

#normal_BA_above10 = normal_ba[which(age_by_group_ba %in% c('(10,15]'))]
#normal_BA_mean_above10 = mean.fd(normal_BA_above10)

# Put normal BA curves in a list
normal_mean_ba_curves = list('[0,2]' = normal_BA_mean_02,
                          '(2,3]' = normal_BA_mean_23,
                          '(3,4]' = normal_BA_mean_34,
                          '(4,5]' = normal_BA_mean_45,
                          '(5,6]' = normal_BA_mean_56,
                          '(6,8]' = normal_BA_mean_68,
                          '(8,10]' = normal_BA_mean_810,
                          '(10,15]' = normal_BA_mean_810)

# Pool all normal BA curves together and compute a sd curve
sd_BA = sd.fd(normal_ba)


# Standardize 97 Malawi LMCA curves
M_left_st = M_left
new_coef = NULL
for (i in 1:97){
  temp = standardize_curves(M_left[i], 
         normal_mean_curves[[Malawi_age_by_group[i]]], sd_LMCA)
  new_coef = cbind(new_coef, temp$coefs)
}
M_left_st$coefs = new_coef


# Standardize 97 Malawi RMCA curves
M_right_st = M_right
new_coef = NULL
for (i in 1:97){
  temp = standardize_curves(M_right[i], 
         normal_mean_curves[[Malawi_age_by_group[i]]], sd_LMCA)
  new_coef = cbind(new_coef, temp$coefs)
}
M_right_st$coefs = new_coef


# Standardize 97 Malawi BA curves
M_ba_st = M_ba
new_coef = NULL
for (i in 1:97){
  cat(i, '\n')
  temp = standardize_curves(M_ba[i], 
                            normal_mean_ba_curves[[Malawi_age_by_group[i]]], sd_BA)
  plot(temp)
  new_coef = cbind(new_coef, temp$coefs)
}
M_ba_st$coefs = new_coef


library(fda.usc)
# Plot 97 curves after standardization
# This is Figure 13
#par(mfrow = c(1,2), mar = c(1.5,1.5,1.5,1.5), oma = c(2,2,0,0))
#plot(fdata(M_left_st), main = '', ylim = c(-5, 8),
#     lwd = 2, axes = FALSE)
#axis(2, at = c(-4, -2, 0, 2, 4, 6, 8), 
#     labels = c(-4, -2, 0, 2, 4, 6, 8), cex.axis = 2.5)
#box()

#plot(fdata(M_right_st), main = '', ylim = c(-5, 8),
#     lwd = 2, axes = FALSE)
#axis(2, at = c(-4, -2, 0, 2, 4, 6, 8), 
#     labels = c(-4, -2, 0, 2, 4, 6, 8), cex.axis = 2.5)
#box()

# Plot Malawi LMCA/RMCA/BA before standardization
par(mfrow = c(1,3), mar = c(2,2,2,2), oma = c(2,2,0,0))
plot(fdata(M_left), main = 'LMCA', ylim = c(0, 250),
     lwd = 2, axes = FALSE, cex.main = 2.5)
axis(2, at = c(0, 50, 100, 150, 200, 250), 
     labels = c(0, 50, 100, 150, 200, 250), cex.axis = 2.5)
box()

plot(fdata(M_right), main = 'RMCA', ylim = c(0, 250),
     lwd = 2, axes = FALSE, cex.main = 2.5)
axis(2, at = c(0, 50, 100, 150, 200, 250), 
     labels = c(0, 50, 100, 150, 200, 250), cex.axis = 2.5)
box()

plot(fdata(M_ba), main = 'BA', ylim = c(0, 200),
     lwd = 2, axes = FALSE, cex.main = 2.5)
axis(2, at = c(0, 50, 100, 150, 200), 
     labels = c(0, 50, 100, 150, 200), cex.axis = 2.5)
box()


# Plot Malawi LMCA/RMCA/BA after standardization
par(mfrow = c(1,3), mar = c(2,2,2,2), oma = c(2,2,0,0))
plot(fdata(M_left_st), main = 'LMCA', ylim = c(-6, 10),
     lwd = 2, axes = FALSE, cex.main = 2.5)
axis(2, at = c(-6, -4, -2, 0, 2, 4, 6, 8, 10), 
     labels = c(-6, -4, -2, 0, 2, 4, 6, 8, 10), cex.axis = 2.5)
box()

plot(fdata(M_right_st), main = 'RMCA', ylim = c(-6, 10),
     lwd = 2, axes = FALSE, cex.main = 2.5)
axis(2, at = c(-6, -4, -2, 0, 2, 4, 6, 8, 10), 
     labels = c(-6, -4, -2, 0, 2, 4, 6, 8, 10), cex.axis = 2.5)
box()

plot(fdata(M_ba_st), main = 'BA', ylim = c(-6, 10),
     lwd = 2, axes = FALSE, cex.main = 2.5)
axis(2, at = c(-6, -4, -2, 0, 2, 4, 6, 8, 10), 
     labels = c(-6, -4, -2, 0, 2, 4, 6, 8, 10), cex.axis = 2.5)
box()

####################################################
# Save standardized LMCA/RMCA/BA curves
saveRDS(M_left_st, 
        '/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_LMCA_st.rds')
saveRDS(M_right_st, 
        '/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_RMCA_st.rds')
saveRDS(M_ba_st, 
        '/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_BA_st.rds')


M_left_st = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_LMCA_st.rds')
M_right_st = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_RMCA_st.rds')
M_ba_st = readRDS('/Users/bzhan/Dropbox/TCD project/one cycle Malawi/fd_Malawi_BA_st.rds')







################################################################################
################################################################################
# CLUSTER 97 Malawi LMCA curves using the funFEM algorithm
################################################################################
################################################################################
library(funFEM)
res = funFEM(M_left_st, K=7, model = 'DBk')
plot(2:7,res$plot$bic,type='b',xlab='Number of cluster', ylab = 'BIC')
plot(2:7,res$plot$ll,type='b',xlab='K',main='loglik')

################################################################################
# Plot cluster 4 to illustrate
# This is Figure 14
par(mfrow=c(1,2), mar = c(0.5,0.5,0.5,0.5), oma = c(3,3,0,0))
plot(fdata(M_left_st[res$cls == 4]), 
     lwd = 2, main = '',
     ylim = c(-4, 4), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4), labels = c(-4, -2, 0, 2, 4), cex.axis = 2.5)
box()
plot(fdata(mean.fd(M_left_st[res$cls == 4])), 
     lwd = 3, main = '', col = 'red',
     ylim = c(-4, 4), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()

################################################################################
# Plot seven clusters together
# This is Figure 15
par(mfrow = c(3, 3), mar = c(0.5,0.5,0.5,0.5),
    oma = c(3,3,1,1))
plot(fdata(M_left_st[res$cls == 1]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4, 6), labels = c(-4, -2, 0, 2, 4, 6), cex.axis = 3)
box()
plot(fdata(M_left_st[res$cls == 2]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, labels = FALSE)
box()
plot(fdata(M_left_st[res$cls == 3]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, labels = FALSE)
box()
plot(fdata(M_left_st[res$cls == 4]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4, 6), labels = c(-4, -2, 0, 2, 4, 6), cex.axis = 3)
box()
plot(fdata(M_left_st[res$cls == 5]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, labels = FALSE)
box()
plot(fdata(M_left_st[res$cls == 6]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, labels = FALSE)
box()
plot(fdata(M_left_st[res$cls == 7]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4, 6), labels = c(-4, -2, 0, 2, 4, 6), cex.axis = 3)
box()

################################################################################
# Plot seven cluster means together
# This is Figure 16
par(mfrow = c(3, 3), mar = c(0.5,0.5,0.5,0.5), 
    oma = c(3,3,0,0))
plot(fdata(mean.fd(M_left_st[res$cls == 1])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4), labels = c(-4, -2, 0, 2, 4), cex.axis = 3)
box()
plot(fdata(mean.fd(M_left_st[res$cls == 2])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(fdata(mean.fd(M_left_st[res$cls == 3])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(fdata(mean.fd(M_left_st[res$cls == 4])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4), labels = c(-4, -2, 0, 2, 4), cex.axis = 3)
box()
plot(fdata(mean.fd(M_left_st[res$cls == 5])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(fdata(mean.fd(M_left_st[res$cls == 6])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(fdata(mean.fd(M_left_st[res$cls == 7])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4), labels = c(-4, -2, 0, 2, 4), cex.axis = 3)
box()

################################################################################
################################################################################
# CLUSTER 97 Malawi RMCA curves using the funFEM algorithm
################################################################################
################################################################################
library(funFEM)
res2 = funFEM(M_right_st, K=7, model = 'DBk')
plot(2:7,res$plot$bic,type='b',xlab='Number of cluster', ylab = 'BIC')
plot(2:7,res$plot$ll,type='b',xlab='K',main='loglik')

################################################################################
# Plot seven clusters together
# This is Figure 17
par(mfrow = c(3, 3), mar = c(0.5,0.5,0.5,0.5),
    oma = c(3,3,1,1))
plot(fdata(M_right_st[res2$cls == 1]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4, 6), labels = c(-4, -2, 0, 2, 4, 6), cex.axis = 3)
box()
plot(fdata(M_right_st[res2$cls == 2]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, labels = FALSE)
box()
plot(fdata(M_right_st[res2$cls == 3]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, labels = FALSE)
box()
plot(fdata(M_right_st[res2$cls == 4]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4, 6), labels = c(-4, -2, 0, 2, 4, 6), cex.axis = 3)
box()
plot(fdata(M_right_st[res2$cls == 5]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(fdata(M_right_st[res2$cls == 6]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, labels = FALSE)
box()
plot(fdata(M_right_st[res2$cls == 7]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4, 6), labels = c(-4, -2, 0, 2, 4, 6), cex.axis = 3)
box()

################################################################################
# Plot 
par(mfrow = c(3, 3), mar = c(0.5,0.5,0.5,0.5),
    oma = c(3,3,1,1))
layout( matrix(c(0, 1, 1, 1, 0, 2, 2, 2, 0,
                 0, 3, 3, 3, 0, 4, 4, 4, 0,
                 0, 5, 5, 5, 6, 6, 6, 6, 6,
                 0, 7, 7, 7, 0, 8, 8, 8, 0,
                 9, 9, 9, 9, 9, 9, 9, 9, 9,
                 10, 10, 10, 11, 11, 11, 12, 12, 12), nrow=6, byrow=TRUE),
              heights = c(1,3,1,3,1,3))

plot.new()
text(0.5,0.5,"Normal",cex=2,font=2)

plot.new()
text(0.5,0.5,"Microvascular obstruction",cex=2,font=2)


plot(fdata(M_right_st[res2$cls == 6]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4, 6), labels = c(-4, -2, 0, 2, 4, 6), cex.axis = 3)
box()


plot(fdata(M_right_st[res2$cls == 3]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4, 6), labels = c(-4, -2, 0, 2, 4, 6), cex.axis = 3)
box()

plot.new()
text(0.5,0.5,"Hyperemia/RMCA vasospasm",cex=2,font=2)

plot.new()
text(0.5,0.5,"Hyperemia/Isolated posterior hyperemia",cex=2,font=2)


plot(fdata(M_right_st[res2$cls == 4]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4, 6), labels = c(-4, -2, 0, 2, 4, 6), cex.axis = 3)
box()


plot(fdata(M_right_st[res2$cls == 1]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4, 6), labels = c(-4, -2, 0, 2, 4, 6), cex.axis = 3)
box()


plot.new()
text(0.5,0.5,"Low flow",cex=2,font=2)

plot(fdata(M_right_st[res2$cls == 2]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4, 6), labels = c(-4, -2, 0, 2, 4, 6), cex.axis = 3)
box()
plot(fdata(M_right_st[res2$cls == 5]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, labels = FALSE)
box()
plot(fdata(M_right_st[res2$cls == 7]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, labels = FALSE)
box()



################################################################################
# Plot seven cluster means together
# This is Figure 18
par(mfrow = c(3, 3), mar = c(0.5,0.5,0.5,0.5), 
    oma = c(3,3,0,0))
plot(fdata(mean.fd(M_right_st[res2$cls == 1])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4), labels = c(-4, -2, 0, 2, 4), cex.axis = 3)
box()
plot(fdata(mean.fd(M_right_st[res2$cls == 2])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(fdata(mean.fd(M_right_st[res2$cls == 3])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(fdata(mean.fd(M_right_st[res2$cls == 4])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4), labels = c(-4, -2, 0, 2, 4), cex.axis = 3)
box()
plot(fdata(mean.fd(M_right_st[res2$cls == 5])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(fdata(mean.fd(M_right_st[res2$cls == 6])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(fdata(mean.fd(M_right_st[res2$cls == 7])), ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-4, -2, 0, 2, 4), labels = c(-4, -2, 0, 2, 4), cex.axis = 3)
box()

################################################################################
# Pool LMCA and RMCA together
# L1 & R5; L2 & R2; L3 & R6;
# L4; L5 & R3; R1 & L6
# L7 & R4; R7

par(mfrow = c(3, 3), mar = c(0.5,0.5,0.5,0.5),
    oma = c(3,3,1,1))
plot(c(fdata(M_left_st[res$cls == 1]), fdata(M_right_st[res2$cls == 5])), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-2, 0, 2), labels = c(-2, 0, 2), cex.axis = 2.5)
box()
plot(c(fdata(M_left_st[res$cls == 2]), fdata(M_right_st[res2$cls == 2])), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, labels = FALSE)
box()
plot(c(fdata(M_left_st[res$cls == 3]), fdata(M_right_st[res2$cls == 6])), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, labels = FALSE)
box()
plot(fdata(M_left_st[res$cls == 4]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-2, 0, 2), labels = c(-2, 0, 2), cex.axis = 2.5)
box()
plot(c(fdata(M_left_st[res$cls == 5]), fdata(M_right_st[res2$cls == 3])), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(c(fdata(M_right_st[res2$cls == 1]), fdata(M_left_st[res$cls == 6])), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, labels = FALSE)
box()
plot(c(fdata(M_left_st[res$cls == 7]), fdata(M_right_st[res2$cls == 4])), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c(-2, 0, 2), labels = c( -2, 0, 2), cex.axis = 2.5)
box()
plot(fdata(M_right_st[res2$cls == 7]), 
     lwd = 2, main = '',
     ylim = c(-5, 7), axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, labels = FALSE)
box()

# Plot cluster mean
par(mfrow = c(3, 3), mar = c(0.5,0.5,0.5,0.5), 
    oma = c(3,3,0,0))
plot(mean(c(fdata(M_left_st[res$cls == 1]), fdata(M_right_st[res2$cls == 5]))), 
     ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c( -2, 0, 2), labels = c( -2, 0, 2), cex.axis = 3)
box()
plot(mean(c(fdata(M_left_st[res$cls == 2]), fdata(M_right_st[res2$cls == 2]))), 
     ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(mean(c(fdata(M_left_st[res$cls == 3]), fdata(M_right_st[res2$cls == 6]))), 
     ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(mean(fdata(M_left_st[res$cls == 4])), 
     ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c( -2, 0, 2), labels = c( -2, 0, 2), cex.axis = 3)
box()
plot(mean(c(fdata(M_left_st[res$cls == 5]), fdata(M_right_st[res2$cls == 3]))), 
     ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(mean(c(fdata(M_right_st[res2$cls == 1]), fdata(M_left_st[res$cls == 6]))), 
     ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()
plot(mean(c(fdata(M_left_st[res$cls == 7]), fdata(M_right_st[res2$cls == 4]))), 
     ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
axis(2, at = c( -2, 0, 2), labels = c( -2, 0, 2), cex.axis = 3)
box()
plot(mean(fdata(M_right_st[res2$cls == 7])), 
     ylim = c(-4, 4),
     main = '', lwd = 3, col = 'red', axes = FALSE)
abline(a = -2, b = 0, lty = 'dashed')
abline(a = 0, b = 0, lty = 'dashed')
abline(a = 2, b = 0, lty = 'dashed')
box()


################################################################################
################################################################################
# CLUSTER 97 Malawi BA curves using the funFEM algorithm
################################################################################
################################################################################
library(funFEM)
res = funFEM(M_ba_st, K=2:10, model = 'all')
plot(2:7,res$plot$bic,type='b',xlab='Number of cluster', ylab = 'BIC')
plot(2:7,res$plot$ll,type='b',xlab='K',main='loglik')


################################################################################
# Cluster LMCA/RMCA/BA together

res.multi <- funHDDC(list(M_left_st, M_right_st, -M_ba_st), K = 3:12,
                     model = c('AkjBkQkDk', 'AkjBQkDk', 'AkBkQkDk', 'ABkQkDk', 'AkBQkDk', 'ABQkDk'),
                     init='kmeans',threshold=0.2)

#######################################
# Which patients belong to each group
M_left_st$fdnames$reps[which(res.multi$class == 1)]
M_left_st$fdnames$reps[which(res.multi$class == 2)]



########################################
# Plot LMCA/RMCA/BA curves in each group

plot_one_group <- function(group_number, axis.T = TRUE, title = 'G1'){
  if (axis.T) {
    plot(fdata(M_left_st[res.multi$class == group_number]), main = title, ylim = c(-4, 5),
         lwd = 2, axes = FALSE, cex.main = 2.5)
    abline(a = -2, b = 0, lty = 'dashed')
    abline(a = 0, b = 0, lty = 'dashed')
    abline(a = 2, b = 0, lty = 'dashed')
    axis(2, at = c(-4, -2, 0, 2, 4),
         labels = c(-4, -2, 0, 2, 4), cex.axis = 2.5)
    mtext("LMCA", side=2, cex = 2, line = 4)
    box()
    
    plot(fdata(M_right_st[res.multi$class == group_number]), main = '', ylim = c(-4, 5),
         lwd = 2, axes = FALSE, cex.main = 2.5)
    abline(a = -2, b = 0, lty = 'dashed')
    abline(a = 0, b = 0, lty = 'dashed')
    abline(a = 2, b = 0, lty = 'dashed')
    axis(2, at = c(-4, -2, 0, 2, 4), 
         labels = c(-4, -2, 0, 2, 4), cex.axis = 2.5)
    mtext("RMCA", side=2, cex = 2, line = 4)
    box()
    
    plot(fdata(M_ba_st[res.multi$class == group_number]), main = '', ylim = c(-4, 7),
         lwd = 2, axes = FALSE, cex.main = 2.5)
    abline(a = -2, b = 0, lty = 'dashed')
    abline(a = 0, b = 0, lty = 'dashed')
    abline(a = 2, b = 0, lty = 'dashed')
    axis(2, at = c(-4, -2, 0, 2, 4, 6), 
         labels = c(-4, -2, 0, 2, 4, 6), cex.axis = 2.5)
    mtext("BA", side=2, cex = 2, line = 4)
    box()
  } else{
    plot(fdata(M_left_st[res.multi$class == group_number]), main = title, ylim = c(-4, 5),
         lwd = 2, axes = FALSE, cex.main = 2.5)
    abline(a = -2, b = 0, lty = 'dashed')
    abline(a = 0, b = 0, lty = 'dashed')
    abline(a = 2, b = 0, lty = 'dashed')
    box()
    
    plot(fdata(M_right_st[res.multi$class == group_number]), main = '', ylim = c(-4, 5),
         lwd = 2, axes = FALSE, cex.main = 2.5)
    abline(a = -2, b = 0, lty = 'dashed')
    abline(a = 0, b = 0, lty = 'dashed')
    abline(a = 2, b = 0, lty = 'dashed')
    box()
    
    plot(fdata(M_ba_st[res.multi$class == group_number]), main = '', ylim = c(-4, 7),
         lwd = 2, axes = FALSE, cex.main = 2.5)
    abline(a = -2, b = 0, lty = 'dashed')
    abline(a = 0, b = 0, lty = 'dashed')
    abline(a = 2, b = 0, lty = 'dashed')
    box()
  }
  
}


# Plot G1, G2, and G3
# Each column represents a group
k = 20
res.multi <- funHDDC(list(M_left_st, M_right_st, M_ba_st), K = k,
                     model = c('AkjBkQkDk', 'AkjBQkDk', 'AkBkQkDk', 'ABkQkDk', 'AkBQkDk', 'ABQkDk'),
                     init='kmeans',threshold=0.2)
for (i in seq(1, k, 3)){
    pdf(paste("C:/Users/bzhan/Dropbox/TCD project/plot/HDDC_more_groups/", 
              'G', i, '_', i+2, '.pdf', sep = ''),
        width = 12, height = 8)
    par(mfcol = c(3,3), mar = c(0.5,0.5,2,0.5), oma = c(2,6,0,0))
  
    plot_one_group(i, TRUE, paste('G', i, sep = ''))
    plot_one_group(i+1, FALSE, paste('G', i+1, sep = ''))
    plot_one_group(i+2, FALSE, paste('G', i+2, sep = ''))
    dev.off()
}

# Which patients in each group
for (i in seq(1,20,1)) cat(i, M_left_st$fdnames$reps[which(res.multi$class == i)], '\n')


########################################
# Plot mean LMCA/RMCA/BA curves in each group
plot_one_group_mean <- function(group_number, axis.T = TRUE, title = 'G1', ymax = 5){
  if (axis.T) {
    plot(mean(fdata(M_left_st[res.multi$class == group_number])), main = title, ylim = c(-3, ymax),
         lwd = 2, axes = FALSE, cex.main = 2.5, col = 'red')
    abline(a = -2, b = 0, lty = 'dashed')
    abline(a = 0, b = 0, lty = 'dashed')
    abline(a = 2, b = 0, lty = 'dashed')
    axis(2, at = seq(-4, ymax, 2),
         labels = seq(-4, ymax, 2), cex.axis = 2.5)
    mtext("LMCA", side=2, cex = 2, line = 4)
    box()
    
    plot(mean(fdata(M_right_st[res.multi$class == group_number])), main = '', ylim = c(-3, ymax),
         lwd = 2, axes = FALSE, cex.main = 2.5, col = 'red')
    abline(a = -2, b = 0, lty = 'dashed')
    abline(a = 0, b = 0, lty = 'dashed')
    abline(a = 2, b = 0, lty = 'dashed')
    axis(2, at = seq(-4, ymax, 2),
         labels = seq(-4, ymax, 2), cex.axis = 2.5)
    mtext("RMCA", side=2, cex = 2, line = 4)
    box()
    
    plot(mean(fdata(M_ba_st[res.multi$class == group_number])), main = '', ylim = c(-3, ymax),
         lwd = 2, axes = FALSE, cex.main = 2.5, col = 'red')
    abline(a = -2, b = 0, lty = 'dashed')
    abline(a = 0, b = 0, lty = 'dashed')
    abline(a = 2, b = 0, lty = 'dashed')
    axis(2, at = seq(-4, ymax, 2),
         labels = seq(-4, ymax, 2), cex.axis = 2.5)
    mtext("BA", side=2, cex = 2, line = 4)
    box()
  } else{
    plot(mean(fdata(M_left_st[res.multi$class == group_number])), main = title, ylim = c(-3, ymax),
         lwd = 2, axes = FALSE, cex.main = 2.5, col = 'red')
    abline(a = -2, b = 0, lty = 'dashed')
    abline(a = 0, b = 0, lty = 'dashed')
    abline(a = 2, b = 0, lty = 'dashed')
    box()
    
    plot(mean(fdata(M_right_st[res.multi$class == group_number])), main = '', ylim = c(-3, ymax),
         lwd = 2, axes = FALSE, cex.main = 2.5, col = 'red')
    abline(a = -2, b = 0, lty = 'dashed')
    abline(a = 0, b = 0, lty = 'dashed')
    abline(a = 2, b = 0, lty = 'dashed')
    box()
    
    plot(mean(fdata(M_ba_st[res.multi$class == group_number])), main = '', ylim = c(-3, ymax),
         lwd = 2, axes = FALSE, cex.main = 2.5, col = 'red')
    abline(a = -2, b = 0, lty = 'dashed')
    abline(a = 0, b = 0, lty = 'dashed')
    abline(a = 2, b = 0, lty = 'dashed')
    box()
  }
}


# Plot G1, G2, and G3
# Each column represents a group
par(mfcol = c(3,3), mar = c(0.5,0.5,2,0.5), oma = c(2,6,0,0))
plot_one_group_mean(1, TRUE, 'G1')
plot_one_group_mean(2, FALSE, 'G2')
plot_one_group_mean(3, FALSE, 'G3')
plot_one_group_mean(4, TRUE, 'G4')
plot_one_group_mean(6, FALSE, 'G5')
plot_one_group_mean(7, FALSE, 'G6')
plot_one_group_mean(5, TRUE, 'G7', ymax = 8)
plot_one_group_mean(8, FALSE, 'G8', ymax = 8)

