# Bivariate Plot Script

dir.base <- "C:/Users/matthew.w.smith/Desktop/Bivariate Profiles/Red_Grouper_FF" 
ctl.file <- "control.ss_new"

dir.prof <- file.path(dir.base, "profile_2D")
copy_SS_inputs(
  dir.old = dir.base,
  dir.new = dir.prof,
  create.dir = TRUE,
  overwrite = TRUE,
  copy_par = TRUE,
  verbose = TRUE
)


par_table <- expand.grid(
  vec1 = seq(.4,.8,.02), #sigr
  vec2 = seq(0.65, 0.85, 0.02) # h
)

# run model once to create control.ss_new with
# good starting parameter values
# exe is assumed to be in PATH, add "exe" argument if needed
run(dir.prof, exe = paste0(dir.base,"/ss"),extras = "-nohess")

# create new_control file
ctl <- SS_readctl(paste0(dir.prof,'/control.ss_new'), version = "3.30", use_datlist = T)#, datlist = 
                    # paste0(dir.prof,'/',"vermilion.dat"))
invisible(SS_writectl(ctl,paste0(dir.prof,'/control.ss'),overwrite = T))

#
# Update starter file to reflect new_control and prepare for profile
starter <- SS_readstarter(file.path(dir.prof, "starter.ss"))
starter[["ctlfile"]] <- "control.ss" # This must match the name in the profile call below
starter[["prior_like"]] <- 1
SS_writestarter(starter, dir = dir.prof, overwrite = TRUE)


# run profile using ss_new file as parameter source and
# overwriting original control file with new values
prof.table <- profile(
  dir = dir.prof,
  oldctlfile = "control.ss_new",
  newctlfile = "control.ss",
  string = c("sigmaR", "steep"),
  profilevec = par_table,
  exe = paste0(dir.base,"/ss"),
  extras = "-nohess"
)


#dir.prof <- paste0(dir.prof,"/SigmaR_Steep")
# get model output
profilemodels <- SSgetoutput(
  dirvec = dir.prof,
  keyvec = 1:nrow(par_table), getcovar = FALSE
)
n <- length(profilemodels)
profilesummary <- SSsummarize(profilemodels)

# add total likelihood (row 1) to table created above
par_table[["like"]] <- as.numeric(profilesummary[["likelihoods"]][1, 1:n])

# reshape data frame into a matrix for use with contour
like_matrix <- reshape2::acast(
  data = par_table,
  formula = vec1 ~ vec2,
  value.var = "like"
)

dimnames(like_matrix)


###########################Plotting Needs updated
library(lattice)
library(latticeExtra)
getwd()
setwd(dir.prof)

png("Steepness and Sigma_R Contour Plot (Smoothed).png")
col.l <- colorRampPalette(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'))
levelplot(like_matrix, xlab = "Sigma_R", ylab = "Steepness",main="Contour Plot of Steepness and Sigma_R",cuts=50,aspect='fill',col.regions=col.l,contour=TRUE,panel=panel.2dsmoother,args=list(span=0.95))
dev.off()

png("Steepness and Sigma_R Contour Plot (Coarse)0.15.png")
col.l <- colorRampPalette(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'))
levelplot(like_matrix, xlab = "Sigma_R", ylab = "Steepness",main="Contour Plot of Steepness and Sigma_R",cuts=50,aspect='fill',col.regions=col.l,contour=TRUE,panel=panel.2dsmoother,args=list(span=0.15))
dev.off()


