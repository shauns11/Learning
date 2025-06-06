##############################.
#Fixed-effects Poisson model.
###############################.

getwd() 
.libPaths()
print(R.version.string)

Macdrive<-
  "/Users/shaun/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Learning/"
UCLdrive<-
  "C:/Users/rmjdshc/OneDrive - University College London/Learning/"
folder<-"Longitudinal/03. Fixed Effects/"  
setwd(paste0(UCLdrive, folder))
#setwd(paste0(Macdrive, folder))

library(fixest)
library(plm)
library(haven)

##data_frame.
df<-haven::read_dta("data/fixed_effects_df.dta")
is.data.frame(df) 
head(df)
#Fixed-effects poisson.
m1 <- fixest::fepois(y ~ factor(x) | id, (panel.id = ~id+month),data = df)
# Exponentiate the coefficients to get the Incidence Rate Ratios (IRR)
exp(coef(m1))

##Stata:
#xtset id month
#xtpoisson y i.x, irr fe

print(paste("Fixed effects Poisson model finished at", format(Sys.time(), 
                "%Y-%m-%d %H:%M:%S")))


#######################
##Finished
#######################
