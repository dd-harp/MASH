## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
# output path
path <- "output/"
if(!dir.exists(path)){
  dir.create(path)
} else {
  files <- list.files(path)
  if(length(files) > 0){
    for(f in files){
      file.remove(paste0(path,f))
    }
  }
}

# seed
set.seed(42L)

# the package
library(macro.pfmoi)

# Matrix for sparse mosquito diffusion matrices
library(Matrix)
# tidyverse and Hmisc for output visualization; I prefer to use tidyverse but the output can be analyzed with base plots just fine
library(tidyverse)
library(Hmisc)

## ------------------------------------------------------------------------
# vector of parameters
pfmoi_pars <- pfmoi_parameters()

# set up patches (n is how many patches we have)
n <- 5
# movement matrix for humans is uniform over possible destinations.
move <- matrix(data = 1/(n-1),nrow = n,ncol = n)
diag(move) <- 0
patch_pars <- patches_parameters(move = move,bWeightZoo = rep(0,n),bWeightZootox = rep(0,n),reservoir = rep(F,n),res_EIR = rep(0,n))

# mosquitos
psi <- Matrix::sparseMatrix(i = {},j = {},x = 0.0,dims = c(n,n))
diag(psi) <- rep(1,n)
mosy_pars <- mosquito_rm_conpars(N = n,lambda = matrix(50,nrow = 365,ncol = n),
                                 psi = psi,EIP = rep(11,365),M = rep(450,n),Y = rep(0,n),Z = rep(0,n))

## ------------------------------------------------------------------------
# humans
patch_sizes <- rep(1e3,n) # number of people in each patch
pfpr <- rep(0.5,n) # malaria prevalence in each patch
nh <- sum(patch_sizes) # total num of humans

# first sample for each person if they are infected (MOI > 0) or not (MOI = 0) based on their home patch
init_moi <- unlist(mapply(FUN = function(n,pr){
  sample(x = c(1,0),size = n,replace = T,prob = c(pr,1-pr))
},n=patch_sizes,pr=pfpr,SIMPLIFY = F))

# for those who had MOI > 0 we sample a MOI > 0 value for them
init_moi <- unlist(lapply(init_moi,function(x){
  if(x==1){
    x <- rpois(n = 1,lambda = 1)
    while(x==0){
      x <- rpois(n = 1,lambda = 1)
    }
    x
  } else {
    0
  }
}))

# where the people go (0-indexed for c++)
patch_id <- rep(0:(n-1),times=patch_sizes)

# uniform biting weights; this could follow any density on the positive reals (gamma, log-normal, weibull, etc.)
bweights <- rep(1,nh)

# the data structure that will be passed down to C++ to construct the human pop
human_pars <- vector("list",nh)
for(i in 1:nh){
  human_pars[[i]] <- human_pfmoi_conpars(id = i-1,home_patch_id = patch_id[i],
                                        trip_duration = rep(3,n),trip_frequency = 1/20,bweight = bweights[i],
                                        age = 20,moi = init_moi[i],chx = FALSE,bite_algorithm = 0)
}

# vaccinations (can uncomment the code below to vaccinate 25% of the population at day 500)
# vaxx_pars <- list()
vaxx_id <- sample(x = 0:(nh-1),size = nh*0.25,replace = F)
vaxx_pars <- lapply(X = vaxx_id,FUN = function(id){
  vaccination_pfmoi_conpars(id = id,t = 5e2,treat = T,type = "PE")
})

## ------------------------------------------------------------------------
log_pars <- list()
h_inf <- paste0(path,"pfmoi.csv")
log_pars[[1]] <- list(outfile = h_inf,key = "pfmoi",
                      header = patches_header()
                    )
mosy <- paste0(path,"mosy.csv")
log_pars[[2]] <- list(outfile = mosy,key = "mosquito",
                      header = paste0(c("time","state",paste0("patch",1:n)),collapse = ","))

run_macro(tmax = 1e3,
          human_pars = human_pars,
          mosquito_pars = mosy_pars,
          patch_pars = patch_pars,
          model_pars = pfmoi_pars,
          log_streams = log_pars,
          vaxx_events = vaxx_pars,
          verbose = T)

pfmoi <- readr::read_csv("output/pfmoi.csv")

pfmoi_pr <- pfmoi %>%
  select(-ends_with("away")) %>%
  select(-starts_with("incidence")) %>%
  select(-starts_with("MOI")) %>%
  gather(key, value, -time,-patch)

ggplot(pfmoi_pr) +
  geom_line(aes(x=time,y=value,color=key)) +
  facet_wrap(. ~ patch) +
  theme_bw()

pfmoi_moi <- pfmoi %>%
  select(matches("time|patch|MOI_visitor|MOI_resident_home")) %>%
  filter(time != 0)

ggplot(pfmoi_moi) +
  geom_line(aes(x=time,y=MOI_visitor_mean),colour="firebrick3") +
  geom_line(aes(x=time,y=MOI_resident_home_mean),colour="steelblue") +
  geom_ribbon(aes(x=time,ymin=pmax(MOI_visitor_mean - MOI_visitor_sd,0),ymax=MOI_visitor_mean + MOI_visitor_sd),alpha=0.5,fill="firebrick3") +
  geom_ribbon(aes(x=time,ymin=pmax(MOI_resident_home_mean - MOI_resident_home_sd,0),ymax=MOI_resident_home_mean + MOI_resident_home_sd),alpha=0.5,fill="steelblue") +
  facet_wrap(. ~ patch) +
  theme_bw()

## ------------------------------------------------------------------------
# run ensemble
nrun <- 1e2
tsteps <- 1e3
pb <- txtProgressBar(min = 1,max = nrun)
for(i in 1:nrun){

  log_pars <- list()
  h_inf <- paste0(path,"pfmoi_",i,".csv")
  log_pars[[1]] <- list(outfile = h_inf,key = "pfmoi",
                        header = patches_header()
                      )
  mosy <- paste0(path,"mosy_",i,".csv")
  log_pars[[2]] <- list(outfile = mosy,key = "mosquito",
                        header = paste0(c("time","state",paste0("patch",1:n)),collapse = ","))

  run_macro(tmax = tsteps,
            human_pars = human_pars,
            mosquito_pars = mosy_pars,
            patch_pars = patch_pars,
            model_pars = pfmoi_pars,
            log_streams = log_pars,
            vaxx_events = vaxx_pars,
            verbose = FALSE)
  setTxtProgressBar(pb,i)
}

pfmoi_ensemble <-
  list.files(path = "output/",pattern = "pfmoi_[[:digit:]]+.csv") %>%
  map_df(~read_csv(paste0("output/",.)),.id = "run")

pfmoi_ensemble_pr <- pfmoi_ensemble %>%
  select(-ends_with("away")) %>%
  select(-starts_with("incidence")) %>%
  gather(key, value, -time,-patch,-run)

ggplot(pfmoi_ensemble_pr,aes(x=time,y=value,color=key,fill=key)) +
  stat_summary(fun.data = median_hilow,fun.args = list(conf.int = 0.95),geom = "ribbon",alpha=0.4,color=NA) +
  stat_summary(geom="line", fun.y="mean") +
  facet_wrap(. ~ patch) +
  guides(color = FALSE) +
  theme_bw()

## ---- echo=FALSE---------------------------------------------------------
# just for cleaning out the vignettes folder in the package
unlink(x = path, recursive = TRUE)
rm(list=ls())

