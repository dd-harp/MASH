#' mosquito_mbites: This wraps the MBITES package to run within MASH.
#'
#' In order to run MBITES in MASH, this class converts MASH messages to input
#' to MBITES and converts MBITES messages to input for MASH. This module has
#' the same interface as a Bloodmeal module, not a Mosquito module, because
#' the Human and Location exchange the same information with it as they would
#' with a MASH bloodmeal module.
#'
#' \itemize{
#'   \item \link{infects_human_path.mosquito_mbites}
#' }
#'
#' @name mosquito_mbites
#' @docType class
NULL

library(here)
library(data.table)
library(MBITES)
library(futile.logger)

#' Create an MBITES mosquito module.
#'
#' @param parameters There are no parameters for this, so this is ignored.
#' @return a module for a MASH bloodmeal.
#'
#' This initializes MBITES from parameters. There is a single global copy
#' of the MBITES simulation that this initializes.
#'
#' @export
mosquito_mbites_module <- function(parameters) {

  MBITES::local_logging("trace", "mbites.log")
  set.seed(91324724)

  ###############################################################################
  # Make landscape initialization object
  ###############################################################################

  # landscapes
  landscape <- vector(mode = "list",length = 3)

  # site characteristics
  for(i in 1:3){
    landscape[[i]]$id = i
    landscape[[i]]$xy = c(1,1)
    landscape[[i]]$type = 1L
    landscape[[i]]$tileID = 1L
    landscape[[i]]$move = rep(0.5,2)
    landscape[[i]]$move_id = (1:3)[-i]
    landscape[[i]]$haz = 0.005
    # null resources
    landscape[[i]]$feed = NULL
    landscape[[i]]$aqua = NULL
  }

  # site 1 has both resources
  landscape[[1]]$feed[[1]] = list(w=1,enterP=1,zoo_id=-1,zoo_w=0.1)
  landscape[[1]]$aqua[[1]] = list(w=1,lambda=1)

  # site 2 has only blood feeding resource
  landscape[[2]]$feed[[1]] = list(w=1,enterP=1,zoo_id=-1,zoo_w=0.1)

  # site 3 has only aquatic habitat resource
  landscape[[3]]$aqua[[1]] = list(w=1,lambda=1)

  ###############################################################################
  # Make human initialization object
  ###############################################################################

  nHumans = 10L
  if (nHumans > 9L) {
    initial_pr <- 0.4
    pr_vals <- rbinom(nHumans, 1L, initial_pr)
  } else {  # randomizing for too few could mean no pathogens.
    pr_vals <- rep(c(1L, 0L), nHumans)[1:nHumans]
  }

  humans = data.frame(
    tileID = rep(1,nHumans),
    # make sure the humans are at the sites with blood feeding resources
    siteID = 1:2,
    feedingID = rep(1,nHumans),
    w = rep(0.9,nHumans),
    pr = pr_vals
  )

  ###############################################################################
  # Make mosquito initialization object
  ###############################################################################

  nMosquitos = 50

  mosquitos = data.frame(
    tileID = rep(1,nMosquitos),
    # make sure the mosquitos emerge from aquatic habitats
    siteID =rep(c(1,3),length.out=nMosquitos),
    female = rep(T,nMosquitos)
  )

  ###############################################################################
  # Run MBITES
  ###############################################################################

  directory <- here("output/sei/")
  if(!dir.exists(directory)){
    dir.create(directory,recursive = T)
  }

  cat("initializing MBITES functional forms for MBDETES approximation\n")

  # time to event parameters
  MBITES::MBITES_Setup_Timing(timing_model = 2,
                              rate_b = 1/(3/24),tmin_b = 0,
                              rate_bs = 1/(6/24),tmin_bs = 0,
                              rate_o = 1/(3/24),tmin_o = 0,
                              rate_os = 1/(6/24),tmin_os = 0,
                              # minimum waiting time needed for ppr to avoid numerical difficulties
                              ppr_model = 2,rate_ppr = 1/(18/24),tmin_ppr = 1e-2)

  MBITES:::Globals$set_SETUP("timing")

  # bloodmeal: turn off overfeeding
  Mosquito_Female$set(which = "public",name = "Overfeeding",
                      value = MBITES:::mbites_OverfeedingNull, overwrite = TRUE
  )
  MBITES:::Globals$set_SETUP("bloodmeal")

  # Oogenesis options
  # egg batch size proportional to blood meal
  Mosquito_Female$set(which = "public",name = "rBatchSize",
                      value = MBITES:::mbites_rBatchSizeBms, overwrite = TRUE
  )

  # main oogenesis model
  Mosquito_Female$set(which = "public",name = "Oogenesis",
                      value = MBITES:::mbites_oogenesisMBDETES, overwrite = TRUE
  )

  # refeeding probability is function of blood meal size
  Mosquito_Female$set(which = "public",name = "checkRefeed",
                      value = MBITES:::mbites_checkRefeedMBDETES, overwrite = TRUE
  )

  Mosquito_Female$set(which = "public",name = "pReFeed",
                      value = MBITES:::mbites_pReFeed_bm, overwrite = TRUE
  )
  MBITES:::Globals$set_SETUP("oogenesis")

  # turn sugar off
  Mosquito_Female$set(which = "public",name = "queueSugarBout",
                      value = MBITES:::mbites_queueSugarBout_null, overwrite = TRUE
  )
  MBITES:::Globals$set_SETUP("energetics")

  # oviposition uses emerge
  Mosquito_Female$set(which = "public",name = "layEggs",
                      value = MBITES:::mbites_layEggs_Emerge, overwrite = TRUE
  )
  MBITES:::Globals$set_SETUP("oviposition")

  # survival: turn off wing tattering and senescence
  Mosquito$set(which = "public",name = "WingTattering",
               value = MBITES:::mbites_WingTattering_null, overwrite = TRUE
  )

  Mosquito$set(which = "public",name = "Senescence",
               value = MBITES:::mbites_Senescence_null, overwrite = TRUE
  )
  MBITES:::Globals$set_SETUP("survival")

  ## End of Setup for MBDETES

  PATHOGEN_Setup(pathogen_model = "SEI")

  # we want detailed output of blood hosts from the mosquito
  trackBloodHost()
  trackOviposition()


  # a good parameter set
  MBITES:::Parameters$set_parameters(Bs_surv = 0.95,Os_surv = 0.95,B_surv = 0.99,O_surv = 0.99,
                                     Bs_succeed = 0.99,Os_succeed = 0.99,B_succeed = 0.95,O_succeed = 0.99,
                                     S_u = 0,disperse = 0.2)

  # initialize a tile
  Tile_Initialize(landscape)
  Human_NULL_Initialize(humans)
  MBITES_Initialize(mosquitos)

  human_hash <- MBITES:::Globals$get_tile(1)$get_humans()
  stopifnot(human_hash$size() == nHumans)

  p_ancestor <- SEI_Pathogen$new()
  initial_pathogen_id <- p_ancestor$get_id()
  for (hidx in which(humans$pr == 1L)) {
    h <- human_hash$get(hidx)
    p <- SEI_Pathogen$new(initial_pathogen_id)
    p$initial_age(sample(1L:200L, 1))  # give them random previous infection dates
    h$add_pathogen(p)
  }

  set_output(directory = directory, runID = 1)
  # ready to run simulation

  module <- list(
    parameters = parameters,
    day_start = parameters$day_start,
    human_events = NULL
  )
  class(module) <- "mosquito_mbites"
  module
}


#' Take one time step for a bloodmeal module.
#'
#' @param simulation The blodmeal module.
#' @param health_dt Human health data.
#' @param movement_dt Movement of humans.
#' @param bites_dt This should be `NULL` here.
#' @return Returns the simulation that's updated.
#' @export
mash_step.mosquito_mbites <- function(
  module, step_id, health_dt, movement_dt, bites_dt
) {
  stopifnot(is.null(bites_dt))

  # Use health_dt to change infectiousness of human pathogens.

  # Use movement_dt to change site of humans.

  MBITES::simulation(tMax = module$parameters$day_cnt, pretty = TRUE, cleanup = FALSE)

  module$human_events <- outcome$human_events
  module$day_start <- module$day_start + module$parameters$day_cnt
  class(module) <- "mosquito_mbites"
  module
}


#' Extract human bites from the bloodmeal module.
#'
#' @param simulation The bloodmeal_density module.
#' @return A data.table with bite information. These are all bites
#'     of humans where the mosquito was infectious.
#' @export
infects_human_path.mosquito_mbites <- function(simulation) {
  bites_infectious_to_humans <- MBITES:::Globals$get_tile(1)$human_bites()
  simulation$human_events
}
