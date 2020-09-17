################################################################################
#       __  ______   __________  ____
#      /  |/  /   | / ____/ __ \/ __ \
#     / /|_/ / /| |/ /   / /_/ / / / /
#    / /  / / ___ / /___/ _, _/ /_/ /
#   /_/  /_/_/  |_\____/_/ |_|\____/
#
#   PfSI parameters module (parameters for inst/include/Parameters.hpp object)
#
#   Sean Wu
#   December 2018
#
################################################################################


#' PfSI Parameters
#'
#' Parameters for the 'PfSI' model of human infection dynamics
#' (see inst/include/Parameters.hpp for where parameters are stored)
#'
#' @param Pf_c human to mosquito transmission efficiency
#' @param Pf_b mosquito to human transmission efficiency
#' @param DurationPf duration of infection
#' @param LatentPf duration of latent stage
#' @param FeverPf probability of fever upon infection
#' @param mnFeverPf mean of time to fever (Gaussian)
#' @param vrFeverPf sd of time to fever (Gaussian)
#' @param TreatPf probability of seeking treatment after fever
#' @param mnTreatPf time to treatment
#' @param mnChemoprophylaxisPf duration of chemoprophylactic protection
#' @param PEProtectPf vaccination efficacy (probability of effect)
#' @param peBlockPf vaccine effectiveness (reduction in probability of transmission 'b' given challenge)
#' @param mnPEPf mean duration of protection from PE vaccination (Gaussian)
#' @param vrPEPf standard deviation of protection from PE vaccination (Gaussian)
#' @param GSProtectPf vaccination efficacy (probability of effect)
#' @param gsBlockPf vaccine effectiveness (reduction in probability of transmission 'c' given challenge)
#' @param mnGSPf mean duration of protection from GS vaccination (Gaussian)
#' @param vrGSPf standard deviation of protection from GS vaccination (Gaussian)
#' @param travel_vaxx when someone goes to a reservoir patch, do they get vaccinated immediately before?
#' @param travel_treat when someone goes to a reservoir patch, do they get treated immediately before? (is conditional on vaccination)
#'
#' @export
pfsi_parameters <- function(
    Pf_c   = 0.15,
    Pf_b   = 0.55,
    DurationPf = 200,
    LatentPf = 10,
    FeverPf = 0.3,
    mnFeverPf = 10,
    vrFeverPf = .1,
    TreatPf = .5,
    mnTreatPf = 1,
    mnChemoprophylaxisPf = 32,
    PEProtectPf = .99,
    peBlockPf = 1,
    mnPEPf = 270,
    vrPEPf = 50,
    GSProtectPf = 1,
    gsBlockPf = .9,
    mnGSPf = 180,
    vrGSPf = 20,
    travel_vaxx = FALSE,
    travel_treat = FALSE
  ){
  list(
    Pf_c = as.numeric(Pf_c),
    Pf_b = as.numeric(Pf_b),
    DurationPf = as.numeric(DurationPf),
    LatentPf = as.numeric(LatentPf),
    FeverPf = as.numeric(FeverPf),
    mnFeverPf = as.numeric(mnFeverPf),
    vrFeverPf = as.numeric(vrFeverPf),
    TreatPf = as.numeric(TreatPf),
    mnTreatPf = as.numeric(mnTreatPf),
    mnChemoprophylaxisPf = as.numeric(mnChemoprophylaxisPf),
    PEProtectPf = as.numeric(PEProtectPf),
    peBlockPf = as.numeric(peBlockPf),
    mnPEPf = as.numeric(mnPEPf),
    vrPEPf = as.numeric(vrPEPf),
    GSProtectPf = as.numeric(GSProtectPf),
    gsBlockPf = as.numeric(gsBlockPf),
    mnGSPf = as.numeric(mnGSPf),
    vrGSPf = as.numeric(vrGSPf),
    travel_vaxx = as.logical(travel_vaxx),
    travel_treat = as.logical(travel_treat)
  )
}
