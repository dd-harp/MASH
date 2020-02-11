# ################################################################################
# #
# #   Mosquito -> Human bite distribution module
# #   Sean Wu (slwu89@berkeley.edu)
# #   November 2019
# #
# ################################################################################
#
#
#
#
# # biting_machine
# #
# # bitiing_machine_result
# #
# # init.biting_machine <- function(){
# #
# #   biting_machine <- new.env()
# #   biting_machine$result <- rep(0,100)
# #
# # }
# #
# # get_bites <- function(){
# #
# # }
# #
# # get_bites_human <- function(id){
# #
# # }
#
#
# #' Distribute infectious Bites to Humans (Multinomial)
# #'
# #' This is an exact algorithm that first samples the number of infectious bites
# #' produced by mosquitoes for each patch from a Poisson distribution and then apportions
# #' them to humans in each patch by Multinomial sampling.
# #'
# #' @section input:
# #' The input \code{parameters} may be a named list or environment with the following
# #' elements
# #'  * a: human biting rate (by patch)
# #'  * Z: number of infectious mosquitoes (by patch)
# #'  * rho: biting weights of all humans
# #'  * loc: locations of all humans
# #'
# #' @param parameters a named list or environment
# #' @param time_step the size of the time step
# #'
# #' @return a vector of bites on each human
# #'
# #' @export
# step.bites_multinom_machine <- function(parameters,time_step){
#
#   with(parameters,{
#
#     stopifnot(max(loc) <= length(Z))
#
#     alpha <- (a*time_step) * Z
#     alpha <- rpois(n = length(alpha),lambda = alpha)
#
#     bites <- rep(0,length(loc))
#     unique_loc <- unique(loc)
#     for(i in seq_along(unique_loc)){
#       ix <- which(loc == unique_loc[i])
#       bites[ix] <- as.vector(rmultinom(n = 1,size = alpha[unique_loc[i]],prob = rho[ix]))
#     }
#
#     return(bites)
#   })
# }
#
# #' Distribute infectious Bites to Humans (Poisson)
# #'
# #' This is a marginal algorithm that samples a Poisson distributed
# #' number of bites for each person.
# #'
# #' @section input:
# #' The input \code{parameters} may be a named list or environment with the following
# #' elements
# #'  * a: human biting rate (by patch)
# #'  * Z: number of infectious mosquitoes (by patch)
# #'  * rho: biting weights of all humans
# #'  * loc: locations of all humans
# #'
# #' @param parameters a named list or environment
# #' @param time_step the size of the time step
# #'
# #' @return a vector of bites on each human
# #'
# #' @export
# step.bites_pois_machine <- function(parameters,time_step){
#
#   with(parameters,{
#
#     stopifnot(max(loc) <= length(Z))
#
#     alpha <- (a*time_step) * Z
#
#     bites <- rep(0,length(loc))
#     unique_loc <- unique(loc)
#     for(i in seq_along(unique_loc)){
#       ix <- which(loc == unique_loc[i])
#       rho_norm <- rho[ix] / sum(rho[ix])
#       bites[ix] <- rpois(n = length(ix),lambda = alpha[unique_loc[i]] * rho_norm)
#     }
#
#     return(bites)
#   })
# }
