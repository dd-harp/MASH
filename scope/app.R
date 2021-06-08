#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(ggplot2)
library(shiny)
library(shinythemes)
library(reactable)
library(macro)

build_movement_module <- function(parameters, travel) {
    lcnt <- parameters$location_cnt
    start_at_home <- rep(1:parameters$location_cnt, parameters$humans.l)
    stopifnot(length(start_at_home) == parameters$human_cnt)

    stochastic_matrix <- matrix(
        rep(travel, each = lcnt), nrow = lcnt, ncol = lcnt)
    diag(stochastic_matrix) <- 0
    # The rows should sum to one.
    stochastic_matrix <- diag(1 / rowSums(stochastic_matrix)) %*%
        stochastic_matrix
    return_matrix <- matrix(
        1, nrow = parameters$location_cnt, ncol = parameters$location_cnt)
    diag(return_matrix) <- 0
    return_matrix <- diag(1 / rowSums(return_matrix)) %*% return_matrix

    simpletrip_parameters <- list2env(list(
        trip_rate = rep(0.1, parameters$location_cnt),
        trip_dest = stochastic_matrix,
        return_home_rate = return_matrix,
        npatch = parameters$location_cnt,
        home = start_at_home,
        current = start_at_home,
        duration_days = parameters$duration_days
    ))
    simple_trip_module(simpletrip_parameters)
}


build_health_module <- function(parameters, pfpr) {
    health_parameters <- list(
        recovery_rate = 1 / 200,
        people_cnt = parameters$human_cnt,
        duration_days = parameters$duration_days,
        initial_pfpr = pfpr[1]  # XXX want to set by location!
    )
    human_si_module(health_parameters)
}

build_constant_health_module <- function(parameters, pfpr) {
  health_parameters <- list(
    people_cnt = parameters$human_cnt,
    duration_days = parameters$duration_days,
    initial_pfpr = pfpr[1]  # XXX want to set by location!
  )
  human_constant_module(health_parameters)
}

build_mosquito_module <- function(parameters, lambda, z) {
    year_days <- 365
    # XXX M should determine lambda.
    mosquito_parameters <- list(
        duration = parameters$duration_days,
        N = parameters$location_cnt,
        lambda = matrix(
            rep(lambda, year_days),
            nrow = parameters$location_cnt),
        psi = diag(parameters$location_cnt),  # diffusion matrix
        biting_weight = 0.4,  # The human biting weight. Should be from human.XXX
        EIP = rep(12, year_days),  # Extrinsic incubation period,
        maxEIP = 12,  # Maximum length of EIP.
        p = 0.9,  # survival
        # human blood feeding rate, the proportion of mosquitoes that feed on humans each day
        a = 0.6,
        infected_fraction = z,
        year_day_start = 1
    )
    mosquito_rm_module(mosquito_parameters)
}


build_bloodmeal_module <- function(parameters) {
    params <- list(
        human_cnt = parameters$human_cnt,
        location_cnt = parameters$location_cnt,
        duration = parameters$duration_days,
        day_duration = 1,
        dispersion = 1.5,
        day_cnt = 10,
        day_start = 1,
        biting_weight = 0.5
    )
    bloodmeal_density_module(params)
}


build_modules <- function(human, pfpr, travel, M, Z, module_choices) {
    parameters <- list(
        human_cnt = sum(human),
        humans.l = human,
        location_cnt = length(travel),
        duration_days = 10
    )
    movement <- build_movement_module(parameters, travel)
    if (module_choices$health == "SI") {
      health <- build_health_module(parameters, pfpr)
    } else {
      health <- build_constant_health_module(parameters, pfpr)
    }
    mosquito <- build_mosquito_module(parameters, M, Z)
    bloodmeal <- build_bloodmeal_module(parameters)
    list(location = movement,
         health = health,
         mosquito = mosquito,
         bloodmeal = bloodmeal)
}


#' Run a MASH simulation.
#' @param human a vector of the number of humans at each location.
#' @param pfpr The PfPR of humans at each location.
#' @param travel Rate of travel to other locations.
#' @param M total number of msoquitoes at each location.
#' @param Z number of infectious mosquitoes at each location.
generate_data <- function(human, pfpr, travel, M, Z, module_choices) {
    modules <- build_modules(
      human, pfpr, travel, M, Z, module_choices)

    step_cnt <- 5
    observer <- complete_observer()
    singleobs <- vector(mode = "list", length = step_cnt)
    msgnames <- c(
        "location", "bloodmeal_human", "bloodmeal_mosquito", "health", "mosquito")
    dump_if <- function(step, idx) { step == "bloodmeal" }
    seen <- list()
    for (main_idx in 1:step_cnt) {
        cat(paste("Running 10-day step", main_idx, "\n"))
        modules <- step_mainloop(modules, observer, main_idx, dump_if)
        singleobs[[main_idx]] <- lapply(
            msgnames,
            function(msgtype) {
                data.table::rbindlist(modules$observer[[msgtype]])
            }
        )
        names(singleobs[[main_idx]]) <- msgnames
    }
    observations <- lapply(
        msgnames,
        function(msgname) {
            data.table::rbindlist(
                lapply(singleobs, function(obs) obs[[msgname]])
            )
        }
    )
    names(observations) <- msgnames
    observations
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MASH for Three Locations"),

    fluidRow(
      column(
          4,
          "Health",
          selectInput("health", "Health:", c("SI", "Constant PfPR"))
      ),
      column(
          4,
          "Location",
          selectInput("location", "Location:", c("SimpleTrip", "Constant"))
      ),
      column(
          4,
          "Mosquito",
          selectInput("mosquito", "Mosquito:", c("RM"))
      )
    ),

    fluidRow(
        column(
            2,
            "People",
            numericInput("n1", label="n1", value = 10, width = "80px"),
            numericInput("n2", label="n2", value = 5, width = "80px"),
            numericInput("n3", label="n3", value = 15, width = "80px")
            ),
        column(
            2,
            "PfPR",
            numericInput("pfpr1", label="pfpr1", value = 0.2, width = "80px"),
            numericInput("pfpr2", label="pfpr2", value = 0.2, width = "80px"),
            numericInput("pfpr3", label="pfpr3", value = 0.2, width = "80px")
        ),
        column(
            4,
            "Travel",
            numericInput("travel1", label="travel1", value = 0.2),
            numericInput("travel2", label="travel2", value = 0.3),
            numericInput("travel3", label="travel3", value = 0.2)
        ),
        column(
            2,
            "Daily Emergence",
            numericInput("mosy1", label="mosy1", value = 200),
            numericInput("mosy2", label="mosy2", value = 300),
            numericInput("mosy3", label="mosy3", value = 100)
        ),
        column(
            2,
            "z",
            numericInput("z1", label="z1", value = 0.2),
            numericInput("z2", label="z2", value = 0.3),
            numericInput("z3", label="z3", value = 0.05)
        )
    ),
    fluidRow(
        column(2, actionButton("simulate", "Simulate")),
        column(10, textOutput("greeting"))
    ),
    fluidRow(
        column(6,
               plotOutput("bites")
               ),
        column(6,
               plotOutput("humanBites")
               )
    ),
    fluidRow(
        column(12,
               plotOutput("mosyz"))
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    observations <- eventReactive(input$simulate, {
        human <- c(input$n1, input$n2, input$n3)
        pfpr <- c(input$pfpr1, input$pfpr2, input$pfpr3)
        travel <- c(input$travel1, input$travel2, input$travel3)
        lambda <- c(input$mosy1, input$mosy2, input$mosy3)
        z <- c(input$z1, input$z2, input$z3)
        module_choices <- list(
          health = input$health
        )
        generate_data(human, pfpr, travel, lambda, z, module_choices)
        })
    output$greeting <- renderText({
        obs <- observations()
        paste0("Days: ", nrow(obs$bloodmeal_mosquito))
    })
    output$bites <- renderPlot({
        bm <- observations()$bloodmeal_mosquito
        bm$Location <- as.factor(bm$Location)
        ggplot(bm, aes(Time, Bites, colour = Location)) + geom_point() +
            labs(title = "Bites in Each Location During Time Step", y = "Bites per Day")
    }, res = 96)
    output$humanBites <- renderPlot({
        bh <- observations()$bloodmeal_human
        ggplot(bh, aes(times)) + geom_histogram(binwidth = 10 / 40) +
            labs(title = "Bites for all Humans During Time Step", y = "Bites per Quarter Day")
    }, res = 96)
    output$mosyz <- renderPlot({
        mosy <- observations()$mosquito[, c("Location", "Time", "M", "Y", "Z")]
        ggplot(data=mosy, aes(x=Time, y = Z)) + geom_point() + facet_wrap(~Location)
    }, res = 96)
}


# Run the application
shinyApp(ui = ui, server = server)
