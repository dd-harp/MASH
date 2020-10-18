# Set up logging for this package.
# This creates a function to set up logging for different
# circumstances and calls for debug, warn, info, and error
# calls. It is set up in a way that should make it easier
# to switch which logging library we use, because there are
# many competing logging libraries.

#' This is the namespace in which all logging messages
#' are placed, so that they can be controlled as a group.
.baseLogger <- ifelse(is.null(packageName()), "macro", packageName())
#' Create a child namespace that also logs errors to a file.
.errorLogger <- paste0(.baseLogger, ".err")


.name_to_level <- list(
  "trace" = futile.logger::TRACE,
  "debug" = futile.logger::DEBUG,
  "info" = futile.logger::INFO,
  "warn" = futile.logger::WARN,
  "error" = futile.logger::ERROR,
  "fatal" = futile.logger::FATAL
)


string_log_level <- function(levelName) {
  if (tolower(levelName) %in% names(.name_to_level)) {
    .name_to_level[[tolower(levelName)]]
  } else {
    cat(paste("Could not set logger because level",
              level, "isn't one of trace, debug,",
              "info, warn, error, or fatal."))
    .name_to_level[["debug"]]
  }
}


#' Set up logging for use on a local machine.
#' @param level_name One of the strings (trace, debug, info, warn, error, fatal)
#' This creates a logger that is named after the package
#' so that all logging messages for this package can be turned on or off
#' together. This function makes everything go to the console on
#' standard out.
local_logging <- function(level_name = "info") {
  invisible(futile.logger::flog.logger(
    .baseLogger,
    threshold=string_log_level(level_name),
    layout=futile.logger::layout.format('~l [~t] ~n:~f ~m')
    ))
}


#' Set up logging for running on the cluster.
#' @param level_name One of the strings (trace, debug, info, warn, error, fatal)
#' This sets logging so that warnings and above go to a file that is
#' named by the date and time, but all else goes to standard out.
cluster_logging <- function(level_name = "info") {
  error_file <- format(Sys.time(), "%Y%m%d-%H%M%S.txt")
  futile.logger::flog.logger(
    .baseLogger,
    threshold=string_log_level(level_name),
    layout=futile.logger::layout.format('~l [~t] ~n:~f ~m')
    )
  futile.logger::flog.logger(
    .errorLogger,
    threshold=string_log_level(level_name),
    appender=appender.file(error_file),
    layout=futile.logger::layout.format('~l [~t] ~n:~f ~m')
    )
}


#' Set the logging level on some part of a package.
#' @param module The string name of a section of the package.
#' @param level_name One of the strings (trace, debug, info, warn, error, fatal)
#' If you want to debug one module, then you can set its level differently
#' here from the rest of the package.
log_module <- function(module, level_name = "debug") {
  level <- string_log_level(level_name)
  futile.logger::threshold(level, name = paste0(.baseLogger, ".", module))
  futile.logger::threshold(level, name = paste0(.errorLogger, ".", module))
}


logtrace <- function(...) {
  futile.logger::flog.trace(..., name = .baseLogger)
}


logdebug <- function(...) {
  arglist <- list(...)
  if ("name" %in% names(arglist)) {
    arglist[names(arglist) == "name"] <- paste0(.baseLogger, ".", arglist$name)
    do.call(futile.logger::flog.debug, arglist)
  } else {
    futile.logger::flog.debug(..., name = .baseLogger)
  }
}


loginfo <- function(...) {
  arglist <- list(...)
  if ("name" %in% names(arglist)) {
    arglist[names(arglist) == "name"] <- paste0(.baseLogger, ".", arglist$name)
    do.call(futile.logger::flog.info, arglist)
  } else {
    futile.logger::flog.info(..., name = .baseLogger)
  }
}


logwarn <- function(...) {
  arglist <- list(...)
  if ("name" %in% names(arglist)) {
    arglist[names(arglist) == "name"] <- paste0(.errorLogger, ".", arglist$name)
    do.call(futile.logger::flog.warn, arglist)
  } else {
    futile.logger::flog.warn(..., name = .errorLogger)
  }
}


logerror <- function(...) {
  arglist <- list(...)
  if ("name" %in% names(arglist)) {
    arglist[names(arglist) == "name"] <- paste0(.errorLogger, ".", arglist$name)
    do.call(futile.logger::flog.error, arglist)
  } else {
    futile.logger::flog.error(..., name = .errorLogger)
  }
}


logfatal <- function(...) {
  arglist <- list(...)
  if ("name" %in% names(arglist)) {
    arglist[names(arglist) == "name"] <- paste0(.errorLogger, ".", arglist$name)
    do.call(futile.logger::flog.fatal, arglist)
  } else {
    futile.logger::flog.fatal(..., name = .errorLogger)
  }
}
