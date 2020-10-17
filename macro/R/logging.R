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
local_logging <- function(level_name = "info") {
  invisible(futile.logger::flog.logger(
    .baseLogger,
    threshold=string_log_level(level_name),
    layout=futile.logger::layout.format('~l [~t] ~n:~f ~m')
    ))
}


#' Set up logging for running on the cluster.
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


logtrace <- function(...) {
  futile.logger::flog.trace(..., name = .baseLogger)
}


logdebug <- function(...) {
  futile.logger::flog.debug(..., name = .baseLogger)
}


loginfo <- function(...) {
  futile.logger::flog.info(..., name = .baseLogger)
}


logwarn <- function(...) {
  futile.logger::flog.warn(..., name = .errorLogger)
}


logerror <- function(...) {
  futile.logger::flog.error(..., name = .errorLogger)
}


logfatal <- function(...) {
  futile.logger::flog.fatal(..., name = .errorLogger)
}
