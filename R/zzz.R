#' @include utils.R
#' @importFrom  rjd3jars reload_dictionaries
#' @importFrom  rjd3jars check_java_version
#' @import rjd3xjars
NULL

.onLoad <- function(libname, pkgname) {
    if (!requireNamespace("rjd3xjars", quietly = TRUE)) {
        stop("Loading {rjd3xjars} failed", call. = FALSE)
    }
    if (!requireNamespace("rjd3toolkit", quietly = TRUE)) {
        stop("Loading {rjd3toolkit} failed", call. = FALSE)
    }
    # Loading Java class
    jar_dir <- file.path(libname, pkgname, "inst", "java")
    jars_inst <- list.files(
        jar_dir,
        pattern = "\\.jar$",
        full.names = TRUE,
        all.files = TRUE
    )
    result <- rJava::.jpackage(
        pkgname,
        lib.loc = libname,
        morePaths = jars_inst
    )
    if (!result)
        stop("Loading java packages failed")

    proto.dir <- system.file("proto", package = pkgname)
    RProtoBuf::readProtoFiles2(protoPath = proto.dir)



    assign("sts", list(), rjd3toolkit::.jd3_env)

    # reload extractors
    if (rjd3jars::check_java_version()) rjd3jars::reload_dictionaries()

}

#' Set an option for sts
#'
#' @param name Name of the option
#' @param obj Option
#'
#' @export
#'
#' @examples
#' sts_option("test", "DUMMY")
sts_option<-function(name, obj){
    options<-rjd3toolkit::.jd3_env$sts
    options[[name]]<-obj
    assign("sts", options, rjd3toolkit::.jd3_env)
    invisible()
}

#' Set an option for sts
#'
#' @param name Name of the option
#'
#' @returns The requested option or NULL if it doesn't exist
#' @export
#'
#' @examples
#' sts_option("test", "DUMMY")
#' get_sts_option("test")
get_sts_option<-function(name){
    options<-rjd3toolkit::.jd3_env$sts
    return (options[[name]])
}

