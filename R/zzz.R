#' @include utils.R
#' @import rjd3xjars
NULL

.onLoad <- function(libname, pkgname) {
    result <- .jpackage(pkgname, lib.loc = libname)
    if (!result)
        stop("Loading Java packages failed")

    proto.dir <- system.file("proto", package = pkgname)
    RProtoBuf::readProtoFiles2(protoPath = proto.dir)

    # reload extractors
    try({
        .jcall(
            "jdplus/toolkit/base/api/information/InformationExtractors",
            "V",
            "reloadExtractors"
        )
    })
}
