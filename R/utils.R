#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
#' @importFrom RProtoBuf readProtoFiles2
#' @importFrom stats frequency is.ts pt start ts
#' @importFrom rjd3toolkit result dictionary
NULL

.jd2r_diffuse_likelihood<-function(jrslt){
    return (structure(list(
        nobs=.jcall(jrslt, "I", "dim"),
        ndiffuse=.jcall(jrslt, "I", "getD"),
        ll=.jcall(jrslt, "D", "logLikelihood"),
        ssq=.jcall(jrslt, "D", "ssq"),
        ldet=.jcall(jrslt, "D", "logDeterminant"),
        dcorr=.jcall(jrslt, "D", "getDiffuseCorrection")
    ),
    class = "JD3DIFFUSELIKELIHOOD"))
}
