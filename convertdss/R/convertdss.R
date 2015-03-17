#' dss_to_list get data from a dss file as a list of lists 
#' 
#' read dss data
#' 
#' Long Description
#' 
#' @return list of lists containing dss data
#' @note NOTE
#' @author Evan Heisman
#' @export 
read_dss_list <- function(dss,variable_part = 'B',time_part = 'D'){
    # open the DSS file and fiddle with the paths to allow bulk extraction of a 
    # single variable. Internally, DSS stores monthly data in 10 year chunks (why?!?!)
    #require(parallel)
    #require(doParallel)
    require(data.table)

    #registerDoParallel(makeCluster(detectCores()))

    paths = getAllPaths(dss)
    parts = data.table(separatePathParts(paths))

    d = dlply(parts[1:100],.(get(variable_part)), summarise, 
        read_dss_variable(get(variable_part)[1],PATH,dss))#,.parallel=TRUE)
        
    lapply(d,'[[',1)
}

read_dss_variable <- function(variable,path_group,dss){

    # get a single variable as a data.table
    dt = getFullDT(dss,path_group)
    dt$units = NULL

    metadata = attr(dt,'dssMetadata')[1,]
    attr(dt,'dssMetadata') = NULL
    return(list(data=dt,metadata=metadata))
}

# Determine if range of vector is FP 0.
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
    if (length(x) == 1) return(TRUE)
    x <- range(x) / mean(x)
    isTRUE(all.equal(x[1], x[2], tolerance = tol))
}

concatSinglePath <- function(row){
    paste('',paste(row[1:6],collapse='/'),'',sep='/')
}

concatPathParts <- function(parts){
    apply(parts,1,concatSinglePath)
}

get_wildcard_ts <- function(path,dss){
    require(dssrip)
    getFullTSC(dss, fullPathByWildcard(paths))
}

posixct_to_dss_time <- function(x){
    as.integer(x)/60 + 2209075200/60
}

dss_time_to_posixct <- function(x){
    as.POSIXct(x*60, origin="1899-12-31 00:00", tz="UTC")
}

separatePathParts <- function (paths) 
{
    parts.df = data.frame(rbind(do.call(rbind, str_split(paths, 
        fixed("/")))[, 2:7]),stringsAsFactors=FALSE)
    colnames(parts.df) = toupper(letters[1:6])
    parts.df$PATH = paths
    return(parts.df)
}