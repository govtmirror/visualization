#' dss_to_list get data from a dss file as a list of lists 
#' 
#' Read all variable from a dss file into R 
#' 
#' Read all data from a dss file into a list, one variable per element. 
#' Be aware this will probably take a long time. 
#'
#' @param dss a dss file handle, from opendss
#' 
#' @return list of lists containing dss data
#' @note NOTE
#' @author Cameron Bracken
#' @seealso \code{\link{nchar}} which this function wraps
#' @export 
read_dss_list <- function(dss,variable_parts='B'){
    #registerDoParallel(makeCluster(detectCores()))

    paths = getAllPaths(dss)
    parts = separate_path_parts(paths,variable_parts)

    d = dlply(parts,.(id_var), summarise, 
        read_dss_variable_by_path_group(id_var,PATH,dss))#,.parallel=TRUE)
        
    lapply(d,'[[',1)
}

#' Read a single dss variable out of a dss file
#' 
#' This function will read a single variable out of a DSS file, 
#' it will read the list of variables from the dss file and parse it
#' to find the path group unless you specefy a parts data.table, from 
#' separatePathParts, this is because for big DSS files the searching 
#' for the path group can take a few seconds.
#'
#' A path group is a collection of DSS paths that define a single variable
#' in DSS. For some strange reason DSS does not want to put all its data in
#' a single path.
#'
#' A 'variable' is a unique name refering to a collection of paths, it can be 
#' defined by a dingle dss path part (default is "B") or two or more path parts
#' which will be separated by an "_"
#'
#' @param variable dss variable defined by the combination of one or more path parts
#' @param dss a dss file handle from opendss
#' @param parts a dss path parts data.table returned from separate_path_parts, if NULL, 
#'              it will be generated from the dss file (can take time for big dss files)
#' @param variable_parts the letters corresponding to the path parts to use as a unique 
#'                       identifier for a variable, default "B"
#' 
#' @return list containing dss data
#' @note NOTE
#' @author Cameron Bracken
#' @export 
read_dss_variable <- function(variable,dss,parts=NULL,variable_parts='B'){

    if(is.null(parts)){
        paths = getAllPaths(dss)
        parts = separate_path_parts(paths,variable_parts)
    }
    path_group = parts[id_var==variable]$PATH

    # get a single variable as a data.table
    dt = getFullDT(dss,path_group)
    dt$units = NULL

    metadata = attr(dt,'dssMetadata')[1,]
    attr(dt,'dssMetadata') = NULL
    return(list(data=dt,metadata=metadata,variable=variable))
}

#' Read a variable from a dss file as defined by a collection of paths
#' 
#' Long Description
#' 
#' @return a list of data from dss
#' @note NOTE
#' @author Cameron Bracken
#' @export 
read_dss_variable_by_path_group <- function(variable,path_group,dss){

    # get a single variable as a data.table
    dt = getFullDT(dss,path_group)
    dt$units = NULL

    metadata = attr(dt,'dssMetadata')[1,]
    attr(dt,'dssMetadata') = NULL
    return(list(data=dt,metadata=metadata,variable=variable))
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
    getFullTSC(dss, fullPathByWildcard(paths))
}

posixct_to_dss_time <- function(x){
    as.integer(x)/60 + 2209075200/60
}

dss_time_to_posixct <- function(x){
    as.POSIXct(x*60, origin="1899-12-31 00:00", tz="UTC")
}

#' Break up dss variable paths for filtering
#' 
#' Long Description
#' 
#' @return a data.table with with path parts
#' @note NOTE
#' @author Cameron Bracken
#' @export 
separate_path_parts <- function (paths,variable_parts = 'B') 
{
    parts.df = data.frame(rbind(do.call(rbind, str_split(paths, 
        fixed("/")))[, 2:7]),stringsAsFactors=FALSE)
    colnames(parts.df) = toupper(letters[1:6])
    parts.df$PATH = paths
    parts = data.table(parts.df)
    idv = apply(as.matrix(parts[,variable_parts,with=F]),1,paste,collapse='_')
    parts[,id_var:=idv]
    return(parts)
}
