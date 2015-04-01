#' function  
#' 
#' Writes a dss variable to an existing netcdf file
#' 
#' Uses the output of read_dss_variable, creating a new variable in an _existing_
#' netcdf file
#' 
#' @return new netcdf file handle
#' @note NOTE
#' @author Cameron Bracken
#' @export 
dss_var_to_ncdf <- function(v, nc){

    # Define a time dimension 
    time_dim = ncdim_def('time','Time indicies', 1:nrow(v$data), unlim=T)
    var_name = v$variable
    md = lapply(v$metadata,as.character)

    #browser()
    # define the variable, with no data
    var = ncvar_def(var_name, md$units, list(time_dim), missval = NA)

    # Add the variable to an existing nc file, creates the data container
    nc2 = ncvar_add(nc, var)

    # Write the data
    ncvar_put(nc2, var, v$data$value)

    # add the metadata
    for(m in names(md))
        ncatt_put(nc2, var_name, m, as.character(md[[m]]))
    
    #nc_close(nc)

    return(nc2)

}

#' function  
#' 
#' short description
#' 
#' Long Description
#' 
#' @return list of lists containing dss data
#' @note NOTE
#' @author Cameron Bracken
#' @export 
dss_to_netcdf <- function(){



}

#' dss_to_list get data from a dss file as a list of lists 
#' 
#' read dss data into a list
#' 
#' Long Description
#' 
#' @return list of lists containing dss data
#' @note NOTE
#' @author Cameron Bracken
#' @export 
read_dss_list <- function(dss,variable_part = 'B',time_part = 'D'){
    #registerDoParallel(makeCluster(detectCores()))

    paths = getAllPaths(dss)
    parts = data.table(separatePathParts(paths))

    d = dlply(parts,.(get(variable_part)), summarise, 
        read_dss_variable(get(variable_part)[1],PATH,dss))#,.parallel=TRUE)
        
    lapply(d,'[[',1)
}

#' function  
#' 
#' short description
#' 
#' Long Description
#' 
#' @return list of lists containing dss data
#' @note NOTE
#' @author Cameron Bracken
#' @export 
read_dss_variable <- function(variable,path_group,dss){

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