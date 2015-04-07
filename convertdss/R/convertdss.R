#' function  
#' 
#' initilizes a ncdf file to recieve dss data
#' 
#' Uses an open dss file. A time dimension is defined with a corresponding 
#' dimensional variable (a variable with the same name as the dimension).
#' 
#' It is important that all the times be defined before any data gets written,
#' netcdf works like a large array, and so inserting new times only works at the 
#' end of a series. It is not possible to insert values at times that don't already 
#' exist if those times are at the beginning or middle of a time dimension. This 
#' makes it vary tricky to convert an arbirtrary dss file which may have irregular 
#' timeseries data. For this reason, initialization of time stamps is left to the user.
#' 
#' @return 
#' @note NOTE
#' @author Cameron Bracken
#' @export 
dss_to_ncdf_init <- function(dss,datetimes,nc_file='convertdss.nc',overwrite=TRUE){

    nc = create.nc(nc_file, clobber=overwrite, large=TRUE, share=FALSE, prefill=FALSE)
    
    # Define a time dimension 
    dim.def.nc(nc, "time", unlim=TRUE)
    dim.def.nc(nc, "max_string_length", nchar("0000-00-00 00-00-00 UTC"))
    var.def.nc(nc, "time", "NC_CHAR", c("max_string_length", "time"))
    var.put.nc(nc, "time", format(datetimes, '%Y-%m-%d %H:%M:%S UTC'))
    #utinvcal.nc("hours since 1900-01-01 00:00:00",format(vv[[1]]$data$datetime,'%Y-%m-%d %H-%M-%S'))
    
    ##  Put some global attributes
    att.put.nc(nc, "NC_GLOBAL", "title", "NC_CHAR", sprintf("Data from %s", basename(dss$getFilename())))
    att.put.nc(nc, "NC_GLOBAL", "history", "NC_CHAR", paste("Created: ", Sys.time()))

    return(nc)
}

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
dss_var_to_ncdf <- function(v, nc, nc_datetimes=NULL){
    
    # if the user does not supply nc datetimes of the variable, 
    # read them out of the nc file
    if(is.null(nc_datetimes))
        nc_datetimes = ymd_hms(var.get.nc(nc, 'time'))

    var_name = v$variable
    md = lapply(v$metadata,as.character)

    dss_datetimes = v$data$datetime

    # check if dates match up
    #first_dates_match = (dss_datetimes[1] == nc_datetimes[1])
    #lengths_match = (length(dss_datetimes) == length(nc_datetimes))
    all_match = all(dss_datetimes == nc_datetimes[1:length(dss_datetimes)])
    init_offset = (dss_datetimes[1] %in% nc_datetimes)

    written = FALSE
    if(all_match){
         # define the variable, with no data
        var.def.nc(nc, var_name, "NC_DOUBLE", "time")
        att.put.nc(nc, var_name, "missing_value", "NC_DOUBLE", -99999.9)
        # we can write all the data from the index
        var.put.nc(nc, var_name, v$data$value)
        written = TRUE
    }else if(init_offset){
        # the value starts at another index but we need to 
        # check datetimes all match up with the existing nc datetimes
        # TODO: possibly grow the time dimension
        init_index = which(dss_datetimes[1] == nc_datetimes)
        ldss = length(dss_datetimes)
        lnc = length(nc_datetimes)

        runs_over = (ldss > length(nc_datetimes[init_index:lnc]))

        if(runs_over){
            warning('DSS variable and NCDF times do not match, skipping')
        }else{
            all_match2 = all(dss_datetimes == nc_datetimes[init_index:(init_index + ldss - 1)])
            var.def.nc(nc, var_name, "NC_DOUBLE", "time")
            att.put.nc(nc, var_name, "missing_value", "NC_DOUBLE", -99999.9)
            var.put.nc(nc, var_name, v$data$value, start=init_index)
            written = TRUE
        }
    }

    if(written){
        # add the metadata
        for(m in names(md))
            att.put.nc(nc, var_name, m, "NC_CHAR", as.character(md[[m]]))
    }

    return(written)

}

#' dss_to_netcdf  
#' 
#' Convert dss to netcdf
#' 
#' This function does the bulk of the work to convert dss data to netcdf,
#' but not all of it. In particular, you will need to call \code{\link{dss_to_ncdf_init}}
#' first to set up the netcdf file. The way dss and netcdf handle time data
#' is slightly different and so some data will not convert nicely. See the 
#' description of \code{\link{dss_to_ncdf_init}} for more information
#'
#' @param dss a dss file handle, from opendss
#' @param nc a nc file handle, from dss_to_ncdf_init
#' 
#' @return Outputs a NetCDF file
#' @note NOTE
#' @author Cameron Bracken
#' @seealso \code{\link[dssrip]{opendss}}, \code{\link{dss_to_ncdf_init}}
#' @export 
dss_to_netcdf <- function(dss, nc, variable_parts='B'){

    nc_datetimes = ymd_hms(var.get.nc(nc, 'time'))

    parts = separate_path_parts(getAllPaths(dss),variable_parts)
    dss_variables = unique(parts$id_var)[1:10]

    lapply(dss_variables, function(var)
        dss_var_to_ncdf(read_dss_variable(var,dss,parts, variable_parts), nc, nc_datetimes))


}

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

#' function  
#' 
#' short description
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
#'
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
    require(dssrip)
    getFullTSC(dss, fullPathByWildcard(paths))
}

posixct_to_dss_time <- function(x){
    as.integer(x)/60 + 2209075200/60
}

dss_time_to_posixct <- function(x){
    as.POSIXct(x*60, origin="1899-12-31 00:00", tz="UTC")
}

separate_path_parts <- function (paths,variable_parts = 'B') 
{
    parts.df = data.frame(rbind(do.call(rbind, str_split(paths, 
        fixed("/")))[, 2:7]),stringsAsFactors=FALSE)
    colnames(parts.df) = toupper(letters[1:6])
    parts.df$PATH = paths
    parts = data.table(parts)
    idv = apply(as.matrix(parts[,variable_parts,with=F]),1,paste,collapse='_')
    parts[,id_var:=idv]
    return(parts)
}

print.NetCDF <- function(x)print.nc(x)