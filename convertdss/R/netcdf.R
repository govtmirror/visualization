#' Initilizes a ncdf file to recieve dss data
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
#' @param dss a dss file object from \code{\link[dssrip]{opendss}}
#' @param datetimes a vector of datetimes (such as from \code{\link[libridate]{ymd_hms}})
#'                  corresponding to the exact times to set up in the nc file
#'                  
#' 
#' @return An RNetCDF file handle
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
 
#' Writes a dss variable to an existing netcdf file
#' 
#' Uses the output of read_dss_variable, creating a new variable in an _existing_
#' netcdf file
#' 
#' @param v a variable object returned by \code{\link{read_dss_variable}}
#' @param nc A RNetCDF file handle set up by \code{\link{dss_to_ncdf_init}}
#' @param nc_datetimes the datetimes from the time variable in the netcdf file
#'                     this is for efficiency so the times do not have to be 
#'                     read and converted for each new variable written.
#' 
#' @return TRUE if the write was successful, false otherwise
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

    # if either data is longer, use the shorter length 
    ldss = length(dss_datetimes)
    lnc = length(nc_datetimes)
    ml = min(ldss,lnc)

    # check if dates match up, from the first date
    all_match = all(dss_datetimes[1:ml] == nc_datetimes[1:ml])

    # check also if the dss data starts later then the netcdf first time,
    # no support for any other cases
    first_dates_match = (dss_datetimes[1] == nc_datetimes[1])
    init_offset = (dss_datetimes[1] %in% nc_datetimes & !first_dates_match)

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
dss_to_netcdf <- function(dss, nc, parts=NULL, variable_parts='B'){

    nc_datetimes = ymd_hms(var.get.nc(nc, 'time'))

    if(is.null(parts))
        parts = separate_path_parts(getAllPaths(dss),variable_parts)
    dss_variables = unique(parts$id_var)

    lapply(dss_variables, function(var)
        dss_var_to_ncdf(read_dss_variable(var,dss,parts, variable_parts), nc, nc_datetimes))


}

print.NetCDF <- function(x)print.nc(x)