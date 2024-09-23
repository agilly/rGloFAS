



genericGloFASAPICall=function(endpoint, method="GET", body=NULL, headers=NULL, encode="json"){
    if(is.null(headers)){
        headers=list("accept"="application/json", "Content-Type"="application/json")
    }
    # if the token is not stored in the environment, authenticate the user
    response=tryCatch(
        httr::RETRY(verb=method, url=glue("{API_BASE}/{endpoint}"), 
        body=body, do.call(add_headers, headers), encode=encode, times=3)
        , error=function(e){
            stop(e$message)
        }
    )
    # if the response is not 200, throw an error
    if(response$status_code>201){
        print(response)
        stop("API call failed. Please check your request.")
    }
    # if the response is 200, return the content
    return(httr::content(response))
}


# a function that authenticates the user with the API
#' @importFrom httr POST
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom glue glue
glofasAPILogin=function(force=FALSE){
    # if we are still logged in, return the token
    if(!is.null(pkgenv$token) && !is.null(pkgenv$expiresAt) && Sys.time()<pkgenv$expiresAt && !force){
        return(invisible())
    }
    # if username and password are already stored in the environment, use them
    if(!is.null(pkgenv$config) && !is.null(pkgenv$config$username) && !is.null(pkgenv$config$password)){
        this_username=pkgenv$config$username
        this_password=pkgenv$config$password
    }else{
        # ask the user for username and password
        this_username=readline("Enter your username: ")
        this_password=readline("Enter your password: ")
    }
    # using httr wrap curl -X 'POST' 'https://api.gfm.eodc.eu/v1/auth/login' -H 'accept: application/json' -H 'Content-Type: application/json' -d '{ "email": "arthur.leonard.gilly@gmail.com", "password": "oxQ%!vGsEzd2VWa3" }'
    # in a tryCatch block to catch errors 
    body=list(email=this_username, password=this_password)
    response=genericGloFASAPICall("auth/login", method="POST", body=body)
        


    # if username/password is not in the environment, store them in the config.yml file and the environment
    if(is.null(pkgenv$config$username) & is.null(pkgenv$config$password)){
        pkgenv$config$username=this_username
        pkgenv$config$password=this_password
        existing_config=pkgenv$config
        existing_config$username=this_username
        existing_config$password=this_password
        path=system.file("config/config.yml", package=pkgload::pkg_name())
        cli::cli_inform("Storing username and password in {path}")
        yaml::write_yaml(existing_config, file=path)
    }

    # store the token in the environment
    pkgenv$token=glue("bearer {response$access_token}")
    pkgenv$clientId=response$client_id
    pkgenv$expiresAt=Sys.time()+as.numeric(response$expires_in)

        # if the response is 200, store the token in the environment
        return(invisible)
}

generateSFFromAOI=function(aoi) {
  # Extract the coordinates from the geoJSON structure
  coordinates = do.call(rbind, lapply(aoi$geoJSON$coordinates[[1]], function(coord) {
    c(coord[[1]], coord[[2]])
  }))
  
  # Create a polygon
  polygon <- st_polygon(list(coordinates))
  
  # Return an sf object with metadata
  st_sf(
    aoi_id = aoi$aoi_id,
    aoi_name = aoi$aoi_name,
    description = aoi$description,
    geometry = st_sfc(polygon, crs = 4326)
  )
}

generateSFFromProduct = function(product) {
  # Extract the coordinates from the geoJSON structure
  coordinates = do.call(rbind, lapply(product$layer_boundary$coordinates[[1]], function(coord) {
    c(as.numeric(coord[[1]]), coord[[2]])
  }))
  
  # Create a polygon
  polygon = st_polygon(list(coordinates))
  
  # Create a list to hold the product attributes
  product_attributes = list()
  
  # Iterate over the names of the product and add them to the list
  for (name in names(product)) {
    if (name != "layer_boundary") {
      product_attributes[[name]] = product[[name]]
    }
  }
  
  # Add the geometry to the list
  product_attributes$geometry = st_sfc(polygon, crs = 4326)
  
  # Return an sf object with metadata
  st_sf(product_attributes)
}


#' List User AOIs
#'
#' This function retrieves the list of Areas of Interest (AOIs) for the current user and converts them into an `sf` object.
#'
#' @return An `sf` object containing the user's AOIs.
#' @export
#' @importFrom data.table rbindlist
#' @importFrom sf st_polygon st_sfc st_sf
#' @examples
#' \dontrun{
#'   user_aois <- listUserAOI()
#'   print(user_aois)
#' }
listUserAOI=function(){
    glofasAPILogin()
    authHeader=list("Authorization"=pkgenv$token)
    response=genericGloFASAPICall(glue("aoi/user/{pkgenv$clientId}"), headers=authHeader)
    if(is.null(response$aois))
        cli::cli_abort("No AOIs found for this user.")
    
    # Convert the list of AOIs to an sf object
    aoi_list = lapply(response$aois, generateSFFromAOI)
    aoi_sf = do.call(rbind, aoi_list)
    return(aoi_sf)
}

#' List AOI Products
#'
#' This function retrieves the list of products for a specified Area of Interest (AOI) and converts them into an `sf` object.
#'
#' @param aoi_id The ID of the Area of Interest (AOI).
#' @param start_date The start date for the product search (optional, default is NULL).
#' @param end_date The end date for the product search (optional, default is NULL).
#' @param all Logical, if TRUE, retrieves all products regardless of date (default is FALSE).
#' @param verbose Logical, if TRUE, prints additional information during the function execution (default is FALSE).
#' @return An `sf` object containing the products for the specified AOI.
#' @note By default, the function retrieves the latest product for the specified AOI. If the `start_date` and `end_date` parameters are provided, the function retrieves products within the specified date range.
#' @export
#' @importFrom httr modify_url
#' @examples
#' \dontrun{
#'   products <- listAOIProducts(aoi_id = "12345", start_date = "2023-01-01", end_date = "2023-01-31")
#'   print(products)
#' }
listAOIProducts=function(aoi_id, start_date=NULL, end_date=NULL, all=F, verbose=F){
    glofasAPILogin()
    # if start_date and end_date are not provided and all=F, set time to "latest", if all=T, set time to "all", else set to "range"
    if(is.null(start_date) & is.null(end_date) & !all){
        this_time="latest"
    }else if(is.null(start_date) & is.null(end_date) & all){
        this_time="all"
    }else{
        this_time="range"
    }

    url=glue("aoi/{aoi_id}/products")
    url=glue("{url}?time={this_time}")

    if(this_time=="range"){
        if(is.null(start_date) | is.null(end_date)){
            cli::cli_abort("Please provide both start_date and end_date.")
        }
        # format as YYYY-MM-DDTHH:MM:SS
        start_date=format(as.POSIXct(start_date), "%Y-%m-%dT%H:%M:%S")
        end_date=format(as.POSIXct(end_date), "%Y-%m-%dT%H:%M:%S")
        url=glue("{url}&from={start_date}&to={end_date}")
        # replace : with %3A
        #url=gsub(":", "%3A", url)
    }

    authHeader=list("Authorization"=pkgenv$token)
    if(verbose){
        cli::cli_inform(glue("Calling {url}"))
    }
    response=genericGloFASAPICall(url, headers=authHeader)
    if(is.null(response$products))
        cli::cli_abort("No products found for this AOI.")
    product_list = lapply(response$products, generateSFFromProduct)
    product_sf = do.call(rbind, product_list)
    return(product_sf)
}

getSingleLayerDownloadLink=function(cell_code, aoi_id, layer){
    # layer = 1:12 where:
    # 1: Observed Flood Extent
    # 2: Observed Water Extent
    # 3: Reference Water Mask
    # 4: Exclusion Mask
    # 5: Likelihood values
    # 6: Advisory Flags
    # 7: Sentinel-1 metadata
    # 8: Sentinel-1 footprint
    # 9: Sentinel-1 Schedule
    # 10: Affected Population
    # 11: Affected landcover
    # 12: Observed Water Extent (vector)
    glofasAPILogin()
    authHeader=list("Authorization"=pkgenv$token)
    response=genericGloFASAPICall(glue("download/scene-file/{cell_code}/{aoi_id}/{layer}"), headers=authHeader)
    if(! "download_link" %in% names(response))
        cli::cli_abort("No download link found for this layer.")
    return(response$download_link)
}

#' Get Layer Numbers
#'
#' This function returns a named vector of layer numbers and their corresponding descriptions.
#'
#' @return A named vector where the names are descriptions of the layers and the values are their corresponding numbers.
#' @export
#' @examples
#' layers <- getLayerNumbers()
#' print(layers)
getLayerNumbers=function(){
    return(
        setNames(1:12,
            c("Observed Flood Extent", "Observed Water Extent", "Reference Water Mask", "Exclusion Mask", "Likelihood values", "Advisory Flags", "Sentinel-1 metadata", "Sentinel-1 footprint", "Sentinel-1 Schedule", "Affected Population", "Affected landcover", "Observed Water Extent (vector)")
        )
    )
}

#' Get Single Layer
#'
#' This function downloads a single layer for a specified cell code and Area of Interest (AOI) ID, unzips the downloaded file, and returns the list of files.
#'
#' @param cell_code The cell code for the layer to be downloaded.
#' @param aoi_id The Area of Interest (AOI) ID for the layer to be downloaded.
#' @param layer The layer number to be downloaded.
#' @return A character vector containing the file paths of the unzipped files.
#' @export
#' @examples
#' \dontrun{
#'   files <- getSingleLayer(cell_code = "12345", aoi_id = "67890", layer = "1")
#'   print(files)
#' }
getSingleLayer=function(cell_code, aoi_id, layer){
    downloadLink=getSingleLayerDownloadLink(cell_code, aoi_id, layer)
    downloadDir=tempdir()
    downloadDir=file.path(downloadDir, cell_code, aoi_id, layer)
    # if the directory exists, delete it and its contents
    if(dir.exists(downloadDir)){
        unlink(downloadDir, recursive=T)
    } 
    dir.create(downloadDir, recursive = T)
    
    # loop until the directory exists
    while(!dir.exists(downloadDir)){
        Sys.sleep(1)
    }
    downloadPath=file.path(downloadDir, "download.zip")
    download.file(downloadLink, downloadPath)
    unzip(downloadPath, exdir=downloadDir)
    unlink(downloadPath)
    return(list.files(downloadDir, full.names=T))
}


getRelevantFileset=function(bbox, fileset){
    relevant_files=c()
    relevant_raster=list()
    for(f in fileset){
        r=rast(f)
        if(!is.null(intersect(ext(r), ext(bbox)))){
            cli::cli_inform(glue("Found relevant file: {f}"))
            relevant_files=c(relevant_files, f)
            # mask the raster with the bbox
            relevant_raster=c(relevant_raster, mask(r, ext(bbox)))
        }
    }
    return(relevant_raster)

}

#' Create Area of Interest (AOI)
#'
#' This function creates an Area of Interest (AOI) by making an API call with the provided parameters.
#'
#' @param aoi_name The name of the Area of Interest (AOI).
#' @param description A description of the AOI.
#' @param geoJSON The geoJSON defining the AOI.
#' @return The response from the API call, typically containing details about the created AOI.
#' @export
#' @examples
#' \dontrun{
#'   response <- createAOI(aoi_name = "My AOI", description = "Description of my AOI", geoJSON = geojson_data)
#'   print(response)
#' }
createAOI=function(aoi_name, description, geoJSON){#, region, skip_aoi_check=F){
    glofasAPILogin()
    authHeader=list("Authorization"=pkgenv$token)
    body=list(
        aoi_name=aoi_name, 
        description=description, 
        user_id=pkgenv$clientId, 
        geoJSON=geoJSON
        # region=region,
        # skip_aoi_check=skip_aoi_check
        )
    response=genericGloFASAPICall("aoi/create", method="POST", body=body, headers=authHeader)
    return(response)
}

#' Order Maximum Flood Extent
#'
#' This function orders the maximum flood extent for a specified Area of Interest (AOI) ID and date range.
#'
#' @param aoi_id The Area of Interest (AOI) ID.
#' @param start_date The start date for the flood extent order.
#' @param end_date The end date for the flood extent order.
#' @param output_type The type of output to be returned. Must be one of "both", "raster", or "vector". Default is "both".
#' @return The response from the API call, typically containing details about the order.
#' @export
#' @examples
#' \dontrun{
#'   response <- orderMaxFloodExtent(aoi_id = "12345", start_date = "2023-01-01", end_date = "2023-01-31", output_type = "both")
#'   print(response)
#' }
orderMaxFloodExtent = function(aoi_id, start_date, end_date, output_type = "both") {
    glofasAPILogin()
    # error if output_type is not one of "both", "raster", "vector"
    if (length(output_type) != 1 | !output_type %in% c("both", "raster", "vector"))
        cli::cli_abort("output_type must be one of 'both', 'raster', 'vector'")
    
    # error if start_date is not before end_date 
    if (start_date > end_date)
        cli::cli_abort("start_date must be before end_date")
    
    # error if the diff is >2 months
    if (difftime(end_date, start_date, units = "weeks") > 8)
        cli::cli_abort("The difference between start_date and end_date must be less than 8 weeks.")
    
    authHeader = list("Authorization" = pkgenv$token)
    url = "download/max_flood_extent"
    # format as YYYY-MM-DDTHH:MM:SS
    start_date = format(as.POSIXct(start_date), "%Y-%m-%dT%H:%M:%S")
    end_date = format(as.POSIXct(end_date), "%Y-%m-%dT%H:%M:%S")
    # the url becomes /{aoi_id}/{start_date}/{end_date} and output_type is added as a query parameter
    url = glue("{url}/{aoi_id}/{start_date}/{end_date}?output_type={output_type}")
    response = genericGloFASAPICall(url, headers = authHeader)
    return(response)
}

#' Check Maximum Flood Extent Order Status
#'
#' This function checks the status of a maximum flood extent order by order ID.
#'
#' @param order_id The order ID for the maximum flood extent.
#' @return The response from the API call, typically containing the status of the order.
#' @export
#' @examples
#' \dontrun{
#'   status <- checkMaxFloodExtentOrderStatus(order_id = "12345")
#'   print(status)
#' }
checkMaxFloodExtentOrderStatus = function(order_id) {
    glofasAPILogin()
    authHeader = list("Authorization" = pkgenv$token)
    url = glue("download/max_flood_extent/{order_id}")
    response = genericGloFASAPICall(url, headers = authHeader)
    return(response)
}

getProductInfo=function(product_id){
    glofasAPILogin()
    authHeader=list("Authorization"=pkgenv$token)
    url=glue("products/{product_id}")
    response=genericGloFASAPICall(url, headers=authHeader)
    return(response)
}
