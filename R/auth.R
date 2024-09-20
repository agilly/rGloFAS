



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
    if(response$status_code!=200){
        print(response)
        stop("API call failed. Please check your request.")
    }
    # if the response is 200, return the content
    return(httr::content(response))
}


# a function that authenticates the user with the API
#' @export
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
        path=system.file("config/config.yml", package=utils::packageName())
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

generateSFFromProduct=function(product) {
  # Extract the coordinates from the geoJSON structure
  coordinates = do.call(rbind, lapply(product$layer_boundary$coordinates[[1]], function(coord) {
    c(coord[[1]], coord[[2]])
  }))
  
  # Create a polygon
  polygon <- st_polygon(list(coordinates))
  
  # Return an sf object with metadata
  st_sf(
    product_id = product$product_id,
    product_time = product$product_time,
    latest_product = product$latest_product,
    cell_code = product$cell_code,
    geometry = st_sfc(polygon, crs = 4326)
  )
}

#' @export
#' @importFrom data.table rbindlist
#' @importFrom sf st_polygon st_sfc
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

#' @export
#' @importFrom httr modify_url
listAOIProducts=function(aoi_id, start_date=NULL, end_date=NULL, all=F){
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
        url=glue("{url}&start_date={start_date}&end_date={end_date}")
    }

    authHeader=list("Authorization"=pkgenv$token)
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

getSingleLayer=function(cell_code, aoi_id, layer){
    downloadLink=getSingleLayerDownloadLink(cell_code, aoi_id, layer)
    downloadDir=tempdir()
    downloadDir=file.path(downloadDir, cell_code, aoi_id, layer)
    # if the directory exists, delete it and its contents
    if(dir.exists(downloadDir)){
        unlink(downloadDir, recursive=T)
    } else{
        dir.create(downloadDir, recursive = T)
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