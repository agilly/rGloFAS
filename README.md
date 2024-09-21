# `rGloFAS`, a package to download and process GloFAS data

## Installation
```r
pak::pkg_install("agilly/rGloFAS")
```
## Example notebook

Have a look at `inst/examples/example.ipynb`.

## Usage
```r
library(rGloFAS)
# get the list of AOIs for the user (define this in the portal, or using rGloFAS, see below)
myAOIs=listUserAOI()

# get the list of products for that AOI during the past 7 days
myProducts=listAOIProducts(aoi_id=myAOIs$aoi_id[1], start_date=Sys.Date()-7, end_date=Sys.Date())

# get layer "Observed Flood Extent" for the first product
filePaths=getSingleLayer(cell_code=myProducts$cell_code[1], aoi_id=myAOIs$aoi_id[1], layer=getLayerNumbers()["Observed Flood Extent"])

```

#### Order a max flood extent
```r
# get the list of AOIs for the user (define this in the portal, or using rGloFAS, see below)
myAOIs=listUserAOI()

# order a max flood extent for the first AOI and get the order ID
flext=orderMaxFloodExtent(aoi_id = myAOIs$aoi_id[1], start_date = Sys.Date()-15, end_date = Sys.Date())

# check the status
checkMaxFloodExtentOrderStatus(flext$order_id)
```	

Unfortunately, there is no way to link the order ID to the download link, so you will have to use the download link you receive per email.

#### Create an AOI

```r
# assume laoadm is a GADM object read as SF
selected_names <- c("Sisattanak", "Xaysetha", "Hadxaifong")
# Filter the sf object to include only the specified names
selected_areas <- laoadm[laoadm$NAME_2 %in% selected_names, ]
# Compute the union of these geometries
union_geometry <- st_union(selected_areas)
library(geojsonio)
geojson=geojson_list(union_geometry)
attr(geojson, "class")= "list"
createAOI(aoi_name = "South VTE", description = "Sisattanak, Xaysetha, Hadxaifong", geoJSON = geojson)
```

You will receive the AOI ID in the response. You can use this ID to order products for this AOI.
