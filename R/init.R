
pkgenv=new.env()
API_BASE="https://api.gfm.eodc.eu/v2"
.onLoad=function(libname, pkgname){

    # if there is a config.yml file in the package inst directory, read it
    confpath=system.file("config/config.yml", package=utils::packageName())
    if(file.exists(confpath)){
        pkgenv$config=yaml::yaml.load_file(confpath)
        # set all the contents in the config.yml file as elements in the environment
    }



}