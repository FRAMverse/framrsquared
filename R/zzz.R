.onAttach <- function(libname, pkgname) {
  packageStartupMessage(startup_message())
}


startup_message <- function(){
  "
             .
            \":\"
          ___:____     |\"\\/\"|
        ,'        `.    \\  /
        |  O        \\___/ |
      ~^~^~^~^~^~^~^~^~^~^~^~^~
          framrsquared 0.6.0
              "
}

## to track connections and clean up orphans
.fram_connections <- new.env(parent = emptyenv())
