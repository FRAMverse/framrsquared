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
          framrsquared 0.5.0
              "
}
