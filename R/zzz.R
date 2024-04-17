.onAttach <- function(libname, pkgname) {
  startup_message()
}


startup_message <- function(){
  cli::cli_text(cat(
    cli::col_black(
      "
             .
            \":\"
          ___:____     |\"\\/\"|
        ,'        `.    \\  /
        |  O        \\___/ |
      ~^~^~^~^~^~^~^~^~^~^~^~^~
          framrsquared 0.2.0
              "
    )
  ))
}
