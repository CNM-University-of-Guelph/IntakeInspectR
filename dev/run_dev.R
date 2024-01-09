
# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
# options(shiny.port = httpuv::randomPort())
#options(shiny.port = 14455)
options(shiny.port = 48275)

options(shiny.launch.browser = FALSE)



# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# visualise reactive log in real time in Ctrl + F3 to open vis in browser:
options(shiny.reactlog=TRUE)

# Newer option in shiny for development:
shiny::devmode(devmode = TRUE, verbose = TRUE)


# options(shiny.host='0.0.0.0')
options(shiny.host='127.0.0.1')

run_app()

