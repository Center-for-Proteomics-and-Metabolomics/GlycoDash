# Use "Source" or "Source with echo" to launch dashboard.


options(golem.app.prod = FALSE)

golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

golem::document_and_reload()

run_app()
