# Use "Ctrl+Shift+Enter" to launch the dashboard.
# (Or click `Source` -> `Source with Echo`).


options(golem.app.prod = FALSE)

golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

golem::document_and_reload()

run_app()

