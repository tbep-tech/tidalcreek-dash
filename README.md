# README

Materials for TBEP tidal creek dashboard.

[Shiny](http://shiny.tbep.org/tidalcreek-dash/)

[![DOI](https://zenodo.org/badge/231956629.svg)](https://zenodo.org/badge/latestdoi/231956629)

## How the dashboard runs

The dashboard is a flexdashboard document (`tidalcreek-dash.Rmd`) that uses `runtime: shiny_prerendered`. This affects how updates reach users.

* The page layout is rendered once to `tidalcreek-dash.html` (plus `tidalcreek-dash_files/`) and reused for every visitor. These files are created on the server and are not tracked by git.
* The `setup` chunk (`context='setup'`) runs once when an R process starts. It loads the packages, the files in `data/`, and the functions in `R/funcs.R`. These objects are shared by the page and the server.
* The chunks with `context='server'` run once for each visitor. They hold the reactives, the maps, the plot and table outputs, and the download handlers.
* The plots on each page are only computed when a visitor first opens that page.

## Updating the dashboard

After pulling changes on the TBEP server, make sure running R processes pick up the new data. To restart the app right away, create or update a `restart.txt` file in the app directory on the server (e.g., `touch restart.txt`). No other files need to be deleted or rendered by hand. rmarkdown re-renders `tidalcreek-dash.html` on the next request if `tidalcreek-dash.Rmd`, `R/funcs.R`, the files in `data/`, the header files, or the files in `www/` (including `styles.css`) are newer than `tidalcreek-dash.html`. Pulling new files gives them a newer modified time, so this happens automatically.

## Troubleshooting

* __The dashboard still shows old data.__ Update `restart.txt` as described above and reload the page. If that does not help, delete `tidalcreek-dash.html` and the `tidalcreek-dash_files/` folder on the server and update `restart.txt` again. This forces a fresh render on the next request.
* __The app fails to start with "Unable to write prerendered HTML file".__ The Shiny Server user needs write access to the app directory so it can create `tidalcreek-dash.html`. Fix the folder permissions, or render the page once on the server with `rmarkdown::render('tidalcreek-dash.Rmd')` from the app directory.
* __The first visit after an update is slow.__ This is expected. The first request renders `tidalcreek-dash.html`, which takes some time. Later visits reuse it.
* __A plot or map shows an error.__ Run the dashboard locally to see the full R error. In RStudio, open `tidalcreek-dash.Rmd` and click Run Document. From the command line, run `rmarkdown::run('tidalcreek-dash.Rmd')`. Outside RStudio this needs pandoc, which can be pointed to the copy that comes with RStudio by setting the `RSTUDIO_PANDOC` environment variable (e.g., `Sys.setenv(RSTUDIO_PANDOC = 'C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools')`).
* __Editing the dashboard.__ Code that loads data or creates objects needed by both the page and the server goes in `R/funcs.R` or the `setup` chunk. Reactives and `output$...` assignments go in a `context='server'` chunk. The page layout only contains the matching output placeholders (e.g., `plotlyOutput('cntcrkplo')`). Code placed in a regular chunk only runs when the page is rendered and is not available to the server.
