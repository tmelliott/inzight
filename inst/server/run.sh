#!/bin/sh

Rscript -e "plumber::pr('inst/server/app.R') |> plumber::pr_run(port = 4567, host = '0.0.0.0')"
