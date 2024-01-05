# OmniLog(R) Phenotype Microarray

This repository is a fork of the R package 'opm' by Markus Göker (see [website](http://www.goeker.org/opm/)). I forked the project to test some changes and to easily share it with colleagues.

Currently this repsoitory applies the following fixes:
* Allow user defined colors in `xy_plot(..)` for instances of `OPMS`. This way the user can use the `col` parameter to either use the default (`col = ""`), use a single color for all curves (e.g. `col = "#5a2474"`), use a predefined color set (e.g. `col = "w3c"`) or specify a custom color vector (e.g. `col = c("#111111", "#222222", "#333333")`).

## Quickstart

If you want to use these fixes, make sure you have followed the installation instruction of the OPM suite (see [Installation](http://www.goeker.org/opm/)). This will of course also install the original `opm` package. Afterwards you can simply override it with this version using the following command.
```r
remotes::install_github("tumbleowlee/opm")
```

# Disclaimer

I am neither the creator or maintainer of the R package `opm`. I am just a user of it that stumbled upon a bug, tried to fix it and needs a way to easily share it with colleagues.
