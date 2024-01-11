# OmniLog(R) Phenotype Microarray

[![SVN](https://github.com/TumbleOwlee/opm/actions/workflows/svn.yml/badge.svg?branch=main)](https://github.com/TumbleOwlee/opm/actions/workflows/svn.yml)

This repository is a fork of the R package `opm` by Markus Göker (see [website](http://www.goeker.org/opm/)). I forked the project to test some changes and to easily share it with colleagues.

Currently this repsoitory applies the following fixes:
* Allow the use of a custom color list in plots. The user can use the `col` parameter to either use the default (`col = ""`), use a single color for all curves (e.g. `col = "#5a2474"`), use a predefined color set (e.g. `col = "w3c"`) or specify a custom color vector (e.g. `col = c("#111111", "#222222", "#333333")`). Previously it would fail with a color vector because it performed the check `!nzchar(col)` on all cases.

## Quickstart

If you want to use these fixes, make sure you have followed the installation instruction of the OPM suite (see [Installation](http://www.goeker.org/opm/)). This will of course also install the original `opm` package. Afterwards you can simply override it with this version using the following command.
```r
remotes::install_github("tumbleowlee/opm")
```

# FAQ

**Will this repository also contain all future changes applied to the SVN reposity by the original author?**

Yes, at least I hope so. This repository utilizes a Github workflow that will clone the SVN repository by the original author and rebase this repository to apply my changes. Afterwards the rebased history will be forcefully pushed. This is scheduled for 5:30am on Monday of every week.

# Disclaimer

I am neither the creator or maintainer of the R package `opm`. I am just a user of it that stumbled upon a bug, tried to fix it and needs a way to easily share it with colleagues.
