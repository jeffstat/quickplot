**What is this package:** The `quick_plot()` function within the `quickplot` package is used to quickly make ggplot objects with just one line of code.

**Installation:** `devtools::install_github("jeffstat/quickplot")`

**Example:** `quickplot(x = factor(rbinom(100, 1, 0.4)), y = c(1:100), xlab = "Knows R Studio (0=no, 1=yes)", ylab = "Final Grade (%)")`

**Exercise 1.1 - How this package was made:**
1. The `quickplot` package was made using functions from the package `devtools`.
2. The `create_package()` function was used to create the package, saving it to a local directory.
3. The `use_git()` function was used to create a Git repository.
4. The `use_r()` function was used to create an R script for the `quick_plot` function.
5. The `check()` function was used to check that the function works correctly and the package passes without errors.
6. The `use_mit_license()` function was used to create a license.
7. The `roxygen2` package was used, so that Insert Roxygen Skeleton can be manually selected in RStudio to create R Documentation.
8. The `document()` function was used to create documentaiton for the package.
9. The `usethis::use_package()` function was used to list all package dependencies in the DESCRIPTION file.
10. The `use_vignette()` and `build_vignettes()` functions were used to create a vignette.
