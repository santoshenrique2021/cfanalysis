# cfanalysis

An R package to analyze categorical features and their long-term stability.

## ⏬ Installation

Install the cfanalysis package via CRAN:

``` r
# NOT YET AVAILABLE VIA CRAN
```

You can also install the package via the Github repository.

``` r
install.packages("remotes")
remotes::install_github("santoshenrique2021/cfanalysis")
```

## 🔨 Contributing

You can learn more about package authoring with RStudio at:

http://r-pkgs.had.co.nz/

Some useful keyboard shortcuts for package authoring:

Install Package: 'Ctrl + Shift + B'

Check Package: 'Ctrl + Shift + E'

Test Package: 'Ctrl + Shift + T'

Document Functions: 'devtools::document()'

https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html
https://web.mit.edu/insong/www/pdf/rpackage_instructions.pdf

## Action List

- Do we need to record the dataset source?
- Come up with a license.
- Warnings:
  full_fr: no visible binding for global variable 'absolute_frequency'
  plot_bar: no visible global function definition for 'stack'
  plot_bar: no visible global function definition for 'ggplot'
  plot_bar: no visible global function definition for 'aes'
  plot_bar: no visible binding for global variable 'ind'
  plot_bar: no visible binding for global variable 'values'
  plot_bar: no visible global function definition for 'geom_bar'
  plot_bar: no visible global function definition for 'scale_fill_brewer'
  plot_bar: no visible global function definition for 'theme_classic'
  plot_bar: no visible global function definition for 'ylab'
  plot_bar: no visible global function definition for 'xlab'
  plot_bar: no visible global function definition for 'ggtitle'
  plot_bar: no visible global function definition for 'theme'
  plot_bar: no visible global function definition for 'element_text'
  plot_bar: no visible global function definition for 'element_blank'
  plot_bar: no visible global function definition for 'coord_flip'
  plot_bar: no visible global function definition for 'stat_count'
  plot_bar: no visible global function definition for 'after_stat'
  plot_bar: no visible binding for global variable 'count'
  plot_bar: no visible global function definition for 'position_fill'
  re_fr: no visible binding for global variable 'total'
  re_fr: no visible binding for global variable 'relative_frequency'
  tv_d: no visible binding for global variable 'dif_abs'
  Undefined global functions or variables:
    absolute_frequency aes after_stat coord_flip count dif_abs
    element_blank element_text geom_bar ggplot ggtitle ind position_fill
    relative_frequency scale_fill_brewer stack stat_count theme
    theme_classic total values xlab ylab
  Consider adding
    importFrom("utils", "stack")
  to your NAMESPACE file.