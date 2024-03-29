# AdlerMDS_MinimalDifference

This is a RShiny web app for the estimation of the Minimal Difference (MD) at a given cut-off.

The algorithms used were published in the following [paper](https://link.springer.com/article/10.1007/s13300-019-00740-w).

Here is the link to the "ready-to-use" [web app](https://adlermds.shinyapps.io/AdlerMDS_MinimalDifference/).

This WebApp is published in [this](https://www.degruyter.com/document/doi/10.1515/labmed-2023-0020/html) article.

You can run this WebApp on your local machine in a R Session using the following code:

```bash
library(shiny)
runGitHub("2021_AdlerMDS_MinimalDifference", "Bussard91", ref = "main")
```

Notice that the package "shiny" has to be installed.
