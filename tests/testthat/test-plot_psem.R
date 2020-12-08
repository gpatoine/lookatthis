library(testthat)

test_that("output is a graph", {

  data <- decomposition_data

  pmod <- psem(
    lm(decomposition ~ CN_ratio_litter + earthworm_abundance + detritivore_abundance, data),
    lm(earthworm_abundance ~ CN_ratio_litter + functional_diversity + LAI_canopy , data),
    lm(detritivore_abundance ~ CN_ratio_litter + functional_diversity + herb_cover + LAI_canopy, data),
    lm(LAI_canopy ~ functional_diversity, data),
    lm(herb_cover ~ LAI_canopy, data)
  )

  x <- plot_psem(pmod, render = FALSE)
  checkmate::expect_class(x, "dgr_graph")
})
