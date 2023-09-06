RSCRIPT = Rscript
SLIDES = slides.pdf
SLIDES_THEME = scripts/common/setup-ggplot-theme.R

ALL_PLOTS = \

TEX_FILES = $(wildcard tex-input/*.tex) \
	$(wildcard tex-input/*/*.tex) \
	$(wildcard tex-input/*/*/*.tex)

all : $(SLIDES)

# ============================== PB model figures ==============================
PB_POP_PLOT = figures/pb-model/pop-target-marginal.pdf
PB_POP_PLOT_EXTRAS = figures/pb-model/pop-target-marginal-with-samples.pdf
PB_COV_PLOT = figures/pb-model/cov-target-prior.pdf

$(PB_POP_PLOT) \
$(PB_POP_PLOT_EXTRAS) &: \
	scripts/pb-model/plot-population-prior.R \
	$(SLIDES_THEME)
	$(RSCRIPT) $<

$(PB_COV_PLOT) : scripts/pb-model/plot-covariate-prior.R $(SLIDES_THEME)
	$(RSCRIPT) $<

ALL_PLOTS += $(PB_POP_PLOT) $(PB_COV_PLOT) $(PB_POP_PLOT_EXTRAS)

$(SLIDES) : slides.rmd $(ALL_PLOTS) $(TEX_FILES)
	Rscript -e "rmarkdown::render(input = 'slides.rmd', encoding = 'UTF-8')"
