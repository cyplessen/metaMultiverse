# metaMultiverse <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
![Version](https://img.shields.io/badge/version-0.2.0-blue.svg)
![R](https://img.shields.io/badge/R-%E2%89%A53.5-blue)
![License](https://img.shields.io/badge/license-MIT-green.svg)
![Tests](https://img.shields.io/badge/tests-446%20passing-brightgreen)
<!-- badges: end -->

> **Principled Multiverse Meta-Analysis in R**

`metaMultiverse` is a comprehensive toolkit for conducting multiverse meta-analyses. Rather than making arbitrary choices about analytical decisions, systematically explore how different reasonable choices affect your conclusions.

---

## ✨ Features

- 🔬 **11+ Meta-Analytic Methods**: Fixed-effects, REML, Paule-Mandel, PET-PEESE, p-uniform*, UWLS, WAAP, Three-level, RVE, and more
- 🌳 **E/U/N Decision Framework**: Classify analytical decisions as Equivalent, Uncertain, or Non-equivalent
- 📊 **Beautiful Visualizations**: Specification curves and Vibration of Effects (VoE) plots
- 🔄 **Flexible Pipeline**: Clean, pipeable workflow using `%>%` or `|>`
- 📖 **Comprehensive Documentation**: Two detailed vignettes with fully executable examples
- ✅ **Well-Tested**: 446 passing tests with full integration coverage

---

## 📦 Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("cyplessen/metaMultiverse")
```

---

## 🚀 Quick Start

```r
library(metaMultiverse)
library(dplyr)

# Load example data
data("data_digDep")

# Run a complete multiverse analysis
results <- data_digDep %>%
  check_data_multiverse() %>%                    # 1. Validate data
  define_factors(                                 # 2. Define analytical choices
    Population = "wf_3|E",                        #    E = Equivalent options
    Guidance = "wf_2|U"                           #    U = Uncertain which is best
  ) %>%
  create_multiverse_specifications(               # 3. Generate all combinations
    ma_methods = c("fe", "reml", "pm"),
    dependencies = c("aggregate", "select_max")
  ) %>%
  run_multiverse_analysis()                       # 4. Run all analyses

# Visualize results
plot_spec_curve(results)
plot_voe(results)
```

**Output:**
```
✅ Factor setup complete
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📊 Population (simple)
   Column: wf_3 | Decision: E (Equivalent)
   Levels: adult, mixed, young (+ all combined)

Running 36 specifications...
✅ 36/36 successful (100%)
```

---

## 🎯 The E/U/N Framework

Classify your analytical decisions to create **principled multiverses**:

| Type | Description | Example | Multiverse Impact |
|------|-------------|---------|-------------------|
| **E** (Equivalent) | Options are theoretically interchangeable | Population subgroups | Creates variations **within** multiverse |
| **U** (Uncertain) | Unclear which option is best | Outlier handling methods | Creates variations **within** multiverse |
| **N** (Non-equivalent) | Different research questions | Post-treatment vs. follow-up | Creates **separate** multiverses |

**Why this matters:** Type N factors create separate analyses rather than combining apples and oranges.

---

## 📊 Available Methods

### Standard Meta-Analysis
- `fe` - Fixed-effects
- `reml` - Restricted Maximum Likelihood
- `pm` - Paule-Mandel
- `hk` - Hartung-Knapp

### Publication Bias Correction
- `pet-peese` - PET-PEESE
- `pet-peese-corrected` - Corrected PET-PEESE (no negative estimates)
- `p-uniform` - p-uniform*
- `uwls` - Unrestricted Weighted Least Squares
- `waap` - Weighted Average of Adequately Powered studies

### Dependency Modeling
- `three-level` - Three-level meta-analysis
- `rve` - Robust Variance Estimation

### Bayesian
- `bayesmeta` - Bayesian meta-analysis (slow, optional)

---

## 📚 Documentation

### Vignettes

Get started with our comprehensive guides:

```r
# Quick introduction (recommended for beginners)
vignette("getting-started", package = "metaMultiverse")

# In-depth guide with theory and practice
vignette("multiverse-theory-practice-IMPROVED", package = "metaMultiverse")
```

### Key Topics Covered

- ✅ Complete pipeline walkthrough
- ✅ E/U/N decision type framework
- ✅ Custom factor groupings
- ✅ Multiple meta-analytic methods
- ✅ Dependency handling strategies
- ✅ Result interpretation and reporting
- ✅ Visualization examples (static + interactive)
- ✅ Troubleshooting common issues

---

## 🎨 Visualizations

### Specification Curve

Shows all effect sizes sorted by magnitude with specification details:

```r
plot_spec_curve(results, interactive = FALSE)
```

### Vibration of Effects (VoE)

Explores the relationship between effect size and p-value:

```r
plot_voe(results, interactive = FALSE)
```

---

## 💡 Example: Custom Groupings

Sometimes you want to test specific groupings rather than all individual levels:

```r
results <- data_digDep %>%
  check_data_multiverse() %>%
  define_factors(
    Population = "wf_3|E",

    # Custom groupings using list syntax
    Guidance = list(
      column = "wf_2",
      decision = "U",
      groups = list(
        guided = "guided",
        minimal = c("minimal to no support", "automated encouragement"),
        human_support = "human encouragement"
      )
    )
  ) %>%
  create_multiverse_specifications(
    ma_methods = c("fe", "reml"),
    dependencies = "aggregate"
  ) %>%
  run_multiverse_analysis()
```

---

## 📖 Citation

If you use `metaMultiverse` in your research, please cite:

```bibtex
@phdthesis{plessen2024multiverse,
  title={Exploring Robustness in Meta-Analytic Research: A Multiverse Approach},
  author={Plessen, Constantin Yves},
  year={2024},
  school={Vrije Universiteit Amsterdam}
}
```

**Related Publication:**
> Plessen, C. Y., Panagiotopoulou, O. M., Tong, L., Ciharova, M., & Cuijpers, P. (2024).
> Digital mental health interventions for the treatment of depression: A multiverse meta-analysis.
> *Journal of Affective Disorders, 369*, 1031-1044.
> [doi:10.1016/j.jad.2024.10.018](https://doi.org/10.1016/j.jad.2024.10.018)

---

## 🎓 Learn More

### Key References

**Multiverse Analysis:**
- Del Giudice, M., & Gangestad, S. W. (2021). A traveler's guide to the multiverse. *Advances in Methods and Practices in Psychological Science, 4*(1), 1-15.
- Steegen, S., et al. (2016). Increasing transparency through a multiverse analysis. *Perspectives on Psychological Science, 11*(5), 702-712.

**Specification Curve Analysis:**
- Simonsohn, U., Simmons, J. P., & Nelson, L. D. (2020). Specification curve analysis. *Nature Human Behaviour, 4*(11), 1208-1214.

**Multiverse Meta-Analysis:**
- Voracek, M., Kossmeier, M., & Tran, U. S. (2019). Which data to meta-analyze, and how? *Zeitschrift für Psychologie, 227*(1), 64-82.

---

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

### Development

```r
# Clone the repository
git clone https://github.com/cyplessen/metaMultiverse.git

# Install development dependencies
devtools::install_dev_deps()

# Run tests
devtools::test()

# Build documentation
devtools::document()

# Check package
devtools::check()
```

---

## 📝 License

MIT License - see [LICENSE](LICENSE) file for details.

---

## 🙏 Acknowledgments

- Built with ❤️ by [Constantin Yves Plessen](https://constantinyvesplessen.com)
- Developed as part of PhD research at Vrije Universiteit Amsterdam
- Thanks to all contributors and users providing feedback

---

## 📬 Contact

- **Author:** Constantin Yves Plessen
- **Website:** [constantinyvesplessen.com](https://constantinyvesplessen.com)
- **GitHub:** [@cyplessen](https://github.com/cyplessen)

---

## ⭐ Star History

If you find this package useful, please consider giving it a star on GitHub!

[![Star History](https://api.star-history.com/svg?repos=cyplessen/metaMultiverse&type=Date)](https://star-history.com/#cyplessen/metaMultiverse&Date)

---

<div align="center">

**[Documentation](https://github.com/cyplessen/metaMultiverse)** •
**[Issues](https://github.com/cyplessen/metaMultiverse/issues)** •
**[Changelog](NEWS.md)**

Made with 🔬 for transparent meta-analysis

</div>
