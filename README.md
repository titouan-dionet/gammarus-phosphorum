# Gammarus Phosphorum: Testing the Growth Rate Hypothesis at the Population Level

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This repository contains the code and data for analyzing phosphorus stoichiometry and population dynamics in *Gammarus fossarum*, a freshwater amphipod. The project investigates whether the Growth Rate Hypothesis (GRH) applies at the population level by combining matrix population models with elemental stoichiometry.

## Repository Structure

```
gammarus-phosphorum/
├── analyses/              # Pipeline and analysis code
│   └── pipeline/
│       └── _targets.R     # Pipeline definition using the targets package
│
├── data/                  # Input data
│   ├── raw_data/          # Raw measurement data
│   │   ├── monthly_surv_rates_coulaud_2014.csv
│   │   ├── phosphorus_measurements_2023_07.csv
│   │   └── P_conc_range_2023_07.csv
│   └── README.md
│
├── R/                     # Source code for the project
│   ├── data_processing.R  # Data processing functions
│   ├── figures.R          # Figure creation functions
│   ├── gammarus-phosphorum-package.R
│   ├── population_dynamics.R  # Matrix population model functions
│   ├── simulation.R       # Simulation utility functions
│   ├── statistics.R       # Statistical functions
│   ├── stoichiometry.R    # Stoichiometric analysis functions
│   └── visualization.R    # Graphical utilities and theme settings
│
├── outputs/               # Generated output files (figures, results)
│   ├── figures/           # Generated figures
│   ├── pipeline/          # Pipeline visualization files
│   ├── simulation_results/ # Simulation result data
│   └── _targets/          # Cache data for the targets pipeline
│
├── fonts/                 # Custom fonts for figures
│   └── roboto/            # Roboto font files
│
├── tests/                 # Unit tests for R functions
│   ├── testthat/          # Test files
│   └── testthat.R         # Test runner
│
├── DESCRIPTION            # Package description and dependencies
├── LICENSE.md             # GPL-3 license
├── CODE_OF_CONDUCT.md     # Contributor code of conduct
├── NAMESPACE              # Package namespace definitions
├── make.R                 # Main script to execute the pipeline
└── README.md              # This file
```

## Core Functionality

### Population Dynamics Model

The project uses a Lefkovitch matrix population model based on five size classes of *Gammarus fossarum*:

- Two juvenile classes: J1 (<3.5 mm) and J2 (3.5-5.2 mm)
- Three adult classes: A1 (5.2-6 mm), A2 (6-7 mm), and A3 (>7 mm)

The model incorporates:
- Size-dependent growth rates influenced by temperature
- Size-dependent reproduction rates
- Size-dependent survival rates
- Phosphorus content specific to each size class

Key functions in `population_dynamics.R`:
- `growth_rates_matrix()`: Calculates transition rates between size classes
- `fecondity_rates_matrix()`: Determines reproduction rates for adult classes
- `survival_rates_matrix()`: Creates survival rate matrix
- `Leslie_matrix()`: Combines the above matrices into a complete population matrix
- `find_lambda_SSD()`: Calculates asymptotic growth rate and stable stage distribution
- `elasticity_matrix()`: Performs elasticity analysis on the Leslie matrix

### Stoichiometry Integration

The project integrates elemental stoichiometry (particularly phosphorus) with population dynamics by:

1. Measuring phosphorus content in different size classes
2. Creating a stoichiometry matrix for each size class
3. Calculating population-level phosphorus content based on stable stage distribution

Key functions in `stoichiometry.R`:
- `elem_rates()`: Calculates elemental composition at individual and population levels

### Simulation Framework

The project includes multiple simulation approaches in `simulation.R`:

1. **Single-parameter simulations**: Vary one survival rate while keeping others constant
2. **Multi-parameter simulations**: Randomly vary all survival rates simultaneously
3. **Elasticity analysis**: Examine how small changes in survival affect population growth and phosphorus content
4. **Monthly variation**: Incorporate seasonal changes in survival rates

## Figures and Results

The pipeline generates four main figures:

1. **Figure 1**: Phosphorus content across different size classes (individual level)
2. **Figure 2**: Sensitivity analysis showing the effects of survival rates on population growth and phosphorus content
3. **Figure 3**: Effects of J1 and A3 survival on the relationship between growth rate and phosphorus
4. **Figure 4**: Full parameter space exploration of growth rate and phosphorus relationship

## Getting Started

### Prerequisites

This project uses R with several packages. You can restore the required packages using:

```r
# Install renv if not already installed
install.packages("renv")

# Restore dependencies
renv::restore()
```

### Running the Analysis

The analysis pipeline is managed using the `targets` package, which ensures reproducibility and efficient computation. To execute the complete pipeline:

```r
source("make.R")
```

This will:
1. Restore dependencies using `renv`
2. Configure the targets pipeline
3. Generate a visualization of the planned pipeline
4. Execute all analysis steps
5. Create output figures and data files

## Development

### Adding New Functions

1. Add new functions to the appropriate R file in the `R/` directory
2. Document functions using roxygen2 format
3. Run `devtools::document()` to generate documentation
4. Add tests in the `tests/testthat/` directory

### Testing

Run the tests using:

```r
devtools::test()
```

## Author

**Titouan Dionet** - [ORCID: 0009-0006-5602-1873](https://orcid.org/0009-0006-5602-1873)  
Email: titouan.dionet@univ-lorraine.fr

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE.md](LICENSE.md) file for details.

## Code of Conduct

Please note that the Gammarus Phosphorum project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.