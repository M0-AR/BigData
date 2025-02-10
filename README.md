# BigData Analytics Project

## Overview
This project consists of two main components:
1. Wind Turbine Predictive Maintenance Analysis
2. Online Shopping Intention Analysis

### Wind Turbine Analysis
Analyzes Vattenfall wind turbine data to predict component failures and maintenance needs. The dataset contains approximately 450 failure events of main components including gearboxes, transformers, generators, main bearings, and blades.

### Shopping Intention Analysis
Analyzes online shopping behavior to predict customer purchase intentions using various machine learning techniques including:
- Decision Trees
- Random Forests
- K-Nearest Neighbors
- Support Vector Machines
- Hierarchical Clustering

## Project Structure
```
BigData/
├── src/                    # Source code
├── data/                   # Data files
├── notebooks/              # Jupyter notebooks and R markdown files
├── tests/                  # Test files
├── docs/                   # Documentation
└── output/                 # Generated analysis output
```

## Requirements

### Python Dependencies
- pandas
- numpy
- scikit-learn
- matplotlib
- seaborn
- jupyter

### R Dependencies
- tidyverse
- caret
- rpart
- e1071
- ggplot2
- cluster

## Installation

1. Clone the repository:
```bash
git clone https://github.com/M0-AR/BigData.git
cd BigData
```

2. Set up Python environment:
```bash
# Create and activate virtual environment
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

3. Set up R environment:
```R
# Run setup.R script
Rscript setup.R
```

## Usage

### Wind Turbine Analysis
1. Navigate to the notebooks directory
2. Open `predictibe_maintenance_RUL.ipynb`
3. Follow the step-by-step analysis process

Key features:
- Data preprocessing and cleaning
- Feature engineering
- Predictive modeling
- Visualization of results

### Shopping Intention Analysis
1. Navigate to the notebooks directory
2. Run the R scripts in sequence:
   - `data_preprocessing.R`
   - `modeling.R`
   - `visualization.R`

Key features:
- Customer behavior analysis
- Purchase prediction
- Cluster analysis
- Performance visualization

## Results
- Detailed analysis results can be found in `Big_Data_final_report.pdf`
- Visualizations are stored in the `output/figures` directory

## Contributing
1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Authors
- M0-AR

## Acknowledgments
- Vattenfall for providing the wind turbine dataset
- Online Shoppers Intention Dataset contributors
