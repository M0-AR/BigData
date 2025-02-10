"""
Tests for wind turbine data preprocessing functionality.
"""

import unittest
import pandas as pd
import numpy as np
import sys
import os

# Add src to Python path
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from src.wind_turbine_analysis.data_preprocessing import (
    load_data,
    analyze_missing_values,
    clean_data
)

class TestWindTurbinePreprocessing(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        """Set up test data"""
        # Create sample data
        cls.sample_database = pd.DataFrame({
            'Park': ['Park1', 'Park1'],
            'Turbine': ['T1', 'T2'],
            'Component': ['Gearbox', 'Generator'],
            'Missing_Col': [np.nan, np.nan]
        })
        
        cls.sample_turbine_data = pd.DataFrame({
            'Park': ['Park1', 'Park1'],
            'Turbine': ['T1', 'T2'],
            'Type': ['Type1', 'Type1']
        })

    def test_analyze_missing_values(self):
        """Test missing values analysis"""
        missing_analysis = analyze_missing_values(self.sample_database)
        
        self.assertEqual(missing_analysis.loc['Missing_Col', 'Missing Values'], 2)
        self.assertEqual(missing_analysis.loc['Missing_Col', '% of Total Values'], 100.0)

    def test_clean_data(self):
        """Test data cleaning functionality"""
        cleaned_data = clean_data(self.sample_database, self.sample_turbine_data)
        
        # Check if highly missing columns are dropped
        self.assertNotIn('Missing_Col', cleaned_data.columns)
        
        # Check if merge was successful
        self.assertIn('Type', cleaned_data.columns)
        self.assertEqual(len(cleaned_data), 2)

if __name__ == '__main__':
    unittest.main()
