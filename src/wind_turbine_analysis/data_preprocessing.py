"""
Data preprocessing module for wind turbine analysis.
"""

import pandas as pd
import numpy as np
from typing import Tuple, Dict
import logging

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def load_data(file_path: str) -> Tuple[pd.DataFrame, pd.DataFrame]:
    """
    Load data from Excel file containing wind turbine information.
    
    Args:
        file_path (str): Path to the Excel file
        
    Returns:
        Tuple[pd.DataFrame, pd.DataFrame]: Database and Turbine data sheets
    """
    try:
        database = pd.read_excel(file_path, sheet_name='DATABASE')
        turbine_data = pd.read_excel(file_path, sheet_name='Turbine Dat')
        logger.info(f"Successfully loaded data from {file_path}")
        return database, turbine_data
    except Exception as e:
        logger.error(f"Error loading data: {str(e)}")
        raise

def analyze_missing_values(df: pd.DataFrame) -> pd.DataFrame:
    """
    Analyze missing values in the dataset.
    
    Args:
        df (pd.DataFrame): Input dataframe
        
    Returns:
        pd.DataFrame: Missing values analysis
    """
    missing_values = df.isnull().sum()
    missing_values_percent = 100 * missing_values / len(df)
    
    missing_values_table = pd.concat([missing_values, missing_values_percent], axis=1)
    missing_values_table.columns = ['Missing Values', '% of Total Values']
    
    return missing_values_table.sort_values('% of Total Values', ascending=False)

def clean_data(database: pd.DataFrame, turbine_data: pd.DataFrame) -> pd.DataFrame:
    """
    Clean and prepare the data for analysis.
    
    Args:
        database (pd.DataFrame): Main database
        turbine_data (pd.DataFrame): Turbine information
        
    Returns:
        pd.DataFrame: Cleaned and merged dataset
    """
    try:
        # Remove columns with too many missing values (e.g., > 90%)
        missing_analysis = analyze_missing_values(database)
        columns_to_drop = missing_analysis[missing_analysis['% of Total Values'] > 90].index
        database_cleaned = database.drop(columns=columns_to_drop)
        
        # Merge with turbine data
        merged_data = pd.merge(
            database_cleaned,
            turbine_data,
            on=['Park', 'Turbine'],
            how='left'
        )
        
        logger.info("Data cleaning completed successfully")
        return merged_data
    except Exception as e:
        logger.error(f"Error during data cleaning: {str(e)}")
        raise
