# R Project for Train Batteries Analysis

## Description

This project contains a series of R scripts for analyzing groups, managing power discharges, and performing statistical analysis on the provided dataset. The scripts are designed to work with the dataset `df_C2_carica`.

## Project Evolution

- ### Initial Phase:

The project began with a focus on identifying key groups within a dataset that exhibit certain desirable characteristics.

- ### Intermediate Phase:

Following the identification of good groups, the focus shifted to managing the power discharges in these groups.

- ### Advanced Phase:

The final phase involved performing detailed statistical analyses to understand the overall data trends and group dynamics.

## File Description

### **Description of the 1_data_cleaning.R Script**

This script is designed to process and analyze a dataset to identify "good" groups based on specific criteria. Here's a general overview of the script's workflow:

- **Data Import**: It reads in the primary dataset and an auxiliary dataset containing ID values. These datasets are merged and cleaned to remove unnecessary columns.
- **Data Filtering**: The script filters the dataset to include only the records from the year 2022.
- **Data Transformation**: It performs several data transformations, such as renaming columns and converting timestamps to the appropriate format.
- **Consistency Check**: It checks for consistency across the newly added ID columns and filters out any rows where inconsistencies are found.
- **Group Identification**: For each battery channel, the script calculates the rate of change of the current and uses it to identify groups based on changes in the current's sign and other criteria.
- **Group Assignment**: The identified groups are labeled and assigned to the dataset, providing a structured way to analyze different segments of data.
- **Output**: The processed dataset, now with group labels, is ready for further analysis or visualization in subsequent steps.

*This script is essential for preparing the data, ensuring its consistency, and segmenting it into meaningful groups for detailed analysis in the subsequent scripts.*

### **Description of the 2_power_discharges_management.R Script**

This script focuses on managing power discharges within identified groups in the dataset. Here’s a general overview of what the script does:

- **Data Import**: It reads in a cleaned dataset and additional workspace data, ensuring all necessary data is available for processing.
- **Index and Time Calculation**: It adds columns for indices and seconds within each group, facilitating time-based analysis within each group.
- **Voltage Threshold Detection**: The script marks the first instance in each group where the voltage exceeds 25.5V. This is important for identifying significant events within each group.
- **Adding Date-related Columns**: It adds columns for the month and season based on timestamps, enabling seasonal analysis of the data.
- **Discharge Duration Calculation**: The script calculates the duration of discharges within each group and adds this information to the dataset.

*This script is crucial for managing and analyzing the power discharges within groups identified as significant, setting the stage for more detailed analysis and reporting in subsequent steps.*

### **Description of the 3_statistical_analysis.R Script**

This script is designed to perform various statistical analyses on the dataset related to battery groups. Here's a general overview of what the script does:

- **Library Loading**: The script starts by loading necessary R libraries for data manipulation, visualization, statistical analysis, and correlation plotting.
- **Data Import**: It reads in multiple datasets corresponding to different battery channels (C2, C4, C5, C7). These datasets are essential for comprehensive statistical analysis.
- **Data Transformation**: The script converts categorical variables into numerical ones to facilitate statistical operations. For example, seasons are encoded as numerical values, and POC (Point of Charge) identifiers are converted to numeric form.
- **Group Segmentation**: The script identifies and segments the data into different groups based on POC values, preparing the data for detailed group-specific analysis.
- **Correlation Analysis**: A correlation matrix is calculated for the numerical variables in the dataset, excluding certain columns that are not needed for the correlation analysis. The correlation matrix is then visualized using the corrplot library to identify relationships between variables.
- **Visualization**: Various plots and visualizations are created to illustrate the statistical properties and relationships within the data. These visualizations help in understanding the data distributions and correlations better.

*This script is essential for performing detailed statistical analysis and deriving insights from the dataset, setting the foundation for further data-driven decision-making.*
