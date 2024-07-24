# Handeling visual distraction
This project is the repository for all data and scripts used for the manuscript entitled: "***Distraction vs. Interference: Handling fully irrelevant vs. potentially relevant distractors***".

### Project Structure ###

The project is organized into the following main sections:

- **Input**: Where the raw data are stored.
- **Output**: Where all the formatted data from **load_data.R** and the plots generated from **plots.R** within the **Scripts** folder.
- **Scripts**: Includes scripts used to load and preprocess data, and execute the analysis.

##### Scripts Folder Structure #####

The **scripts** folder is straightforward:

- *analysis_script.R*: The script used in the **Result** for every experiment in the manuscript.
- *plots.R*: In this script is the code for all the results figures presented in the manuscript. 

All the above .R files implicitly call `source(...)` to run *load_data.R* and *functions.R*. In the former, data are loaded and prepossessed; in the latter, helper functions are loaded into the workspace.
