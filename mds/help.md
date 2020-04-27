
### Input Data

This panel allows users to upload the data that will be analyzed in Covid-19 Explorer.  

Your data should be a **.XLSX** file (recommended) or a **.CSV** file.

The data **must** have two essential columns: An **ID column** and a **DATE column**. In addition, we can add the numerical and categorical variables that we want using the following nomenclature in the Excel file columns:

  - The column containing the ID must **start with** "id_".
  - The column containing the date must **contain** the word "date". (Only one column can contain the word "date". Otherwise the program will fail in some functionalities).
  - Factor columns (such as gender or patient outcome) must **start with** "f_".
  - Categorical columns (such as the unique identifier of the database) must **start with** "c_".
  - Numeric columns that will not be transformed (such as age) must **start with** "n_". These variables will NEVER be transformed.
  - Numerical columns able to be transformed (such as hematological variables or cytokines) must **start with** "tn_" (transformable numeric"). However, the variables in this block will not always be transformed, since we can change the settings in "Advanced settings" for some variables and make the variables "n_" and "tn_" equivalent in some cases.

In all the above cases, upper and lower case do not matter since the program will change everything to lowercase, also changing the blank spaces to "_".

User can also select the desired observations (rows) in the '**Processed Data**' panel by clicking over it to subset the data. Since the moment that user select one or more rows in "Processed Data" panel, ALL plots of Covid-19 Explorer will be focussed ONLY in those selected observations. To deselect all rows, click on "Process" again.

<font color="red">

Dear user,  

Unfortunately the current documentation could be incomplete. We are working on it.  

Sorry for the inconvenience.  

</font>