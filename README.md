1. Download the dataset from here (https://www.cs.cmu.edu/~./enron/) and extract it in the main project folder as 'maildir'.
2. Run extract.R (specify the number of emails you want from the entire dataset. About 20,000 taken in this implementation.
3. extract.R creates a file called Unique2.xlsx.
4. Run Doc2vec_Clustering.ipynb on Jupyter Notebooks. This file reads the dataset extracted above, trains adoc2vec model and uses the features extracted to cluster the emails using KMeans and DBScan. 
5. Doc2vec creates file called Lables which has cluster labels for each email in the dataset.
6. Now, run unique_analysis.R to analyse each cluster and create bar charts and word clouds for each cluster
