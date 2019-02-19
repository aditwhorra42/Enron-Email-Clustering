In previous implementations of clustering the Enron Dataset, a traditional KMeans approach was taken on the TF-IDF vectors of emails. However, after thoroughly studying the dataset and understanding the limitations of the previous approach, this implementation uses a different approach and results produces much better results. After a thorough cleaning of the emails, the cleaned emails were trained on a doc2vec model and then they clustered using KMeans and DBScan. The results can be found in the project report. 


1. Download the dataset from here (https://www.cs.cmu.edu/~./enron/) and extract it in the main project folder as 'maildir'.
2. Run extract.R (specify the number of emails you want from the entire dataset. About 20,000 taken in this implementation.
3. extract.R creates a file called Unique2.xlsx.
4. Run Doc2vec_Clustering.ipynb on Jupyter Notebooks. This file reads the dataset extracted above, trains a doc2vec model and uses the features extracted to cluster the emails using KMeans and DBScan. 
5. Doc2vec creates file called Lables which has cluster labels for each email in the dataset.
6. Now, run unique_analysis.R to analyse each cluster and create bar charts and word clouds for each cluster
