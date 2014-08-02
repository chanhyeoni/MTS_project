The Mahalanobis-Taguchi System Application
=================================================================================

The Mahalanobis-Taguchi System (MTS)

The Mahalanobis-Taguchi System (MTS) is a statistical and forecasting method that
classifies data into normal and abnormal groups. Given a multi-dimensional dataset, the
MTS first estimates the Mahalanobis distances between the scaled normal data and the
mean of the normal data, as well as the distances between the abnormal and the normal
mean. Then, the Taguchi technique is employed to calculate the average signal-to-noise
ratio of each variable or feature and to carry out feature selections. Next, the
Mahalanobis distance is estimated again, but using the selected features. The final
analysis delivers a more interpretable result explaining the ‘important’ features that
contribute to the classification. In this research, the MTS was applied to the Fire
Peril Loss Cost dataset, which was provided by the Liberty Mutual Group Inc and included
approximately 300 features. This project will demonstrate how the MTS system would make
a clear separation between the group of the clients with low risk ratio (the normal
group) and the group with high risk (the abnormal group). The MTS analysis was
implemented with the statistical programming language called R, and the resulting script
is applicable to a wide variety of pre-processed datasets. Future plans include adding
more functions in the MTS.R module in order for the project to be applicable to a wide
variety of datasets, and creating a web-based application using the model.

