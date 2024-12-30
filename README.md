### <center><b>Integrating Near-Infrared Hyperspectral Imaging with Machine Learning and Feature Selection: Detecting Adulteration of Extra-Virgin Olive Oil with Lower-Grade Olive Oils and Hazelnut Oil</center> </b>

### Project Overview

Detecting adulteration in extra-virgin olive oil (EVOO) is challenging, particularly when adulterants have similar chemical compositions. This project applies near-infrared hyperspectral imaging (NIR-HSI) and machine learning (ML) techniques to detect EVOO adulteration with hazelnut oil, refined olive oil, and olive pomace oil across a range of concentrations (1%, 5%, 10%, 20%, 40%, and 100% m/m).

### Methodology

The study leverages advanced spectral data preprocessing and machine learning methods to achieve high accuracy in identifying adulterated EVOO samples. The workflow includes:

#### Spectral Data Preprocessing: Various techniques, including:

- Savitzky-Golay filtering
- First and second derivatives
- Multiplicative scatter correction (MSC)
- Standard normal variate (SNV)
- Combinations of these techniques for optimal preprocessing.
- Dimensionality Reduction: Principal Component Analysis (PCA) reduces data dimensionality, facilitating model training.

#### Classification Models: Several ML models were used, including:

- Partial Least Squares-Discriminant Analysis (PLS-DA)
- k-Nearest Neighbors (k-NN)
- Naïve Bayes (NB)
- Random Forest (RF)
- Support Vector Machine (SVM)
- Artificial Neural Networks (ANN)

### Key Results

Model Performance: The models achieved impressive accuracy rates in classifying oils as pure EVOO, oil adulterants or adulterated olive oil:

- PLS-DA: 97.0-99.0%
- k-NN: 96.2-100%
- RF: 96.5-100%
- SVM: 98.6-99.5%
- NB: 93.9-99.7%
- ANN: 99.2-100%

- `Binary Classification`: PLS-DA, RF, SVM, and ANN outperformed Naïve Bayes (p < 0.05) in distinguishing pure EVOO from adulterated oils, with Matthews correlation coefficient (MCC) values > 0.90.

- `Optimal Preprocessing`: Most classifiers, combined with SNV/MSC, Savitzky-Golay smoothing, and derivatives, achieved perfect accuracy, sensitivity, specificity, F1 score, precision, and MCC in binary classification tasks.

- `Variable Selection`: There were no significant differences in model performance between full spectra and selected key variables, though PLS-DA and ANN achieved higher performance (p < 0.05).

### Conclusion

Combining NIR-HSI with machine learning offers an effective, non-destructive solution for detecting EVOO adulteration, potentially aiding in fraud prevention within the olive oil industry.
The link to the publication: https://www.sciencedirect.com/science/article/pii/S2665927124002399 


```python

```
