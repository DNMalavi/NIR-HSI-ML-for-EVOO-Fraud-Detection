# **Integrating Near-Infrared Hyperspectral Imaging with Machine Learning and Feature Selection for Detecting Adulteration of Extra-Virgin Olive Oil**

## **Project Overview**

Detecting adulteration in extra-virgin olive oil (EVOO) is particularly challenging when adulterants share similar chemical compositions. This project integrates **near-infrared hyperspectral imaging (NIR-HSI)** with **machine learning (ML)** and **feature selection** to detect adulteration involving **hazelnut oil, refined olive oil, and olive pomace oil** across multiple concentration levels (1%, 5%, 10%, 20%, 40%, and 100% m/m).

This repository contains reproducible workflows covering **spectral preprocessing, dimensionality reduction, feature selection, model training, evaluation, and deployment** through the Shiny application *OleaSense*.

---

## **Workflow Overview**

### **1. Spectral Data Preprocessing**

Multiple preprocessing pipelines were evaluated to handle noise, scattering, and baseline shifts:

- Standard Normal Variate (SNV)  
- Multiplicative Scatter Correction (MSC)  
- Savitzky–Golay smoothing  
- First and second derivatives  
- Combined SNV/MSC + SG derivatives  
- Mean centering and scaling  

These steps enhance signal quality and stabilize downstream models.

---

### **2. Dimensionality Reduction**

- **Principal Component Analysis (PCA)** was used to reduce collinearity, compress spectral information, and support interpretable visualization.

---

### **3. Classification Models**

Several ML algorithms were optimized and evaluated for classifying pure versus adulterated EVOO:

- Partial Least Squares–Discriminant Analysis (PLS-DA)  
- k-Nearest Neighbors (k-NN)  
- Naïve Bayes (NB)  
- Support Vector Machine (SVM, radial kernel)  
- Random Forest (RF)  
- Artificial Neural Networks (ANN)

Models were assessed using repeated cross-validation and external test sets.

---

## **Key Results**

### **Model Performance**

| Model | Accuracy Range |
|-------|----------------|
| PLS-DA | 97–99% |
| k-NN | 96.2–100% |
| Random Forest | 96.5–100% |
| SVM | 98.6–99.5% |
| Naïve Bayes | 93.9–99.7% |
| ANN | 99.2–100% |

### **Binary Classification**

- PLS-DA, RF, SVM, and ANN significantly outperformed NB (**p < 0.05**).  
- All high-performing models achieved **MCC > 0.90**, with perfect or near-perfect sensitivity, specificity, and F1 scores under optimized preprocessing.

### **Variable Selection**

Feature selection algorithms (Boruta, GA-RFE):

- Did **not** significantly reduce accuracy.  
- Full-spectrum models performed best overall.  
- Selected wavelengths provide insights for designing future multispectral sensors.

---

## **Conclusion**

NIR-HSI with machine learning provides a **rapid, non-destructive, and highly accurate** approach for detecting EVOO adulteration—even when adulterants are chemically similar.  
This workflow offers a practical pathway toward real-world fraud detection and quality assurance.

📄 **Published Article:**  
https://www.sciencedirect.com/science/article/pii/S2665927124002399

---

## **Repository Structure**
/data/ Raw and preprocessed spectral datasets
/scripts/ R scripts for preprocessing, PCA, feature selection, ML models
/notebooks/ R Markdown reports with full analysis
/OleaSense_App/ Shiny application for model deployment
/models/ Saved trained models (.rds)
/docs/ Supporting figures and supplementary files
README.md

The github page link to the statistical analyis for model comparison can be found here: https://dnmalavi.github.io/NIR-HSI-ML-for-EVOO-Fraud-Detection/ 


---

## **Deployment Tool: OleaSense (Shiny App)**

A research-grade decision-support tool for EVOO authentication.

🔗 **Live application:**  
https://dmalavi.shinyapps.io/EVOO_Shiny_App/

📂 **Source code:**  
`/OleaSense_App/`

**App capabilities:**

- Upload spectral files  
- Apply preprocessing pipelines  
- Visualize PCA scores and loadings  
- Generate predictions from multiple models  
- Inspect variable importance and diagnostics  
- Download outputs for reporting  

---

## **Running the App Locally**

```r
# Install dependencies
install.packages(c("shiny", "tidyverse", "caret", "pls", "randomForest",
                   "readxl", "ggplot2", "e1071", "MASS"))

# Launch app
shiny::runApp("OleaSense_App")

