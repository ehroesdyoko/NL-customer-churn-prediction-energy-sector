# Customer Churn Prediction for a Dutch Energy Supplier

This project was conducted as part of the *Data Science Methods* course at the University of Groningen. We developed and compared multiple machine learning models to predict customer churn using behavioral, demographic, and contractual data.

---

## 📌 Project Description

- **Dataset:** 20,000 customer records from a Dutch energy supplier.
- **Goal:** Predict churn and explore which factors most influence customer retention.
- **Best Model:** Boosting (highest predictive performance).

---

## 📂 Structure

- `data/`: Contains the CSV file with the final predictions.
- `docs/`: Includes the full assignment report.
- `scripts/`: Contains the main R script for churn modeling.
- `README.md`: This project overview.

---

## ⚙️ Models Tested

- Baseline Logistic Regression  
- Stepwise Logistic Regression  
- Classification and Regression Trees (CART)  
- Boosting (Best)  
- Random Forest  
- Support Vector Machines (SVM)

---

## 📈 Performance

| Model                | TDL    | Hit - Rate Accuracy |
|----------------------|--------|---------------------|
| Step Wise Regression | 1.695  | 67.42%              |
| Boosting (Best)      | 1.890  | 75.38%              |
| Random Forest        | 1.875  | 75.04%              |
| SVM                  | 1.408  | 68.55%              |

*Exact metrics can be found in the report.*

---

## 🧠 Key Takeaways

- Boosting provided the best generalization on the test set.
- Stepwise regression offered good interpretability.
- Important features include [you can list from the report].

---

## 👥 Authors

Group 3 - University of Groningen  
Data Science Methods, MSc Marketing Analytics and Data Science

---

## 📄 License

Academic use only.
