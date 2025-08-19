import pandas as pd
import numpy as np

# Set seed for reproducibility
np.random.seed(42)

# Number of samples
num_samples = 500

# Generate synthetic data
institution_type = np.random.choice(['Public', 'Private'], size=num_samples)
student_enrollment = np.random.randint(2000, 50000, size=num_samples)
faculty_salary = np.random.randint(50000, 150000, size=num_samples)
research_funding = np.round(np.random.uniform(1, 500, size=num_samples), 2)
graduation_rate = np.round(np.random.uniform(50, 100, size=num_samples), 2)
student_faculty_ratio = np.round(np.random.uniform(5, 30, size=num_samples), 2)
tuition_fees = np.where(institution_type == 'Private', 
                        np.random.randint(20000, 60000, size=num_samples), 
                        np.random.randint(5000, 20000, size=num_samples))
employment_rate = np.round(np.random.uniform(60, 100, size=num_samples), 2)

# Generate a target variable: University Ranking Score (simplified regression target)
university_ranking_score = (0.3 * graduation_rate + 
                            0.2 * employment_rate + 
                            0.2 * (100 - student_faculty_ratio) + 
                            0.15 * (research_funding / 5) + 
                            0.15 * (faculty_salary / 1000)) 

# Create DataFrame
df = pd.DataFrame({
    "Institution Type": institution_type,
    "Student Enrollment": student_enrollment,
    "Faculty Salary (Avg.)": faculty_salary,
    "Research Funding (Million USD)": research_funding,
    "Graduation Rate (%)": graduation_rate,
    "Student-Faculty Ratio": student_faculty_ratio,
    "Tuition Fees (USD)": tuition_fees,
    "Employment Rate (%)": employment_rate,
    "University Ranking Score": np.round(university_ranking_score, 2)
})

# Save as CSV
csv_filename = "C:\\Users\\Public\\higher_education_regression_dataset.csv"
df.to_csv(csv_filename, index=False)

# Display first few rows
print(df.head())

