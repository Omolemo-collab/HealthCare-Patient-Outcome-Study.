-- Look at first few rows from patients
SELECT * 
FROM patients
LIMIT 10;

-- Same for treatments
SELECT * 
FROM treatments
LIMIT 10;

-- Same for outcomes
SELECT * 
FROM outcomes
LIMIT 10;

UPDATE patients
SET gender = CASE
    WHEN gender IN ('M', 'Male', 'male', 'm') THEN 'Male'
    WHEN gender IN ('F', 'Female', 'female', 'f') THEN 'Female'
    ELSE 'Unknown'
END;

UPDATE treatments
SET treatment_type = COALESCE(treatment_type, 'Unknown');

UPDATE patients
SET department = COALESCE(department, 'Unknown');

DELETE FROM patients
WHERE age < 0 OR age > 120;

DELETE FROM outcomes
WHERE event_status NOT IN (0, 1);

CREATE TABLE survival_cleaned AS
SELECT 
    p.patient_id,
    p.age,
    p.gender,
    p.department,
    t.treatment_type,
    o.survival_time,
    o.event_status,
    o.event_date,
    p.admission_date
FROM patients p
LEFT JOIN treatments t ON p.patient_id = t.patient_id
LEFT JOIN outcomes o ON p.patient_id = o.patient_id
WHERE p.admission_date IS NOT NULL
  AND o.event_status IS NOT NULL;
