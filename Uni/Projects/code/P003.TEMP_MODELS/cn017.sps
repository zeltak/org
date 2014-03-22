DATASET ACTIVATE DataSet2.
RECODE lu_orig (22 thru 24=1) (ELSE=0) INTO lu2.
EXECUTE.



COMPUTE lu3=lu2 * Count.
EXECUTE.


DATASET ACTIVATE DataSet2.
DATASET DECLARE summed.
AGGREGATE
  /OUTFILE='summed'
  /BREAK=grid_raste
  /Count_sum=SUM(Count) 
  /lu3_sum=SUM(lu3).

DATASET ACTIVATE summed.
COMPUTE per_urban=lu3_sum / Count_sum.
EXECUTE.
