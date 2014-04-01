To test the significance of a difference between 2 coefficients (b2-b1), compute 

s3=sqrt(s1^2 + s2^2) where s1 is the se of b1, s2 the se of b2
and s3 is the se of (b2-b1) that should be a t statistic and you can look up the p value from that ( 2*(1-probt((b2-b1)/s3)))

#long term
b1=0.00096339
#short term
b2=0.0003317
s3=sqrt(b1^2)+sqrt(b2^2)

t=(b2-b1)/s3 





