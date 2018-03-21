# If X ~ Exp(λ) and Y ~ Exp(ν) 
# 	then min(X, Y) ~ Exp(λ + ν).

# Rewrite for scale: 
#	λ = 1/α and ν = 1/β 
#	λ + ν = (β + α)/αβ
#	If X ~ Exp(α) and Y ~ Exp(β) 
# 	then min(X, Y) ~ Exp(αβ/(β + α)).

# Proof:

PDF_exponential := (alpha, x) -> exp(-x/alpha)/alpha;




evalb(PDF_kX(lambda, x, k) = PDF_exp_scaled(lambda, x, k));