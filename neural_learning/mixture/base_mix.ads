--  Based on scikit-learn/sklearn/mixture/_base.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Base_Mix is

    type Probability_Type is (Bayesian_Prob, Gaussian_Prob);

   procedure Estimate_Log_Prob_Resp (X : Float_Matrix;
                                     Log_Prob_Norm : out Float_Array;
                                     Log_Responsibil : out Float_Matrix);
    function Estimate_Weighted_Log_Prob (X : Float_Matrix) return Float_Matrix;
    function Score (X : Float_Matrix) return Float;
    function Score_Samples (X : Float_Matrix) return Float_Array;

end Base_Mix;
