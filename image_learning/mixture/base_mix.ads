
with IL_Types; use IL_Types;
package Base_Mix is

    type Probability_Type is (Bayesian_Prob, Gaussian_Prob);

   procedure Estimate_Log_Prob_Resp (X : Float_List_2D;
                                     Log_Prob_Norm : out Float_List;
                                     Log_Responsibil : out Float_List_2D);
    function Estimate_Weighted_Log_Prob (X : Float_List_2D) return Float_List_2D;
    function Score (X    : Float_List_2D;
                    Y    : Integer_List) return Float;
    function Score_Samples (X : Float_List_2D) return Float_List;

end Base_Mix;
