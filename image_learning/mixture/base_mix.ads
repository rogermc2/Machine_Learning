
with IL_Types; use IL_Types;
package Base_Mix is

   type Probability_Type is (Bayesian_Prob, Gaussian_Prob);

   function Score (X    : Float_List_2D;
                   Y    : Integer_List) return Float;
   function Score_Samples (X : Float_List_2D) return Float;

end Base_Mix;
