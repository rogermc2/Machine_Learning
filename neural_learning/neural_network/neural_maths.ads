
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Neural_Maths is

   function Digamma (Z : Float) return Float;
   function Log_Sum_Exponent (Log_Prob : Float_Matrix) return Float_Array;
   function Mean (A : Integer_Matrix) return Float;
   function Mean (A : Float_Matrix) return Float;
   function Mean (A : Float_Array) return Float;
   function Mean (A : Float_Matrix; Axis : Positive) return Float_Array;

end Neural_Maths;
