
with NL_Types; use NL_Types;

package Neural_Maths is

   function Digamma (Z : Float) return Float;
   function Log_Sum_Exponent (Log_Prob : Float_List_2D) return Float_List;
   function Mean (A : Integer_List_2D) return Float;
   function Mean (A : Float_List_2D) return Float;
   function Mean (A : Float_List) return Float;
   function Mean (A : Float_List_2D; Axis : Positive) return Float_List;

end Neural_Maths;