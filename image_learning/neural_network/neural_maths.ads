
with IL_Types; use  IL_Types;

package Neural_Maths is

   function Digamma (Z : Float) return Float;
   function Log_Sum_Exponent (Log_Prob : Float_List_2D) return Float_List;
   function Mean (A : Float_List) return Float;

end Neural_Maths;
