
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Neural_Maths is

   function Digamma (Z : Float) return Float;
   function Mean (A : Integer_Matrix) return Float;
   function Mean (A : Real_Float_Matrix; Axis : Positive)
                   return Real_Float_Vector;
   function Mean (A : Real_Float_Matrix) return Real_Float_Vector;
   function Mean (A : Real_Float_Matrix) return Float;
   function Mean (A : Real_Float_Vector) return Float;
   function Sigmoid (Val : Float) return Float;
   function Sigmoid (V : Real_Float_Matrix)
                     return Real_Float_Matrix;
   function Sigmoid_Deriv (Val : Float) return Float;
   function Sigmoid_Deriv (V : Real_Float_Matrix)
                           return Real_Float_Matrix;

end Neural_Maths;
