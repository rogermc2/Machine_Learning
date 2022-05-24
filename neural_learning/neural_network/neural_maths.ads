
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Neural_Maths is

    function Digamma (Z : Float) return Float;
    function Mean (A : Integer_Matrix) return Float;
    function Mean (A : Real_Float_Matrix; Axis : Positive)
                   return Real_Float_Vector;
    function Mean (A : Real_Float_Matrix) return Real_Float_Vector;
    function Mean (A : Real_Float_Matrix) return Float;

end Neural_Maths;
