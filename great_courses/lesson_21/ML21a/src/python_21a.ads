
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Support_21A;

package Python_21A is

   type Plan_Data (Rows, Cols : Positive) is record
      Policy : Real_Float_Matrix (1 .. Rows, 1 .. Cols);
      Q      : Real_Float_Matrix (1 .. Rows, 1 .. Cols);
   end record;

   function Set_Policy (Classifier     : Python.Module;
                         Rewards        : Integer_Array;
                         Mat_Map        : Support_21A.Boolean_Tensor;
                         Mat_Transition : Support_21A.Boolean_Tensor)
                        return Plan_Data;

end Python_21A;
