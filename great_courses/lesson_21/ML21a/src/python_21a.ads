
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API;
with Support_21A;

package Python_21A is

   type Plan_Data (Rows, Cols : Positive) is record
      Pi_Out : Real_Float_Matrix (1 .. Rows, 1 .. Cols);
      Q_Out  : Real_Float_Matrix (1 .. Rows, 1 .. Cols);
   end record;

   function Plan (Classifier : Python.Module;
                   R, Pi, Q : Python_API.PyObject_Ptr) return Plan_Data;
   procedure Set_Policy (Classifier     : Python.Module; R : Integer_Array;
                         Mat_Map        : Support_21A.Boolean_Tensor;
                         Mat_Transition : Support_21A.Boolean_Tensor;
                         Rk, Pi, Q, V   : out Python_API.PyObject_Ptr);
end Python_21A;
