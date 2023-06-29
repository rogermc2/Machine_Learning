
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Support_21A;

package Python_21A is

   function Call (M : Python.Module; Function_Name : String;
                  Q : Support_21A.Float_Tensor) return Float;
   procedure Planner (Classifier : Python.Module; R : Integer_Array;
                      Pi, Q      : out Real_Float_Matrix);
end Python_21A;
