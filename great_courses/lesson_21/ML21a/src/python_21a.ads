
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API;
with Support_21A;

package Python_21A is

   function Call (M : Python.Module; Function_Name : String;
                  Q : Support_21A.Float_Tensor) return Float;
   procedure Plan (Classifier    : Python.Module; R : Python_API.PyObject_Ptr;
                   Pi, Q         :  Real_Float_Matrix;
                   Pi_Out, Q_Out : out Real_Float_Matrix);
end Python_21A;
