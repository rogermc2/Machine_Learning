
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API;
with Support_10A;

package Python_10A is

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : Python_API.PyObject; A : Support_10A.Features_Array)
                  return Integer_Array;
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : Python_API.PyObject; A : Support_10A.Features_Array;
                   B   : Integer_Array);
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : Python_API.PyObject; A : Support_10A.Features_Record)
                  return Real_Float_Matrix;

end Python_10A;
