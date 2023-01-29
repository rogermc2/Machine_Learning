
--  Based on inspirel_ada-python_demo

with ML_Arrays_And_Matrices;
with Python;
with Python_API; use Python_API;

package Python_CLF is

   Interpreter_Error : exception;
   
   function Call (M : Python.Module; Function_Name : String; A : Positive)
                  return PyObject;
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject;
                   A   : ML_Arrays_And_Matrices.Integer_Array_List;
                   B   : ML_Arrays_And_Matrices.Integer_Array_List;
                   C   : ML_Arrays_And_Matrices.Integer_Array_List;
                   D   : ML_Arrays_And_Matrices.Integer_Array_List);
   
end Python_CLF;
