
--  Based on inspirel_ada-python_demo

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API; use Python_API;

package Python_CLF is

   Interpreter_Error : exception;
   
   function Call (M : Python.Module; Function_Name : String; A : Positive)
                  return PyObject;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject; A : Integer_Array_List) return Float;
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject; A, B, C, D : Integer_Array_List);
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject; A, B : Integer_Array_List);
   
end Python_CLF;
