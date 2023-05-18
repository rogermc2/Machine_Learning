
with System;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;

package Python_Class is

   subtype PyTypeObject is System.Address;

   Interpreter_Error : exception;

   function Call (M : Python.Module; Function_Name : String; A : Positive)
                  return PyTypeObject;
   function Call (M : Python.Module; Function_Name : String;
                  A : Float_Vector_List; B : ML_Types.Integer_List)
                  return PyTypeObject;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyTypeObject; A : Float_Vector_List)
                  return Real_Float_Matrix;

end Python_Class;
