
with System;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;

package Python_Class is

   subtype PyTypeObject is System.Address;

   Interpreter_Error : exception;

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyTypeObject);
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyTypeObject) return PyTypeObject;
   function Call (M : Python.Module; Function_Name : String; A : Positive)
                  return PyTypeObject;
   function Call (M : Python.Module; Function_Name : String;
                  A : Float_Vector_List; B : ML_Types.Integer_List)
                  return PyTypeObject;
   function Call (M : Python.Module; Function_Name : String;
                  A : Integer_Array_List; B : ML_Types.Integer_List)
                  return PyTypeObject;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyTypeObject;
                  A   : Integer_Array) return Integer_Array;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyTypeObject;
                  A   : Integer_Matrix) return Integer_Array;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyTypeObject; A : Float_Vector_List)
                  return Real_Float_Matrix;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyTypeObject;
                  A   : Real_Float_Matrix) return Real_Float_Vector;

end Python_Class;
