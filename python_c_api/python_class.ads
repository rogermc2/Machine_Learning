
with System;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;

package Python_Class is

   --  PyClass is Python TypeObject
   subtype PyClass is System.Address;

   Interpreter_Error : exception;

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyClass);
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyClass) return PyClass;
   function Call (M : Python.Module; Function_Name : String; A : Positive)
                  return PyClass;
   function Call (M : Python.Module; Function_Name : String;
                  A : Float_Vector_List; B : ML_Types.Integer_List)
                  return PyClass;
   function Call (M : Python.Module; Function_Name : String;
                  A : Integer_Array_List; B : ML_Types.Integer_List)
                  return PyClass;
   function Call (M : Python.Module; Function_Name : String;
                  A : Integer_Array_List; B : Real_Float_List)
                  return PyClass;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyClass; A : Integer_Array) return Integer_Array;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyClass; A : Integer_Array) return Real_Float_Vector;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyClass; A : Integer_Matrix) return Integer_Array;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyClass; A : Integer_Matrix) return Real_Float_Vector;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyClass; A : Float_Vector_List)
                  return Real_Float_Matrix;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyClass; A : Real_Float_Matrix) return Real_Float_Vector;

end Python_Class;
