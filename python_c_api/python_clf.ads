
--  Based on inspirel_ada-python_demo

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;
with Python_API; use Python_API;

package Python_CLF is

   Interpreter_Error : exception;
   
   function Call (M : Python.Module; Function_Name : String; A : Positive)
                  return PyObject;
   function Call (M   : Python.Module; Function_Name : String;
                  Obj : PyObject; A : Integer) return Float;
   function Call (M : Python.Module; Function_Name : String; A, B : Integer)
                  return PyObject;
   function Call (M   : Python.Module; Function_Name : String;
                  Obj : PyObject; A, B : Integer) return Float;
   function Call (M    : Python.Module; Function_Name : String;
                  A    : Integer_Array_List; B : ML_Types.Integer_List)
                  return PyObject;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject; A : Integer_Array_List)
                  return Integer_Array;
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject; A : Integer_Array_List;
                   B   : ML_Types.Integer_List);
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject; A : ML_Types.Indef_String_List);
   procedure Call (M : Python.Module; Function_Name : String; CLF : PyObject; 
                   A : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B : ML_Arrays_And_Matrices.Integer_Array);
   function Call (M  : Python.Module; Function_Name : String; CLF : PyObject; 
                   A : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B : ML_Arrays_And_Matrices.Integer_Array) return Float;
   function Get_Attribute (CLF : PyObject; Attribute : String)
                           return PyObject;
   
end Python_CLF;
