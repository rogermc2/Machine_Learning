
--  Based on inspirel_ada-python_demo

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;
with Python_API; use Python_API;

package Python_CLF is

   Interpreter_Error : exception;
   
   function Call (M   : Python.Module; Function_Name : String;
                  Obj : PyObject_Ptr) return Integer;
   function Call (M   : Python.Module; Function_Name : String;
                  Obj : PyObject_Ptr) return ML_Types.Unbounded_List;
   function Call (M : Python.Module; Function_Name : String; A : PyObject_Ptr)
                  return PyObject_Ptr;
   function Call (M : Python.Module; Function_Name : String; A : Positive)
                  return PyObject_Ptr;
   procedure Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                   A : Float_Array_List; B : ML_Types.Integer_List);
   procedure Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                   A : Float_Vector_List; B : ML_Types.Integer_List);
   function Call (M   : Python.Module; Function_Name : String;
                  Obj : PyObject_Ptr; A : Integer) return Float;
   function Call (M : Python.Module; Function_Name : String; A, B : Integer)
                  return PyObject_Ptr;
   function Call (M   : Python.Module; Function_Name : String;
                  Obj : PyObject_Ptr; A, B : Integer) return Float;
   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : Integer_Array_List) return Integer_Matrix;
   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : Integer_Array_List) return Real_Float_Matrix;
   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : Float_Vector_List) return Real_Float_Matrix;
   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : Real_Float_Matrix) return Real_Float_Matrix;
   procedure Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                   A : ML_Types.Integer_List_2D);
   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : ML_Types.Integer_List_2D) return Integer_Matrix;
   procedure Call (M    : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                   A, B : ML_Types.Integer_List_2D);
   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : Float_Array_List; B : ML_Types.Integer_List)
                  return PyObject_Ptr;
   function Call (M : Python.Module; Function_Name : String;
                  A : Float_Matrix_List; B : ML_Types.Integer_List)
                  return PyObject_Ptr;
   procedure Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                   A : ML_Types.Integer_List_2D; B : ML_Types.Integer_List);
   procedure Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr);
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr) return Integer_Array;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr) return ML_Types.Integer_List;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : Integer_Array_List)
                  return Integer_Array;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : ML_Types.Integer_List)
                  return Integer_Array;
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject_Ptr; A : Integer_Array_List;
                   B   : ML_Types.Integer_List);
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject_Ptr; A, B : ML_Types.Integer_List);
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : Real_Float_List);
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : Real_Float_Matrix);
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : Real_Float_Matrix;
                   B   : Boolean_Array); 
   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : Real_Float_Matrix) return Boolean_Array;
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : Real_Float_Matrix;
                   B   : Integer_Matrix);
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : Real_Float_Matrix;
                  B   : Integer_Matrix) return Float;
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : Real_Float_Matrix;
                   B   : Real_Float_Vector);
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : Real_Float_List)
                  return Real_Float_Vector;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : Float_Array)
                  return Real_Float_Matrix;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : Real_Float_Matrix)
                  return Real_Float_Vector;
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject_Ptr; A : ML_Types.Indef_String_List);
   procedure Call (M : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : Real_Float_Matrix;
                   B : Integer_Array);
   procedure Call (M : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; 
                   A : Real_Float_Vector; B : Integer_Array);
   function Call (M  : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; 
                  A  : Real_Float_Matrix; B : Integer_Array) return Float;
   function Call (M : Python.Module; Function_Name : String;
                  A : Real_Float_Matrix; B : Integer_Array)
                  return PyObject_Ptr;
   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : ML_Types.Unbounded_List);
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : ML_Types.Unbounded_List)
                  return ML_Types.Integer_List;
   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : ML_Types.Unbounded_List)
                  return ML_Types.Integer_List_2D;
   function Get_Attribute (CLF : PyObject_Ptr; Attribute : String)
                           return PyObject_Ptr;
   
end Python_CLF;
