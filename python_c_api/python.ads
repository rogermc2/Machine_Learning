
with System;

with ML_Types;
with NL_Types;
with ML_Arrays_And_Matrices;
with Python_API;

package Python is

   type Module is new System.Address;
   
   Interpreter_Error : exception;
   
   procedure Initialize (Program_Name : String := "");
   procedure Finalize;
    
   procedure Execute_String (Script : String);
   function Import_File (File_Name : String) return Module;
   function Call_Object (PyFunc, PyParams : Python_API.PyObject)
                         return Python_API.PyObject;
   procedure Close_Module (M : Module);
   function Get_Symbol (M : in Module; Function_Name : in String)
                        return Python_API.PyObject;
   
   --  Overloads for "all" needed combinations of parameters and return types:
   
   procedure Call (M : Module; Function_Name : String);
   function Call (M : Module; Function_Name : String)
                  return Python_API.PyObject;
   procedure Call (M : Module; Function_Name, A : String);
   function Call (M : Module; Function_Name : String; A : Integer)
                  return Integer;
   function Call (M : Module; Function_Name : String; A, B : Integer)
                  return Integer;
   function Call (M : Module; Function_Name : String; A : Integer; B : Float)
                  return Python_API.PyObject;
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Matrix);
   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Integer_Matrix);
   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Real_Float_Matrix);
   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Integer_Matrix;
                   C    : ML_Types.Unbounded_List);
   procedure Call (M : Module; Function_Name : String; A, B, C, D : Float);
   procedure Call (M : Module; Function_Name : String; A, B, C, D : Float;
                   E : String);
   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Arrays_And_Matrices.Real_Float_Matrix);
   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B    : ML_Arrays_And_Matrices.Integer_Array);
   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B    : ML_Arrays_And_Matrices.Integer_Array;
                   C    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   D    : ML_Arrays_And_Matrices.Integer_Array);
   procedure Call (M    : Module; Function_Name : String;
                   A    : in out ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B    : in out ML_Arrays_And_Matrices.Integer_Matrix;
                   C    : in out ML_Arrays_And_Matrices.Real_Float_Matrix;
                   D    : in out ML_Arrays_And_Matrices.Integer_Matrix);
   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B    : ML_Arrays_And_Matrices.Integer_Matrix);
   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B    : ML_Arrays_And_Matrices.Real_Float_Vector);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Vector);
   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Real_Float_Vector;
                   C    : ML_Arrays_And_Matrices.Real_Float_Matrix);
   procedure Call (M       : Module; Function_Name : String;
                   A, B, C : ML_Arrays_And_Matrices.Real_Float_Vector);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Unsigned_8_Array_3D);
   procedure Call (M : Module; Function_Name : String;
                   A : NL_Types.Boolean_List_2D;
                   B : ML_Types.Bounded_String_List);
   procedure Call (M : Module; Function_Name : String;
                   A : NL_Types.Boolean_List_2D; B : NL_Types.Boolean_List;
                   C : NL_Types.Boolean_List_2D);
   procedure Call (M : Module; Function_Name : String;
                   A : NL_Types.Boolean_List_2D; B : NL_Types.Boolean_List;
                   C : NL_Types.Boolean_List_2D; D : ML_Types.Unbounded_List);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Array_List);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Array_List;
                   B : ML_Arrays_And_Matrices.Integer_Array_List);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Array_List;
                   B : ML_Arrays_And_Matrices.Integer_Array_List;
                   C : ML_Arrays_And_Matrices.Integer_Array_List);
   function Call (M : Module; Function_Name : String;
                  A : ML_Arrays_And_Matrices.Integer_Array_List;
                  B : ML_Arrays_And_Matrices.Integer_Array_List;
                  C : ML_Arrays_And_Matrices.Integer_Array_List;
                  D : ML_Arrays_And_Matrices.Integer_Array_List)
                  return Float;
   function Run_String (Script : String) return Python_API.PyObject;

end Python;
