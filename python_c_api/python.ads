
with System;

with Ada.Containers.Indefinite_Ordered_Maps;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;
with NL_Types;
with ML_Arrays_And_Matrices;
with Python_API;

package Python is

   subtype Module is System.Address;
   
   type Tuple_Item is private;    
   type Tuple_List_Array is array (Integer range <>) of Tuple_Item;
   
   package Tuple_Map is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => Unbounded_String, Element_Type => Integer);
   
   Interpreter_Error : exception;
   
   procedure Initialize (Program_Name : String := "");
   procedure Finalize;
    
   procedure Execute_String (Script : String);
   function Import_File (File_Name : String) return Module;
   function Call_Object (PyFunc : Python_API.PyObject_Ptr)
                         return Python_API.PyObject_Ptr;
   function Call_Object (PyFunc, PyParams : Python_API.PyObject_Ptr)
                         return Python_API.PyObject_Ptr;
   procedure Close_Module (M : Module);
   function Get_Symbol (M : in Module; Function_Name : in String)
                        return Python_API.PyObject_Ptr;
   function Convert_Tuple_List  (Tuple_List : Tuple_List_Array)
                                 return Tuple_Map.Map;
   function Py_String_To_Ada (C_String_Ptr : Python_API.PyObject_Ptr)
                              return String;
   
   --  Overloads for "all" needed combinations of parameters and return types:
   
   procedure Call (M : Module; Function_Name : String);
   function Call (M : Module; Function_Name : String)
                  return Python_API.PyObject_Ptr;
   procedure Call (M : Module; Function_Name : String;
                   A : Python_API.PyObject_Ptr);
   function Call (M : Module; Function_Name : String; A : Python_API.PyObject_Ptr)
                  return Integer;
   procedure Call (M : Module; Function_Name, A : String);
   function Call (M : Module; Function_Name, A : String)
                  return Python_API.PyObject_Ptr;
   function Call (M : Module; Function_Name : String; A : Float)
                  return Python_API.PyObject_Ptr;
   function Call (M : Module; Function_Name : String; A : Python_API.PyObject_Ptr)
                  return Boolean;
   function Call (M : Module; Function_Name : String; A : Integer)
                  return Integer;
   function Call (M : Module; Function_Name : String; A : Integer)
                  return Python_API.PyObject_Ptr;
   function Call (M : Module; Function_Name : String; A : Integer;
                  B : ML_Arrays_And_Matrices.Real_Float_Matrix)
                  return Python_API.PyObject_Ptr;
   procedure Call (M : Module; Function_Name : String; A, B : Integer);
   function Call (M : Module; Function_Name : String; A, B : Integer)
                  return Integer;
   function Call (M : Module; Function_Name : String; A : Integer; B : Float)
                  return Python_API.PyObject_Ptr;
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Array);
   function Call (M : Python.Module; Function_Name : String;
                  A : ML_Arrays_And_Matrices.Integer_Array_List;
                  B : ML_Types.Integer_List) return Python_API.PyObject_Ptr;
   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Types.Integer_List);
   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Types.Integer_List; B : Integer);
   function Call (M : Module; Function_Name : String;
                  A : Python_API.PyObject_Ptr)
                  return ML_Arrays_And_Matrices.Integer_Array;
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Matrix);
   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Integer_Matrix);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Array;
                   B : ML_Arrays_And_Matrices.Real_Float_List);
   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Arrays_And_Matrices.Integer_Array;
                   B, C : ML_Arrays_And_Matrices.Real_Float_List);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_List);
   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Types.Integer_List;
                   B, C : ML_Arrays_And_Matrices.Real_Float_List);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Vector;
                   B : ML_Arrays_And_Matrices.Real_Float_List);
   function Call (M : Module; Function_Name : String;
                  A : ML_Arrays_And_Matrices.Real_Float_List)
                  return ML_Arrays_And_Matrices.Real_Float_Vector;
   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Real_Float_Matrix);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B : ML_Arrays_And_Matrices.Boolean_Array);
   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Integer_Matrix;
                   C    : ML_Types.Unbounded_List);
   procedure Call (M : Module; Function_Name : String; A, B, C, D : Float);
   procedure Call (M : Module; Function_Name : String; A, B, C, D : Float;
                   E : String);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Matrix);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B : ML_Arrays_And_Matrices.Integer_Array);
   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Real_Float_Vector);
   function Call (M    : Module; Function_Name : String;
                  A, B : ML_Arrays_And_Matrices.Real_Float_Matrix;
                  C    : ML_Arrays_And_Matrices.Integer_Array)
                  return ML_Arrays_And_Matrices.Integer_Array;
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Vector;
                   B : ML_Arrays_And_Matrices.Integer_Array);
   procedure Call (M          : Module; Function_Name : String;
                   A, B, C, D : ML_Arrays_And_Matrices.Real_Float_Vector);
   procedure Call (M    : Module; Function_Name : String;
                   A, B : NL_Types.Float_List);
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
                   B    : ML_Arrays_And_Matrices.Real_Float_Vector;
                   C    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   D    : ML_Arrays_And_Matrices.Real_Float_Vector);
   procedure Call (M    : Module; Function_Name : String;
                   A    : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B    : ML_Arrays_And_Matrices.Integer_Matrix);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B : ML_Arrays_And_Matrices.Real_Float_Vector);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B : ML_Arrays_And_Matrices.Unbounded_String_Array);
   procedure Call (M : Module; Function_Name, Text : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B : ML_Arrays_And_Matrices.Unbounded_String_Array);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Matrix;
                   B : ML_Arrays_And_Matrices.Unbounded_String_Matrix);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_Vector);
   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Real_Float_Vector;
                   C    : ML_Arrays_And_Matrices.Real_Float_Matrix);
   procedure Call (M       : Module; Function_Name : String;
                   A, B, C : ML_Arrays_And_Matrices.Real_Float_Vector);
   procedure Call (M : Module; Function_Name : String;
                   A : NL_Types.Boolean_List_2D;
                   B : ML_Types.Bounded_String_List);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Float_List_2D);
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
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Real_Vector_List);
   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Real_Vector_List);
   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Real_Float_List);
   function Call (M : Module; Function_Name : String; A : Unbounded_String)
                  return Python_API.PyObject_Ptr;
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Unbounded_String_Array);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Types.Unbounded_List);
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Unbounded_String_Matrix);
   function Call (M : Python.Module; Function_Name : String;
                  A : ML_Arrays_And_Matrices.Real_Float_Matrix;
                  B : ML_Arrays_And_Matrices.Integer_Array)
                  return Python_API.PyObject_Ptr;
   function Run_String (Script : String) return Python_API.PyObject_Ptr;

private
   
   type Tuple_Item is record
      Key   : Unbounded_String;
      Value : Integer;
   end record;    

end Python;
