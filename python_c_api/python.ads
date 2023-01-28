
with System;

with ML_Types;
with NL_Types;
with ML_Arrays_And_Matrices;

package Python is

   Interpreter_Error : exception;
   
   type Module is private;

   procedure Initialize (Program_Name : String := "");
   procedure Finalize;
    
   procedure Execute_String (Script : String);

   function Import_File (File_Name : String) return Module;
   procedure Close_Module (M : Module);
   
   --  Overloads for "all" needed combinations of parameters and return types:
   
   procedure Call (M : Module; Function_Name : String);
   procedure Call (M : Module; Function_Name, A : String);
   function Call (M : Module; Function_Name : String; A : Integer)
                  return Integer;
   function Call (M : Module; Function_Name : String; A, B : Integer)
                  return Integer;
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Matrix);
   procedure Call (M    : Module; Function_Name : String;
                   A, B : ML_Arrays_And_Matrices.Integer_Matrix);
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
   function Call (M : Module; Function_Name : String;
                  A : ML_Arrays_And_Matrices.Integer_Array_List)
                  return ML_Arrays_And_Matrices.Integer_Array_List;
   procedure Call (M : Module; Function_Name : String;
                   A : ML_Arrays_And_Matrices.Integer_Array_List;
                   B : ML_Arrays_And_Matrices.Integer_Array_List);
   
private

   type Module is new System.Address;

end Python;
