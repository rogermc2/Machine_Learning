
with System;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

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
   function  Call (M : Module; Function_Name : String; A : Integer)
                   return Integer;
   function  Call (M : Module; Function_Name : String; A, B : Integer)
                   return Integer;
   procedure  Call (M    : Module; Function_Name : String;
                   A, B : Integer_Matrix);
   --  ...
   
private

   type Module is new System.Address;

end Python;
