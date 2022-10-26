
with System;

package Python is

   Interpreter_Error : exception;
   
   type Module is private;

   procedure Initialize (Program_Name : in String := "");
   procedure Finalize;
    
   procedure Execute_String (Script : in String);

   function Import_File (File_Name : in String) return Module;
   procedure Close_Module (M : in Module);
   
   --  Overloads for "all" needed combinations of parameters and return types:
   
   procedure Call (M : in Module; Function_Name : in String);
   function  Call (M : in Module; Function_Name : in String; A : in Integer;
                   B : Integer) return Integer;
   --  ...
   
private

   type Module is new System.Address;

end Python;
