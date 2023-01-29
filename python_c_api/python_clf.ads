
--  Based on inspirel_ada-python_demo

with System;

with ML_Arrays_And_Matrices;
with Python_API; use Python_API;

package Python_CLF is

   Interpreter_Error : exception;
   
   type Module is private;
   
   procedure Call (M : Module; Function_Name : String;
                   CLF : in out PyObject;
                   A : ML_Arrays_And_Matrices.Integer_Array_List;
                   B : ML_Arrays_And_Matrices.Integer_Array_List;
                   C : ML_Arrays_And_Matrices.Integer_Array_List;
                   D : ML_Arrays_And_Matrices.Integer_Array_List);
private
 
   type Module is new System.Address;
   
end Python_CLF;
