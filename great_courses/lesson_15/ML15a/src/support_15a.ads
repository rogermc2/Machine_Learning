
with System;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API;

package Support_15A is

   type String_9 is new String (1 .. 9);
   type String_9_Array is array (Integer range <>) of String_9;
   type Labels_Array is array (Integer range <>) of Integer;

   function Action_Picker (Classifier  : Python.Module;
                           Env         : Python_API.PyObject;
                           CLF         : Python_API.PyObject :=
                             System.Null_Address;
                           Observation : Integer_Array;
                           Epsilon     : Float) return Boolean;
   function Read_Cats (M : Python.Module; Cats : String_9_Array; Labels : Labels_Array;
                      Train_Size, Test_Size : Positive)
                  return Boolean;
   function Max (Values : Real_Float_Vector) return Float;

end Support_15A;
