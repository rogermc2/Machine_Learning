
with System;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API;

package Support_15A is

   function Action_Picker (Classifier  : Python.Module;
                           Env         : Python_API.PyObject;
                           CLF         : Python_API.PyObject :=
                             System.Null_Address;
                           Observation : Integer_Array;
                           Epsilon     : Float) return Boolean;
   function Step (M          : Python.Module; Function_Name : String;
                  Env        : Python_API.PyObject; Action : Boolean;
                  Next_State : out Integer_Array; Reward : out Integer)
                  return Boolean;
   function Max (Values : Real_Float_Vector) return Float;

end Support_15A;
