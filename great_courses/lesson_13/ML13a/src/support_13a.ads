
with System;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;
with Python;
with Python_API;

package Support_13A is

   type Data_Items (Rows : Positive) is record
      Features : Integer_Array_List;
      Labels   : Integer_Matrix (1 .. Rows, 1 .. 1);
   end record;

   function Action_Picker (Classifier : Python.Module;
                           Env        : Python_API.PyObject;
                           CLF        : Python_API.PyObject :=
                             System.Null_Address;
                           Observation : Real_Float_Vector;
                           Epsilon     : Float) return Natural;
   function Call (M           : Python.Module; Function_Name : String;
                  Env         : Python_API.PyObject; Action : Integer;
                  Observation : out Real_Float_Vector; Reward : out Float)
                  return Boolean;
   function Max (Values : Real_Float_Vector) return Float;
--     function Train (Classifier : Python.Module; Data : Float_Array_List;
--                     Labels : ML_Types.Integer_List) return Python_API.PyObject;

end Support_13A;
