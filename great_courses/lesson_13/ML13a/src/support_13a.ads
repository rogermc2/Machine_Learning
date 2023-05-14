
with System;

--  with Ada.Containers.Doubly_Linked_Lists;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;
with Python;
with Python_API;

package Support_13A is

   type Data_Items (Rows : Positive) is record
      Features : Integer_Array_List;
      Labels   : Integer_Matrix (1 .. Rows, 1 .. 1);
   end record;

   function Action_Picker (Classifier  : Python.Module;
                           Env         : Python_API.PyObject;
                           CLF         : Python_API.PyObject :=
                             System.Null_Address;
                           Observation : Positive;
                           Epsilon     : Float) return Natural;

end Support_13A;
