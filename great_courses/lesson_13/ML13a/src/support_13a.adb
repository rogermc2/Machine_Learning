
--  with System;
--
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Containers;
--  with Ada.Text_IO; use Ada.Text_IO;
--
--  with Maths;

--  with Basic_Printing; use Basic_Printing;
--  with Neural_Utilities;
--  with Python_CLF;

package body Support_13A is

   function Action_Picker (Classifier  : Python.Module;
                           Env         : Python_API.PyObject;
                           Observation : Positive;
                           Epsilon     : Float) return Natural is
      Result : Natural := 0;
   begin

      return Result;

   end Action_Picker;

   --  -------------------------------------------------------------------------

end Support_13A;
