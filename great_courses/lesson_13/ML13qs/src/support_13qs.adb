
with Interfaces.C;

--  with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;

package body Support_13QS is

   function Arg_Max (Values : Real_Float_Vector) return Integer is
      Max_Index : Integer := Values'First;
      Max_Value : Float := Values (Max_Index);
   begin
      for row in Values'Range loop
         if Values (row) > Max_Value then
            Max_Index := row;
            Max_Value := Values (Max_Index);
         end if;
      end loop;

      return Max_Index;

   end Arg_Max;

   --  -------------------------------------------------------------------------

   function Max (Values : Real_Float_Vector) return Float is
      Max_Value : Float := Values (Values'First);
   begin
      for row in Values'Range loop
         if Values (row) > Max_Value then
            Max_Value := Values (row);
         end if;
      end loop;

      return Max_Value;

   end Max;

   --  -------------------------------------------------------------------------

   function Step (M          : Python.Module; Function_Name : String;
                  Env        : Python_API.PyObject; Action : Integer;
                  Next_State : out Integer_Array; Reward : out Integer)
                  return Boolean is
      use Interfaces.C;
      use Python;
      use Python_API;
      --        Routine_Name : constant String := "Support_13a.Step ";

      function Parse_Tuple (Tuple  : PyObject; State : out Integer_Array;
                            Reward : out Integer) return Boolean is
         Py_Obs : PyObject;
      begin
         --  step returns next_state, reward, terminated, truncated , info
         Py_Obs := PyTuple_GetItem (Tuple, 0);
         State (1) :=
           Integer (PyInt_AsLong (PyTuple_GetItem (Py_Obs, 0)));
         State (2) :=
           Integer (PyInt_AsLong (PyTuple_GetItem (Py_Obs, 1)));
         State (3) :=
           Integer (PyInt_AsLong (PyTuple_GetItem (Py_Obs, 2)));

         Reward := Integer (PyFloat_AsDouble (PyTuple_GetItem (Tuple, 1)));

         return Integer (PyInt_AsLong (PyTuple_GetItem (Tuple, 2))) /= 0 or
           Integer (PyInt_AsLong (PyTuple_GetItem (Tuple, 3))) /= 0;

      end Parse_Tuple;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject; I1 : int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : Boolean;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("Oi"), Env, int (Action));

      PyResult := Call_Object (F, PyParams);
      Result := Parse_Tuple (PyResult, Next_State, Reward);

      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Step;

   --  -------------------------------------------------------------------------

end Support_13QS;
