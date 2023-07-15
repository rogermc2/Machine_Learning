
with Interfaces.C; use Interfaces.C;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Parsers;
with Python_API; use Python_API;
with Tuple_Builder;

package body Python_21A is

   function To_Tuple (Tensor : Support_21A.Boolean_Tensor) return PyObject_Ptr;

   --  -------------------------------------------------------------------------

   function Parse_Tuple (Tuple : PyObject_Ptr) return Plan_Data is
      --        Routine_Name : constant String := "Parsers.Parse_Tuple Pi, Q ";
      Pi           : constant Real_Float_Matrix :=
                       Parsers.Parse_Tuple (PyTuple_GetItem (Tuple, 0));
      Q            : constant Real_Float_Matrix:=
                       Parsers.Parse_Tuple (PyTuple_GetItem (Tuple, 1));
      Result       : Plan_Data (Pi'Length, Pi'Length (2));
   begin
      Result.Policy := Pi;
      Result.Q := Q;

      return Result;

   end Parse_Tuple;

   --  -------------------------------------------------------------------------

   function Set_Policy (Classifier     : Python.Module;
                        Rewards        : Integer_Array;
                        Mat_Map        : Support_21A.Boolean_Tensor;
                        Mat_Transition : Support_21A.Boolean_Tensor) return Plan_Data is
      use Tuple_Builder;
      --        Routine_Name    : constant String := "Python_21A.Set_Policy  ";

      function Py_BuildValue (Format : char_array; T1, T2, T3 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F           : constant PyObject_Ptr := Python.Get_Symbol (Classifier,
                                                                "policy");
      R_Tuple     : constant PyObject_Ptr := To_Tuple (Rewards);
      Map_Tuple   : constant PyObject_Ptr := To_Tuple (Mat_Map);
      Trans_Tuple : constant PyObject_Ptr := To_Tuple (Mat_Transition);
      PyParams    : PyObject_Ptr;
      PyResult    : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (To_C ("OOO"), R_Tuple, Map_Tuple, Trans_Tuple);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (R_Tuple);
      Py_DecRef (Map_Tuple);
      Py_DecRef (Trans_Tuple);
      Py_DecRef (PyParams);

      declare
         Result : constant Plan_Data := Parse_Tuple (PyResult);
      begin
         Py_DecRef (PyResult);
         return Result;
      end;

   end Set_Policy;

   --  -------------------------------------------------------------------------

   function To_Tuple (Tensor : Support_21A.Boolean_Tensor)
                      return PyObject_Ptr is
      --        Routine_Name : constant String := "Python_21a.To_Tuple Boolean_Tensor ";
      Tuple        : PyObject_Ptr;
      Row_Tuple    : PyObject_Ptr;
      Col_Tuple    : PyObject_Ptr;
      Value        : long;
      Py_1         : int := -1;
      Py_2         : int;
      Py_3         : int;
   begin
      Tuple := PyTuple_New (int (Tensor'Length));
      for row in Tensor'Range loop
         Row_Tuple := PyTuple_New (int (Tensor'Length (2)));
         Py_1 := Py_1 + 1;
         Py_2 := -1;
         for col in Tensor'Range (2) loop
            Col_Tuple := PyTuple_New (int (Tensor'Length (3)));
            Py_2 := Py_2 + 1;
            Py_3 := -1;
            for item in Tensor'Range (3) loop
               Py_3 := Py_3 + 1;
               if Tensor (row, col, item) then
                  Value := 1;
               else
                  Value := 0;
               end if;

               PyTuple_SetItem (Col_Tuple, Py_3,
                                PyLong_FromLong (Value));
            end loop;
            PyTuple_SetItem (Row_Tuple, Py_2, Col_Tuple);
         end loop;
         PyTuple_SetItem (Tuple, Py_1, Row_Tuple);
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

end Python_21A;
