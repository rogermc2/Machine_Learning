
with Interfaces.C; use Interfaces.C;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
--  with Parsers;
with Python_API; use Python_API;
--  with Tuple_Builder;

package body Python_22A is

   function To_Tuple (Data : Support_22A.Data_Record) return PyObject_Ptr;

   --  -------------------------------------------------------------------------

--     function Parse_Tuple (Tuple : PyObject_Ptr) return Plan_Data is
--        --        Routine_Name : constant String := "Parsers.Parse_Tuple Pi, Q ";
--        Pi           : constant Real_Float_Matrix :=
--                         Parsers.Parse_Tuple (PyTuple_GetItem (Tuple, 0));
--        Q            : constant Real_Float_Matrix:=
--                         Parsers.Parse_Tuple (PyTuple_GetItem (Tuple, 1));
--        Result       : Plan_Data (Pi'Length, Pi'Length (2));
--     begin
--        Result.Policy := Pi;
--        Result.Q := Q;
--
--        return Result;
--
--     end Parse_Tuple;

   --  -------------------------------------------------------------------------

   function Set_Model (Classifier : Python.Module; Data : Support_22A.Data_Record)
                        return Python_API.PyObject_Ptr is
--        use Tuple_Builder;
      --        Routine_Name    : constant String := "Python_22A.Set_Model  ";

      function Py_BuildValue (Format : char_array; T1 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F           : constant PyObject_Ptr := Python.Get_Symbol (Classifier,
                                                                "init_model");
      Data_Tuple  : constant PyObject_Ptr := To_Tuple (Data);
      PyParams    : PyObject_Ptr;
      PyResult    : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (To_C ("(O)"), Data_Tuple);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (Data_Tuple);
      Py_DecRef (PyParams);

      return PyResult;

   end Set_Model;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : Support_22A.Data_Record) return PyObject_Ptr is
      --        Routine_Name : constant String := "Python_22a.To_Tuple Data_Package ";
      Tuple        : PyObject_Ptr;
      Row_Tuple    : PyObject_Ptr;
      Col_Tuple    : PyObject_Ptr;
      Value        : long;
      Py_1         : int := -1;
      Py_2         : int;
      Py_3         : int;
   begin
--   col =  ["treatment", "y_factual", "y_cfactual", "mu0", "mu1" ,]
--  for i in range(1,26):
--      col.append("x"+str(i))
--  data.columns = col
--  data = data.astype({"treatment": bool})

--     type Row_Record is record
--        Treatment  : Boolean;
--        Float_Data : Float_Data_Array (2 .. 11);
--        X7_25      : Boolean_Data_Array (12 .. 25);
--     end record;

--     package Data_Package is new
--       Ada.Containers.Doubly_Linked_Lists (Row_Record);
--     subtype Data_List is Data_Package.List;

--     type Data_Record is record
--        Col_Names  : ML_Types.Indef_String_List;
--        Data       : Data_List;
--     end record;
      Col_Tuple := PyTuple_New (int (Data.Col_Names'Length));
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

end Python_22A;
