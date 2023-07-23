
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
--  with Parsers;
with Python_API; use Python_API;
with Tuple_Builder;

package body Python_22A is

   function To_Tuple (Data : Support_22A.Data_List) return PyObject_Ptr;

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

   function Set_Model (Classifier : Python.Module; Data : Data_Record;
                       X_String   : Unbounded_String)
                       return Python_API.PyObject_Ptr is
      Routine_Name    : constant String := "Python_22A.Set_Model ";

      function Py_BuildValue (Format : char_array; T1, T2, S1 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F           : constant PyObject_Ptr :=
                      Python.Get_Symbol (Classifier, "init");
      Col_Tuple   : constant PyObject_Ptr :=
                      Tuple_Builder.To_Tuple (Data.Col_Names);
      Data_Tuple  : constant PyObject_Ptr := To_Tuple (Data.Data);
      C_String    : constant char_array := To_C (To_String (X_String));
      PyParams    : PyObject_Ptr;
      PyResult    : PyObject_Ptr;
   begin
      Put_Line (Routine_Name & "Data.Col_Names length: " &
                  Integer'Image (Integer (Data.Col_Names.Length)));
      Put_Line (Routine_Name & "Data.Data length: " &
                  Integer'Image (Integer (Data.Data.Length)));
      Put_Line (Routine_Name & "X_String: '" & To_String (X_String) & "'");
      PyParams := Py_BuildValue (To_C ("OOs"), Col_Tuple, Data_Tuple,
                                 PyString_FromString (C_String));
      PyResult := Python.Call_Object (F);

      Py_DecRef (F);
      Py_DecRef (Col_Tuple);
      Py_DecRef (Data_Tuple);
      Py_DecRef (PyParams);

      return PyResult;

   end Set_Model;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : Support_22A.Data_List) return PyObject_Ptr is
      use Support_22A.Data_Package;
      --        Routine_Name : constant String := "Python_22a.To_Tuple Data_Package ";
      Curs            : Cursor := Data.First;
      Row             : Row_Record;
      Row_Tuple       : constant PyObject_Ptr := PyTuple_New (30);
      Treatment       : long;
      Data_Tuple      : constant PyObject_Ptr :=
                          PyTuple_New (int (Data.Length));
      Py_Row          : int := 0;
      Py_Col          : int;
   begin
      while Has_Element (Curs) loop
         Row := Element (Curs);
         Py_Col := 0;
         if Row.Treatment then
            Treatment := 1;
         else
            Treatment := 0;
         end if;
         PyTuple_SetItem (Row_Tuple, Py_Col, PyBool_FromLong (Treatment));
         for col in Row.Float_Data'Range loop
            Py_Col := Py_Col + 1;
            PyTuple_SetItem
              (Row_Tuple, Py_Col,
               PyFloat_FromDouble (double (Row.Float_Data (col))));
         end loop;

         for col in Row.X7_25'Range loop
            Py_Col := Py_Col + 1;
            if Row.X7_25 (col) then
               PyTuple_SetItem (Row_Tuple, Py_Col, PyBool_FromLong (1));
            else
               PyTuple_SetItem (Row_Tuple, Py_Col, PyBool_FromLong (0));
            end if;
         end loop;

         Py_Row := Py_Row + 1;
         PyTuple_SetItem (Data_Tuple, Py_Row, Row_Tuple);

         Next (Curs);
      end loop;

      return Data_Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

end Python_22A;
