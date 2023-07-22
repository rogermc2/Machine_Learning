
with Interfaces.C; use Interfaces.C;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
--  with Parsers;
with Python_API; use Python_API;
with Tuple_Builder;

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

   function To_Tuple (Data : Support_22A.Boolean_Data_Array)
                      return PyObject_Ptr is
      --        Routine_Name : constant String := "Python_22a.To_Tuple Boolean_Data_Array ";
      Value        : long;
      Py_Row       : int := -1;
      Result       : constant PyObject_Ptr := PyTuple_New (int (Data'Length));
   begin
      for row in Data'Range loop
         Py_Row := Py_Row + 1;
         if Data (row) then
            Value := 1;
         else
            Value := 0;
         end if;
         PyTuple_SetItem (Result, Py_Row, PyBool_FromLong (Value));
      end loop;

      return Result;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : Support_22A.Float_Data_Array)
                      return PyObject_Ptr is
      --        Routine_Name : constant String := "Python_22a.To_Tuple Float_Data_Array ";
      Value        : Float;
      Py_Row       : int := -1;
      Result       : constant PyObject_Ptr := PyTuple_New (int (Data'Length));
   begin
      for row in Data'Range loop
         Py_Row := Py_Row + 1;
         Value := Data (row);
         PyTuple_SetItem (Result, Py_Row, PyFloat_FromDouble (double (Value)));
      end loop;

      return Result;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : Support_22A.Data_Record) return PyObject_Ptr is
      use Tuple_Builder;
      use Support_22A;
      use Support_22A.Data_Package;
      --        Routine_Name : constant String := "Python_22a.To_Tuple Data_Package ";
      Curs            : Cursor := Data.Data.First;
      Row             : Row_Record;
      Row_Tuple       : constant PyObject_Ptr := PyTuple_New (3);
      Treatment       : long;
      --        Float_Data      : Float_Data_Array (1 .. 10);
      --        Float_Tuple     : PyObject_Ptr := PyTuple_New (10);
      Data_Tuple      : constant PyObject_Ptr := PyTuple_New (int (Data.Data.Length));
      Py_Row          : int := -1;
      Tuple           : constant PyObject_Ptr := PyTuple_New (2);
   begin
      PyTuple_SetItem (Tuple, 0, To_Tuple (Data.Col_Names));

      while Has_Element (Curs) loop
         Row := Element (Curs);
         if Row.Treatment then
            Treatment := 1;
         else
            Treatment := 0;
         end if;
         PyTuple_SetItem (Row_Tuple, 0, PyBool_FromLong (Treatment));
         PyTuple_SetItem (Row_Tuple, 1, To_Tuple (Row.Float_Data));
         PyTuple_SetItem (Row_Tuple, 2, To_Tuple (Row.X7_25));
         Py_Row := Py_Row + 1;
         PyTuple_SetItem (Data_Tuple, Py_Row, To_Tuple (Row.X7_25));

         Next (Curs);
      end loop;

      PyTuple_SetItem (Tuple, 1, Data_Tuple);

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

end Python_22A;
