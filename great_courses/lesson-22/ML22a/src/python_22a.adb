
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

   function To_Tuple (Data : Support_22A.Data_Record) return PyObject_Ptr is
      use Tuple_Builder;
      use Support_22A;
      use Support_22A.Data_Package;
      --        Routine_Name : constant String := "Python_22a.To_Tuple Data_Package ";
      Curs            : Cursor := Data.Data.First;
      Row             : Row_Record;
      Col_Tuple       : PyObject_Ptr;
      Treatment       : Boolean;
      Data_Tuple      : PyObject_Ptr := PyTuple_New (int (Data.Data.Length));
      Tuple           : PyObject_Ptr;
   begin
      Col_Tuple := To_Tuple (Data.Col_Names);

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

      while Has_Element (Curs) loop
         Row := Element (Curs);
         Treatment := Row.Treatment;
         Next (Curs);
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

end Python_22A;
