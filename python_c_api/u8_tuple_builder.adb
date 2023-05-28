
with Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with ML_Arrays_And_Matrices;
with Tuple_Builder;

package body U8_Tuple_Builder is
   use ML_U8_Types;

   --  -------------------------------------------------------------------------
--
--     function To_Tuple (Data : Interfaces.Unsigned_8)
--                        return PyObject is
--        use Interfaces.C;
--        --        Routine_Name : constant String := "U8_Tuple_Builder.To_Tuple Unsigned_8 ";
--        Py_Tuple     : constant PyObject := PyTuple_New (1);
--     begin
--        PyTuple_SetItem (Py_Tuple, 0, PyLong_FromLong (long (Data)));
--
--        return Py_Tuple;
--
--     end To_Tuple;

   --  -------------------------------------------------------------------------

--     function To_Tuple (Data : ML_U8_Types.Unsigned_8_Array)
--                        return PyObject is
--        use Interfaces.C;
--        --        Routine_Name : constant String := "U8_Tuple_Builder.To_Tuple Unsigned_8_Array ";
--        Tuple_1D     : constant PyObject := PyTuple_New (int (Data'Length));
--        Py_Index     : int := 0;
--     begin
--        for index in Data'Range loop
--           PyTuple_SetItem (Tuple_1D, Py_Index, To_Tuple (Data (index)));
--           Py_Index := Py_Index + 1;
--        end loop;
--
--        return Tuple_1D;
--
--     end To_Tuple;

   --  -------------------------------------------------------------------------

--     function To_Tuple (Data : Unsigned_8_Array_2D)
--                        return PyObject is
--        use Interfaces.C;
--        --        Routine_Name : constant String := "U8_Tuple_Builder.To_Tuple Unsigned_8_Array_2D ";
--        Tuple_2D     : constant PyObject := PyTuple_New (int (Data'Length));
--        Array_1D     : Unsigned_8_Array (1 .. Data'Length * Data'Length (2));
--        Py_Index     : int := 0;
--     begin
--        for row in Data'Range loop
--           for col in Data'Range (2) loop
--              Array_1D ((row - 1) + col) := Data (row, col);
--           end loop;
--           PyTuple_SetItem (Tuple_2D, Py_Index, To_Tuple (Array_1D));
--           Py_Index := Py_Index + 1;
--        end loop;
--
--        return Tuple_2D;
--
--     end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_U8_Types.Unsigned_8_Array_3D)
                      return PyObject is
      use ML_Arrays_And_Matrices;
      --        Routine_Name : constant String := "U8_Tuple_Builder.To_Tuple Unsigned_8_Array_3D ";
      Array_Length : constant Positive := Data'Length (1) * Data'Length (2);
      Array_2D     : Integer_Matrix (1 .. Array_Length, Data'Range (3));
   begin
      for row in Data'Range loop
         for col in Data'Range (2) loop
            for depth in Data'Range (3) loop
               Array_2D ((row - 1) * Data'Length (2) + col, depth) :=
                 Integer (Data (row, col, depth));
            end loop;
         end loop;
      end loop;

      return Tuple_Builder.To_Tuple (Array_2D);

   end To_Tuple;

   --  -------------------------------------------------------------------------

--     function To_Tuple (Data : ML_U8_Types.Unsigned_8_Array_3D)
--                        return PyObject is
--        use Interfaces.C;
--        --        Routine_Name : constant String := "U8_Tuple_Builder.To_Tuple Unsigned_8_Array_3D ";
--        Array_Length : constant Positive := Data'Length (1) * Data'Length (2);
--        Tuple_3D     : constant PyObject := PyTuple_New (int (Data'Length));
--        Array_2D     : Unsigned_8_Array_2D (1 .. Array_Length, Data'Range (3));
--        Py_Index     : int := 0;
--     begin
--        for row in Data'Range loop
--           for col in Data'Range (2) loop
--              for depth in Data'Range (3) loop
--                 Array_2D ((row - 1) * Data'Length (2) + col, depth) :=
--                   Data (row, col, depth);
--              end loop;
--           end loop;
--           PyTuple_SetItem (Tuple_3D, Py_Index, To_Tuple (Array_2D));
--           Py_Index := Py_Index + 1;
--        end loop;
--
--        return Tuple_3D;
--
--     end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_U8_Types.Image_64_Vector)
                      return PyObject is
      use Interfaces.C;
      Routine_Name : constant String :=
                             "U8_Tuple_Builder.To_Tuple Image_64_Vector ";
      Tuple_4D  : constant PyObject := PyTuple_New (int (Data'Length));
      Py_Index  : int := 0;
   begin
      Put_Line (Routine_Name);
      for index in Data'Range loop
         PyTuple_SetItem (Tuple_4D, Py_Index,
                          To_Tuple (Unsigned_8_Array_3D (Data (index))));
         Py_Index := Py_Index + 1;
      end loop;

      return Tuple_4D;

   end To_Tuple;

   --  -------------------------------------------------------------------------

end U8_Tuple_Builder;
