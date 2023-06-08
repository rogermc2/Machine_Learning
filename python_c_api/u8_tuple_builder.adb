
with Interfaces.C;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with ML_Arrays_And_Matrices;
with Tuple_Builder;

package body U8_Tuple_Builder is
   use ML_U8_Types;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_U8_Types.Unsigned_8_Array_3D)
                      return PyObject_Ptr is
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

   function To_Tuple (Data : ML_U8_Types.Image_64_Vector)
                      return PyObject_Ptr is
      use Interfaces.C;
      Routine_Name : constant String :=
                         "U8_Tuple_Builder.To_Tuple Image_64_Vector ";
      Image        : Image_64_Array;
      Length_2D    : constant int := int (Image'Length (3));
      Length_3D    : constant int := int (Image'Length (2));
      Length_4D    : constant int := int (Data'Length);
      Tuple_4D     : constant PyObject_Ptr := PyTuple_New (Length_4D);

      function Do_Column (row : Positive) return PyObject_Ptr is
         Tuple_2D       : constant PyObject_Ptr := PyTuple_New (Length_2D);
         Tuple_3D       : constant PyObject_Ptr := PyTuple_New (Length_3D);
         Py_Col_Index   : int := 0;
         Py_Depth_Index : int;
      begin
         for col in Image'Range (2) loop
            Py_Depth_Index := 0;
            for depth in Image'Range (3) loop
               PyTuple_SetItem (Tuple_2D, Py_Depth_Index, PyLong_FromLong
                                (long (Image (row, col, depth))));
               Py_Depth_Index := Py_Depth_Index + 1;
            end loop;
            PyTuple_SetItem (Tuple_3D, Py_Col_Index, Tuple_2D);
            Py_Col_Index := Py_Col_Index + 1;
         end loop;

         Py_DecRef (Tuple_2D);
         return Tuple_3D;

      end Do_Column;

      function Do_Row return PyObject_Ptr is
         Tuple        : constant PyObject_Ptr := PyTuple_New (Length_3D);
         Py_Index     : int := 0;
         Py_Col_Index : int;
      begin
         for row in Image'Range loop
            Py_Col_Index := 0;
            for col in Image'Range (2) loop
               PyTuple_SetItem (Tuple, Py_Col_Index, Do_Column (row));
               Py_Col_Index := Py_Col_Index + 1;
            end loop;
            Py_Index := Py_Index + 1;
         end loop;

         return Tuple;

      end Do_Row;

      Py_Index : int := 0;
   begin
      Put_Line (Routine_Name & "Data Length" & Integer'Image (Data'Length));
      Put_Line (Routine_Name & "Image Length" & Integer'Image (Image'Length));
      Put_Line (Routine_Name & "Length_3D" & int'Image (Length_3D));
      Put_Line (Routine_Name & "Length_2D" & int'Image (Length_2D));
      for index in Data'Range loop
         Image := Data (index);
--           Put_Line (Routine_Name & "Py_Index" & int'Image (Py_Index));
         PyTuple_SetItem (Tuple_4D, Py_Index, Do_Row);
         Py_Index := Py_Index + 1;
      end loop;

      Put_Line (Routine_Name & "done");
      return Tuple_4D;

   end To_Tuple;

   --  -------------------------------------------------------------------------

end U8_Tuple_Builder;
