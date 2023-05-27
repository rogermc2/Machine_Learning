
with Interfaces.C;

--  with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with ML_Arrays_And_Matrices;
with Tuple_Builder;

package body U8_Tuple_Builder is

   function To_Tuple (Data : ML_U8_Types.Unsigned_8_Array_3D)
                      return PyObject is
      use ML_Arrays_And_Matrices;
      --        Routine_Name : constant String := "U8_Tuple_Builder.To_Tuple Integer_Array_3D ";
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
                      return PyObject is
      use Interfaces.C;
      use ML_U8_Types;
--        Routine_Name : constant String :=
--                         "U8_Tuple_Builder.To_Tuple Image_64_Vector ";
      Tuple_3D  : constant PyObject := PyTuple_New (int (Data'Length));
   begin
      for index in Data'Range loop
         PyTuple_SetItem (Tuple_3D, int (index - 1),
                          To_Tuple (Unsigned_8_Array_3D (Data (index))));
      end loop;

      return Tuple_3D;

   end To_Tuple;

   --  -------------------------------------------------------------------------

end U8_Tuple_Builder;
