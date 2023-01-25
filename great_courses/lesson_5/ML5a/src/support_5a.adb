
with Ada.Assertions; use Ada.Assertions;

with PNG_To_BMP; use PNG_To_BMP;

package body Support_5A is

   function Get_Pixels
     (Image                     : Unsigned_8_Array_3D;
      First_Row, Last_Row       : Positive;
      First_Column, Last_Column : Positive) return Unsigned_8_Array_3D is
      Routine_Name : constant String := "Support_5A.Get_Pixels ";
      Part         : Unsigned_8_Array_3D
        (1 .. Last_Row - First_Row + 1, 1 .. Last_Column - First_Column + 1,
         Image'Range (3));
      Part_Row     : Natural := 0;
      Part_Col     : Natural;
   begin
      Assert (Last_Row <= Image'Last, Routine_Name & "invalid Last_Row.");
      Assert (Last_Column <= Image'Last (2), Routine_Name & "invalid Last_Column.");
      for row in First_Row .. Last_Row loop
         Part_Row := Part_Row + 1;
         Part_Col := 0;
         for col in First_Column .. Last_Column loop
            Part_Col := Part_Col + 1;
            for pix in Image'Range (3) loop
               Part (Part_Row, Part_Col, pix) := Image (row, col, pix);
            end loop;
         end loop;
      end loop;

      return Part;

   end Get_Pixels;

   --  -------------------------------------------------------------------------

   function Get_Picture (File_Name : String) return Unsigned_8_Array_3D is
      Initial : constant Unsigned_8_Array_3D :=
                  Unsigned_8_Array_3D (Process (File_Name));
      Clipped : Unsigned_8_Array_3D
        (1 .. Initial'Length - 14, Initial'Range (2), Initial'Range (3));
   begin
      for row in Clipped'Range loop
         for col in Clipped'Range (2) loop
            for pix in Clipped'Range (3) loop
               Clipped (row, col, pix) := Initial (row, col, pix);
            end loop;
         end loop;
      end loop;

      return Clipped;

   end Get_Picture;

   --  -------------------------------------------------------------------------

   function Set_All_Data (Yes_List, No_List : Integer_Matrix)
                          return Integer_Matrix is
      All_Data              : constant Integer_Matrix := Yes_List & No_List;
      All_Data_With_Offset  :  Integer_Matrix (All_Data'Range ,
                                               1 .. All_Data'Length (2) + 1);
   begin
      for row in All_Data_With_Offset'Range loop
         for col in All_Data_With_Offset'Range (2) loop
            if col <= All_Data'Length (2) then
               All_Data_With_Offset (row, col) := All_Data (row, col);
            else
               All_Data_With_Offset (row, col) := 1;
            end if;
         end loop;
      end loop;

      return All_Data_With_Offset;

   end Set_All_Data;

   --  -------------------------------------------------------------------------

   function To_2D (From : Unsigned_8_Array_3D) return Integer_Matrix is
      M2 : Integer_Matrix (1 .. From'Length * From'Length (2), From'Range (3));
   begin
      for row in From'Range loop
         for col in From'Range (2) loop
            for pix in From'Range (3) loop
               M2 ((row - 1) * From'Length (2) + col, pix) :=
                 Integer (From (row, col, pix));
            end loop;
         end loop;
      end loop;

      return M2;

   end To_2D;

   --  -------------------------------------------------------------------------

end Support_5A;
