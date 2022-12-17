
with PNG_To_BMP; use PNG_To_BMP;

package body Support_5A is

   function Get_Part
     (Image                     : Unsigned_8_Array_3D;
      First_Row, Last_Row       : Positive;
      First_Column, Last_Column : Positive) return Unsigned_8_Array_3D is
      Part      : Unsigned_8_Array_3D
        (1 .. Last_Row - First_Row, 1 .. Last_Column - First_Column, Image'Range (3));
      Image_Row : Natural := First_Row - 1;
      Image_Col : Natural;
   begin
      for row in Part'Range loop
         Image_Row := Image_Row + 1;
         Image_Col := First_Column - 1;
         for col in Part'Range (2) loop
            Image_Col := Image_Col + 1;
            for pix in Part'Range (3) loop
               Part (row, col, pix) := Image (row, col, pix);
            end loop;
         end loop;
      end loop;

      return Part;

   end Get_Part;

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
