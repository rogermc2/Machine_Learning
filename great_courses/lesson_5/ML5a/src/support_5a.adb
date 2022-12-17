
with PNG_To_BMP; use PNG_To_BMP;

package body Support_5A is

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

end Support_5A;
