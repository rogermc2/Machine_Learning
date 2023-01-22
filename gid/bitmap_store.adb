
--  Derived from https://rosettacode.org/wiki/Bitmap#Ada

with Ada.Text_IO;  use Ada.Text_IO;

--  This can be used like:
--
--  use Bitmap_Store;  with Bitmap_Store;
--     ...
--     X : Image (1..64, 1..64);
--  begin
--     Fill (X, (255, 255, 255));
--     X (1, 2) := (R => 255, others => 0);
--     X (3, 4) := X (1, 2);

package body Bitmap_Store is

   procedure Fill (Picture : in out Image; Color : Pixel) is
   begin
      for p of Picture loop
         x:= Color;
      end loop;
   end Fill;

   procedure Print (Picture : Image) is
   begin
      for I in Picture'Range (1) loop
         for J in Picture'Range (2) loop
               Put (if Picture (I, J) = White then ' ' else 'H');
         end loop;
         New_Line;
      end loop;
   end Print;

end Bitmap_Store;
