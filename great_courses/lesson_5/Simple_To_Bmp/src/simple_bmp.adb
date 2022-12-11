
with GID;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Simple_Support; use Simple_Support;

procedure Simple_BMP is

   --  Image used as background for displaying images having transparency
--     default_bkg_name      : constant String := "../gid.gif";
   default_bkg_name      : constant String := "../greenML.png";
--     test_only             : constant Boolean := False;
--     as_background         : constant Boolean := False;
   background_image_name : Unbounded_String := Null_Unbounded_String;
begin
    Put_Line ("To_BMP, using GID version " & GID.version & " dated " & GID.reference);
    Process (default_bkg_name, background_image_name);

end Simple_BMP;
