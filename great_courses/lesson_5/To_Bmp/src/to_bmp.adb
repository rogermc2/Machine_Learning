
with GID;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with BMP_Support; use BMP_Support;

procedure To_BMP is

   --  Image used as background for displaying images having transparency
--     default_bkg_name      : constant String := "src/gid.gif";
   default_bkg_name      : constant String := "src/greenML.png";
   test_only             : constant Boolean := False;
   as_background         : constant Boolean := False;
   background_image_name : Unbounded_String := Null_Unbounded_String;
begin
    Put_Line (Standard_Error, "To_BMP, using GID version " & GID.version &
               " dated " & GID.reference);
    Process (default_bkg_name, as_background, test_only, background_image_name);

end To_BMP;
