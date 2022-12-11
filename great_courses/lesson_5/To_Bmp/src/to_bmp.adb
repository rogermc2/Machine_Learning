
with GID;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with BMP_Support; use BMP_Support;

procedure To_BMP is

   default_bkg_name      : constant String := "src/gid.gif";
   background_image_name : Unbounded_String := Null_Unbounded_String;
   test_only             : Boolean := False;

begin
   if Argument_Count = 0 then
      Blurb;
      return;
   end if;

   Put_Line (Standard_Error, "To_BMP, using GID version " & GID.version &
               " dated " & GID.reference);
   begin
      Process (default_bkg_name, True, False, background_image_name);

   exception
      when Ada.Text_IO.Name_Error =>
         null; -- nothing bad, just couldn't find default background
   end;

   for i in 1 .. Argument_Count loop
      declare
         arg : constant String := Argument (i);
      begin
         if arg /= "" and then arg (arg'First) = '-' then
            declare
               opt : constant String := arg (arg'First + 1 .. arg'Last);
            begin
               if opt = "" then
                  test_only := True;
               else
                  Put_Line (Standard_Error, "Background image is " & opt);
                  Process (opt, True, False, background_image_name);
                  --  define this only after processing, otherwise
                  --  a transparent background will try to use
                  --  an undefined background
                  background_image_name := To_Unbounded_String (opt);
               end if;
            end;
         else
            Process (arg, False, test_only, background_image_name);
         end if;
      end;  --  declare
   end loop;

end To_BMP;
