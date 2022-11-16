
with Ada.Strings.Fixed;
--  with Ada.Text_IO;

with Utilities;

package body Word_Classification is

   function Get_Features (Word_Line : ML_Types.String_List)
                          return ML_Types.String_List;

   --  -------------------------------------------------------------------------

   procedure Build_Dataset (IE_Data, EI_Data : ML_Types.Unbounded_List) is
      aLine     : Unbounded_String;
      Word_Line : ML_Types.String_List;
      Features  : ML_Types.String_List;
   begin
      for index in IE_Data.First_Index .. IE_Data.Last_Index loop
         aLine := IE_Data (index);
         Word_Line := Utilities.Split_String_On_Spaces (To_String (aLine));
         Features := Get_Features (Word_Line);
      end loop;

   end Build_Dataset;

   --  -------------------------------------------------------------------------

   function Get_Features (Word_Line : ML_Types.String_List)
                          return ML_Types.String_List is
      use Ada.Strings.Fixed;
      use ML_Types;
      use String_Package;
      Curs   : Cursor := Word_Line.First;
      Pos    : Natural;
      Result : String_List;
   begin
      declare
         aWord : String := To_String (Word_Line.First_Element);
      begin
         Pos := Index (aWord, "ie");

      end;  --  declare block

      return Result;

   end Get_Features;

   --  -------------------------------------------------------------------------

end Word_Classification;
