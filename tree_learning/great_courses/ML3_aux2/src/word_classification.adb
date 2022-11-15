
--  with Ada.Text_IO;

package body Word_Classification is

   function Get_Features (aLine : Unbounded_String)
                          return ML_Types.Unbounded_List is
      Line_Length : constant Positive := Length (aLine);
      Pos         : Natural := Index (aLine, "ie");
      Features    : ML_Types.Unbounded_List;
   begin

      while Pos < Line_Length loop
         Pos := Pos + 1;
      end loop;

      return Features;

   end Get_Features;

   --  -------------------------------------------------------------------------

end Word_Classification;
