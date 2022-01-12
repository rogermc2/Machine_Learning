
with Ada.Characters.Handling;

with GNAT.String_Split;

package body Dataset_Utilities is

   function Split (Line : String; Sep : String) return String_Array is
      use GNAT.String_Split;
      Tokens : Slice_Set;
   begin
      Create (S => Tokens, From => Line, Separators => Sep,
              Mode => GNAT.String_Split.Multiple);

      declare
         Slices : String_Array
           (1 .. Natural (Slice_Count (Tokens)));
      begin
         for index in Slices'Range loop
            Slices (index) :=
              Ada.Strings.Unbounded.To_Unbounded_String
                (GNAT.String_Split.Slice
                   (Tokens, GNAT.String_Split.Slice_Number (index)));
         end loop;

         return Slices;
      end;

   end Split;

   --  -------------------------------------------------------------------------

   function To_Lower_Case (Text : String) return String is
      use Ada.Characters.Handling;
      LC : String := Text;
   begin
      for char in Text'First .. Text'Last loop
         if Is_Upper (Text (char)) and then Text (char) in 'A' .. 'Z' then
            LC (char) := To_Lower (Text (char));
         end if;
      end loop;

      return LC;

   end To_Lower_Case;

   --  -------------------------------------------------------------------------

   function To_Upper_Case (Text : String) return String is
      use Ada.Characters.Handling;
      UC : String := Text;
   begin
      for char in Text'First .. Text'Last loop
         if Is_Lower (Text (char)) and then Text (char) in 'a' .. 'z' then
            UC (char) := To_Upper (Text (char));
         end if;
      end loop;

      return UC;

   end To_Upper_Case;

end Dataset_Utilities;
