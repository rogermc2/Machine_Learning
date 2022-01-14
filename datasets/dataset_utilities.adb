
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with GNAT.String_Split;

package body Dataset_Utilities is

   function Split_String (aString, Pattern : String) return String_List;

   --  -------------------------------------------------------------------------

   function Get_CSV_Data (CSV_Data : String) return String_List is
   begin
      return Split_String (CSV_Data, ",");

   end Get_CSV_Data;

   --  -------------------------------------------------------------------------

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

   function Split_String (aString, Pattern : String) return String_List is
      use Ada.Strings;
      Last       : constant Integer := aString'Last;
      Last_Char  : constant Character := aString (Last);
      UB_String  : Unbounded_String;
      Split_List : String_List;
   begin
      if Character'Pos (Last_Char) < 32 then
         UB_String :=
           To_Unbounded_String (aString (aString'First .. Last - 1));
      else
         UB_String := To_Unbounded_String (aString);
      end if;

      declare
         String_2 : constant String := To_String (UB_String);
         Last_2   : constant Integer := String_2'Last;
         A_Index  : Integer;
         B_Index  : Integer := String_2'First;
      begin
         for index in String_2'First .. Fixed.Count (String_2, Pattern) loop
            A_Index :=
              Fixed.Index (String_2 (B_Index .. Last_2), Pattern);
            --  process string slice in any way
            Split_List.Append
              (To_Unbounded_String (String_2 (B_Index .. A_Index - 1)));
            B_Index := A_Index + Pattern'Length;
         end loop;
         --  process last string
         Split_List.Append
           (To_Unbounded_String (String_2 (B_Index .. Last_2)));
      end;
      return Split_List;

   end Split_String;

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

   --  -------------------------------------------------------------------------

   function Trimmed_Integer (Value : Integer) return String is
      use Ada.Strings;
   begin
      return Fixed.Trim (Integer'Image (Value), Both);
   end Trimmed_Integer;

   --  -------------------------------------------------------------------------

end Dataset_Utilities;
