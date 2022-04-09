
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Text_IO.Unbounded_IO;

with GNAT.Regpat;
with GNAT.String_Split;

--  From Ada-utils
with Util.Serialize.IO.CSV;
with Util.Serialize.Mappers;

--  with Printing;
with Regexep;

package body Dataset_Utilities is

   Regex_CSV      : constant String := "[^,]+";
   Matcher_CSV    : constant GNAT.Regpat.Pattern_Matcher :=
                      GNAT.Regpat.Compile (Regex_CSV);
   Num_Parens_CSV : constant Natural := GNAT.Regpat.Paren_Count (Matcher_CSV);

   --  ------------------------------------------------------------------------

   procedure CSV_Reader (CSV_File_Name : String;
                         Data          : out String_List) is
      use Util.Serialize.IO.CSV;
      type CSV_Parser is new Util.Serialize.IO.CSV.Parser with null record;

      Prev_Row       : Row_Type;
      Data_File_Name : constant String := "data";
      Data_File      : File_Type;

      overriding
      procedure Set_Cell (Parser : in out CSV_Parser;
                          Value  : in String;
                          Row    : in Util.Serialize.IO.CSV.Row_Type;
                          Column : in Util.Serialize.IO.CSV.Column_Type);

      overriding
      procedure Set_Cell (Parser : in out CSV_Parser;
                          Value  : in String;
                          Row    : in Util.Serialize.IO.CSV.Row_Type;
                          Column : in Util.Serialize.IO.CSV.Column_Type) is
         pragma Unreferenced (Parser, Column);

      begin
         if Prev_Row /= Row then
            Ada.Text_IO.New_Line (Data_File);
            Prev_Row := Row;
         else
            Put (Data_File, " ");
         end if;
         Ada.Text_IO.Put (Data_File, Value);
      end Set_Cell;

      aLine          : Unbounded_String;
      Parser         : CSV_Parser;
      Mapper         : Util.Serialize.Mappers.Processing;
   begin
      pragma Unreferenced (aLine);

      if not Ada.Directories.Exists (Data_File_Name) then
         Create (Data_File, Out_File, Data_File_Name);
      else
         Open (Data_File, Out_File, Data_File_Name);
      end if;

      Prev_Row := Row_Type'Last;
      Parser.Parse (CSV_File_Name, Mapper);
      Close (Data_File);

      Data.Clear;
      Open (Data_File, In_File, Data_File_Name);
      while not End_Of_File (Data_File) loop
         Data.Append (To_Unbounded_String (Get_Line (Data_File)));
      end loop;
      Close (Data_File);
      pragma Unreferenced (Data_File);

   end CSV_Reader;

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

   function Split (Line : String; Sep : String)
                   return GNATCOLL.Strings.XString_Array is
      use GNATCOLL.Strings;
      Text     : constant XString := To_XString (Line);
      Elements : constant XString_Array :=
                   Split (Text, Sep, Omit_Empty => True);
   begin
      return Elements;

   end Split;

   --  -------------------------------------------------------------------------

   function Get_CSV_Data (CSV_Data : String) return Indef_String_List is
      use GNAT.Regpat;
      use Regexep;
      --        Routine_Name        : constant String := "Dataset_Utilities.Get_CSV_Data";
      Group_Index         : constant Natural := 0;
      Line_Last           : constant Positive := CSV_Data'Last;
      Groups              : Match_Array (0 .. Num_Parens_CSV);
      Matches             : Matches_List;
      --        pragma Unreferenced (Matches);
      First               : Positive := CSV_Data'First;
      Last                : Positive := CSV_Data'Last;
      Result              : Match_Location;
      Match_Found         : Boolean;
      Slices              : Indef_String_List;
   begin
      while First < Line_Last loop
         --  Match selects the first substring of Text that matches
         --  the Compiled_Expression
         Match (Matcher_CSV, CSV_Data (First .. Line_Last), Groups);
         Match_Found := Groups (0) /= No_Match;

         if Match_Found then
            for index in 0 .. Num_Parens_CSV loop
               Matches.Append (Groups (index));
            end loop;

            Result := Groups (Group_Index);
            First := Result.First;
            Last := Result.Last;
            Slices.Append (CSV_Data (First .. Last));
         end if;
         First := Last + 1;
      end loop;

      return Slices;

   end Get_CSV_Data;

   --  -------------------------------------------------------------------------

   function Split_String (aString, Pattern : String) return String_List is
      use Ada.Strings;
      --        Routine_Name : constant String := "Dataset_Utilities.Split_String ";
      Patt_Length  : constant Integer := Pattern'Length;
      Last         : Integer := aString'Last;
      Last_Char    : constant Character := aString (Last);
      A_Index      : Integer := 1;
      B_Index      : Integer := aString'First;
      Split_List   : String_List;
   begin
      if Character'Pos (Last_Char) < 32 then
         Last := Last - 1;
      end if;

      for index in aString'First .. Fixed.Count (aString, Pattern) loop
         A_Index :=
           Fixed.Index (aString (B_Index .. Last), Pattern);
         --  process string slice in any way
         Split_List.Append
           (To_Unbounded_String (aString (B_Index .. A_Index - 1)));
         B_Index := A_Index + Patt_Length;
      end loop;
      --  process last string
      Split_List.Append
        (To_Unbounded_String (aString (B_Index .. Last)));

      return Split_List;

   end Split_String;

   --  -------------------------------------------------------------------------

   function Split_String (aString, Pattern : String) return Indef_String_List is
      use Ada.Strings;
      --        Routine_Name : constant String := "Dataset_Utilities.Split_String ";
      Patt_Length  : constant Integer := Pattern'Length;
      Last         : Integer := aString'Last;
      Last_Char    : constant Character := aString (Last);
      A_Index      : Integer := 1;
      B_Index      : Integer := aString'First;
      Split_List   : Indef_String_List;
   begin
      if Character'Pos (Last_Char) < 32 then
         Last := Last - 1;
      end if;

      for index in aString'First .. Fixed.Count (aString, Pattern) loop
         A_Index :=
           Fixed.Index (aString (B_Index .. Last), Pattern);
         --  process string slice in any way
         Split_List.Append (aString (B_Index .. A_Index - 1));
         B_Index := A_Index + Patt_Length;
      end loop;
      --  process last string
      Split_List.Append (aString (B_Index .. Last));

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

   function To_Upper_Case (Text : Unbounded_String) return Unbounded_String is
      UC : constant String := To_String (Text);
   begin

      return To_Unbounded_String (To_Upper_Case (UC));

   end To_Upper_Case;

   --  -------------------------------------------------------------------------

   function Trimmed_Integer (Value : Integer) return String is
      use Ada.Strings;
   begin
      return Fixed.Trim (Integer'Image (Value), Both);

   end Trimmed_Integer;

   --  -------------------------------------------------------------------------

end Dataset_Utilities;
