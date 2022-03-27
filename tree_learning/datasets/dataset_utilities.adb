
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Streams.Stream_IO;
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
                         Data          : out ML_Types.String_List) is
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

   function Get_CSV_Data (CSV_Data : String) return ML_Types.Indef_String_List is
      use ML_Types;
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

   procedure Read_JSON_Array (File_Name : String;
                              theArray  : in out GNATCOLL.JSON.JSON_Array) is
      use Ada.Streams;
      use Stream_IO;
      use GNATCOLL.JSON;
      Routine_Name : constant String := "Dataset_Utilities.Read_JSON_Array ";
      File_ID     : Stream_IO.File_Type;
      Data_Stream : Stream_IO.Stream_Access;
   begin
      Open (File_ID, In_File, File_Name);
      Data_Stream := Stream (File_ID);
      Put_Line (Routine_Name & File_Name & " opened" );

      while not Stream_IO.End_Of_File (File_ID) loop
         Put_Line (Routine_Name & " read" );
         JSON_Array'Read (Data_Stream, theArray);
--           JSON_Value'Read (Data_Stream, aValue);
--           if not Is_Empty (aValue) then
--              Put_Line (Routine_Name & " aValue");
--              Put_Line (aValue.Write);
--              Append (theArray, aValue);
--           end if;
      end loop;

      Close (File_ID);
      pragma Unreferenced (File_ID);
      Put_Line (Routine_Name & "JSON array loaded.");

   end Read_JSON_Array;

   --  -------------------------------------------------------------------------

   function Split_String (aString, Pattern : String)
                          return ML_Types.String_List is
      use Ada.Strings;
      --        Routine_Name : constant String := "Dataset_Utilities.Split_String ";
      Patt_Length  : constant Integer := Pattern'Length;
      Last         : Integer := aString'Last;
      Last_Char    : constant Character := aString (Last);
      A_Index      : Integer := 1;
      B_Index      : Integer := aString'First;
      Split_List   : ML_Types.String_List;
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

   function Split_String (aString, Pattern : String)
                          return ML_Types.Indef_String_List is
      use Ada.Strings;
      --        Routine_Name : constant String := "Dataset_Utilities.Split_String ";
      Patt_Length  : constant Integer := Pattern'Length;
      Last         : Integer := aString'Last;
      Last_Char    : constant Character := aString (Last);
      A_Index      : Integer := 1;
      B_Index      : Integer := aString'First;
      Split_List   : ML_Types.Indef_String_List;
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

   procedure Write_JSON_Array_To_File (Data_Array : GNATCOLL.JSON.JSON_Array;
                                       File_Name : String) is
      use Ada.Streams;
      use Stream_IO;
      use GNATCOLL.JSON;
      Routine_Name : constant String :=
                        "Dataset_Utilities.Write_JSON_Array_To_File ";
      File_ID     : Stream_IO.File_Type;
      Data_Stream : Stream_Access;
--        Index       : Positive := Array_First (Data_Array);
--        aValue      : JSON_Value;
   begin
      if Ada.Directories.Exists (File_Name) then
         Stream_IO.Open (File_ID, In_File, File_Name);
         Stream_IO.Delete (File_ID);
      end if;

      Create (File_ID, Out_File, File_Name);
      Data_Stream := Stream (File_ID);
      Put_Line (Routine_Name & "writing to " & File_Name);
--        while Array_Has_Element (Data_Array, Index) loop
--           aValue := Array_Element (Data_Array, Index);
         JSON_Array'Write (Data_Stream, Data_Array);
--           Index := Array_Next (Data_Array, Index);
--        end loop;
      Put_Line (Routine_Name & File_Name & " written.");

      Stream_IO.Close (File_ID);
      pragma Unreferenced (File_ID);

   end Write_JSON_Array_To_File;

   --  -------------------------------------------------------------------------

end Dataset_Utilities;