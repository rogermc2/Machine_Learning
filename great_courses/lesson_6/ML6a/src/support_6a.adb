
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;

with Neural_Utilities;

package body Support_6A is

   Unknown  : constant String := "@unk";
   Lex_Size : constant String := "@size";

   --  -------------------------------------------------------------------------

   function Get_Data (File_Name : String; Dictionary : ML_Types.String_Map)
                      return Data_Record is
      --        Routine_Name : constant String := "Support_6A.Get_Data ";
      File_ID         : File_Type;
      Data            : Data_Record;
   begin
      Open (File_ID, In_File, File_Name);
      while not End_Of_File (File_ID) loop
         declare
            aLine : constant String := Get_Line (File_ID);
            Label : constant Integer := Integer'Value (aLine (1 .. 1));
            Token : constant Integer_Array :=
                      Tokenize (aLine (3 .. aLine'Last), Dictionary);
         begin
            Data.Labels.Append (Label);
            Data.Features.Append (Token);
         end;
      end loop;
      Close (File_ID);

      return Data;

   end Get_Data;

   --  -------------------------------------------------------------------------

   function Read_Vocabulary (File_Name : String) return ML_Types.String_Map is
      --        Routine_Name : constant String := "Support_6A.Read_Vocab ";
      File_ID         : File_Type;
      Lexicon_Size    : Positive := 1;  --  Token
      Word_Dictionary : ML_Types.String_Map;
   begin
      Word_Dictionary.Insert (Unknown, Lexicon_Size);

      Open (File_ID, In_File, File_Name);

      while not End_Of_File (File_ID) loop
         declare
            aLine : constant String := Get_Line (File_ID);
            Count : constant Positive := Integer'Value (aLine (1 .. 4));
            Token : constant String := aLine (6 .. aLine'Last);
         begin
            if Count > 1 then
               Word_Dictionary.Include (Token, Lexicon_Size);
               Lexicon_Size := Lexicon_Size + 1;
            end if;
            --              Put_Line (Routine_Name & aLine);
         end;
      end loop;
      Close (File_ID);

      Word_Dictionary.Insert (Lex_Size, Lexicon_Size);

      return Word_Dictionary;

   end Read_Vocabulary;

   --  -------------------------------------------------------------------------

   function Tokenize (Data : String; Dictionary : ML_Types.String_Map)
                      return Integer_Array is
      use Neural_Utilities;
      use ML_Types.String_Package;
      Routine_Name : constant String := "Support_6A.Tokenize ";
      Words        : ML_Types.String_List;
      Word_Cursor  : Cursor;
      Index        : Positive;
      Vec          : Integer_Array (1 .. Dictionary (Lex_Size)) :=
                       (others => 0);
   begin
      Words := Split_String_On_Spaces (Data);
      Word_Cursor := Words.First;
      while Has_Element (Word_Cursor) loop
         declare
            Word : constant String := To_String (Element (Word_Cursor));
         begin
            if Dictionary.Contains (Word) then
               Put_Line (Routine_Name & "known word: '" & Word & "'");
               Index := Dictionary.Element (Word);
               Vec (Index) := Vec (Index) + 1;
            else
               Put_Line (Routine_Name & "unknown word: '" & Word & "'");
               Index := Dictionary.Element (Unknown);
               Vec (Index) := Vec (Index) + 1;
            end if;
         end;
         Next  (Word_Cursor);
      end loop;

      return Vec;

   end Tokenize;

   --  -------------------------------------------------------------------------

   function Word_List  (Dictionary : ML_Types.String_Map)
                        return ML_Types.Unbounded_List is
      use ML_Types;
      use ML_Types.String_Map_Package;
      --        Routine_Name : constant String := "Support_6A.Word_List ";
      Curs  : Cursor := Dictionary.First;
      Words : Unbounded_List;
      --        Count : Natural := 0;
   begin
      while Has_Element (Curs) loop
         --           Count := Count + 1;
         declare
            aWord : constant String := Key (Curs);
         begin
            --              if aWord'Length > 1 and aWord /= "" and aWord /= " " then
            --                 if Count < 6 then
            --                    Put_Line (Routine_Name & "aWord length: " & Integer'Image (Count) &
            --                               ", " & Integer'Image (aWord'Length));
            --                    Put_Line (Routine_Name & "aWord: " & Integer'Image (Count) &
            --                               ", '" & aWord & "'");
            --                 end if;
            Words.Append (To_Unbounded_String (aWord));
            --              end if;
         end;
         Next (Curs);
      end loop;

      return Words;

   end Word_List;

   --  -------------------------------------------------------------------------

end Support_6A;
