
with Ada.Assertions; use Ada.Assertions;
--  with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Neural_Utilities;
with Python_CLF;

package body Support_6A is

   Lex_Size    : constant String := "@size";
   Unknown     : constant String := "@unk";
   Num_Known   : Natural := 0;
   Num_Unknown : Natural := 0;

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

      --        Put_Line (Routine_Name & "Number of words occurring more than once: " &
      --                    Integer'Image (Num_Known));
      --        Put_Line (Routine_Name & "Number of words occurring only once: " &
      --                    Integer'Image (Num_Unknown));

      return Data;

   end Get_Data;

   --  -------------------------------------------------------------------------

   procedure Plot_Sentence (CLF        : Python_API.PyObject;
                            Word_Dict  : ML_Types.String_Map;
                            Sentence   : ML_Types.Indef_String_List;
                            Facs       : out Real_Float_List;
                            Labels     : out ML_Types.Indef_String_List) is
      use Maths.Float_Math_Functions;
      use ML_Types.Indefinite_String_Package;
      Class_Log_Prior  : constant Float_Array :=
                           Python_CLF.Get_Attribute (CLF, "class_log_prior_");
      Feature_Log_Prob : constant Real_Float_Matrix :=
                           Python_CLF.Get_Attribute (CLF, "feature_log_prob_");
      Curs             : Cursor := Sentence.First;
      Acc              : Float := 1.0;
      Index            : Natural;
      Factor           : Float;
   begin
      Labels.Append ("PRIOR");
      Facs.Append (Exp (Class_Log_Prior (1) - Class_Log_Prior (2)));

      while Has_Element (Curs) loop
         Index := Word_Dict (Sentence (Curs));
         Labels.Append (Sentence (Curs));
         Factor := Exp (Feature_Log_Prob (1, Index) - Feature_Log_Prob (2, Index));
         Acc := Acc * Factor;
         Facs.Append (Factor);
         Next (Curs);
      end loop;

      Labels.Append ("POST");
      Facs.Append (Acc);

   end Plot_Sentence;

   --  -------------------------------------------------------------------------

   function Read_Vocabulary (File_Name : String) return ML_Types.String_Map is
      Routine_Name    : constant String := "Support_6A.Read_Vocabulary ";
      File_ID         : File_Type;
      Lexicon_Size    : Positive := 1;  --  Token
      Word_Dictionary : ML_Types.String_Map;
   begin
      Word_Dictionary.Include (Unknown, Lexicon_Size);

      Open (File_ID, In_File, File_Name);

      while not End_Of_File (File_ID) loop
         declare
            aLine : constant String := Get_Line (File_ID);
            Count : constant Positive := Integer'Value (aLine (1 .. 4));
            Token : constant String (1 .. aLine'Length - 6) :=
                      aLine (aLine'First + 5 .. aLine'Length - 1);
         begin
            if Count > 1 then
               Word_Dictionary.Include (Token, Lexicon_Size);
               Lexicon_Size := Lexicon_Size + 1;
               Assert (Word_Dictionary.Contains (Token), Routine_Name &
                         "Word_Dictionary entry failed: '" & Token & "', " &
                         "length: " & Integer'Image (Token'Length));
            end if;
         end;
      end loop;

      Close (File_ID);

      Word_Dictionary.Include (Lex_Size, Lexicon_Size);
      New_Line;

      return Word_Dictionary;

   end Read_Vocabulary;

   --  -------------------------------------------------------------------------

   function Tokenize (Data : String; Dictionary : ML_Types.String_Map)
                      return Integer_Array is
      use Neural_Utilities;
      use ML_Types.String_Package;
      --        Routine_Name : constant String := "Support_6A.Tokenize ";
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
               Num_Known := Num_Known + 1;
               Index := Dictionary.Element (Word);
               Vec (Index) := Vec (Index) + 1;
            else
               Num_Unknown := Num_Unknown + 1;
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
                        return ML_Types.Indef_String_List is
      use ML_Types;
      use ML_Types.String_Map_Package;
      --        Routine_Name : constant String := "Support_6A.Word_List ";
      Curs  : Cursor := Dictionary.First;
      Words : Indef_String_List;
   begin
      while Has_Element (Curs) loop
         if Curs /= Dictionary.Last then
            Words.Prepend (Key (Curs));
         end if;
         Next (Curs);
      end loop;

      return Words;

   end Word_List;

   --  -------------------------------------------------------------------------

end Support_6A;
