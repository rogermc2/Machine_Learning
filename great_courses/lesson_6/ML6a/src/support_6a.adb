
with Ada.Assertions; use Ada.Assertions;
--  with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Neural_Utilities;
with Python_CLF;

package body Support_6A is

   Lex_Size    : constant Unbounded_String := To_Unbounded_String ("@size");
   Unknown     : constant Unbounded_String := To_Unbounded_String ("@unk");
   Num_Known   : Natural := 0;
   Num_Unknown : Natural := 0;

   function Find_Item
     (Dictionary : Dictionary_List; Key : Unbounded_String;
      Item       : out Dictionary_Record) return Boolean is
      use Dictionary_Package;
      --        Routine_Name : constant String := "Support_6A.Find_Item ";
      Curs  : Cursor := Dictionary.First;
      Found : Boolean := False;

   begin
      while Has_Element (Curs) and not Found loop
         Item := Element (Curs);
         Found := Item.Key = Key;
         Next (Curs);
      end loop;

      return Found;

   end Find_Item;

   --  -------------------------------------------------------------------------

   function Get_Data (File_Name : String; Dictionary : Dictionary_List)
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

   procedure Plot_Sentence (Classifier : Python.Module;
                            CLF        : Python_API.PyObject;
                            Word_Dict  : Dictionary_List;
                            Sentence   : ML_Types.Indef_String_List;
                            Facs       : out Real_Float_List;
                            Labels     : out ML_Types.Indef_String_List) is
      use Maths.Float_Math_Functions;
      use ML_Types.Indefinite_String_Package;
      use Python_CLF;
      Routine_Name     : constant String := "Support_6A.Plot_Sentence ";
      Class_Log_Prior  : constant Python_API.PyObject :=
                           Get_Attribute (CLF, "class_log_prior_");
      Feature_Log_Prob : constant Python_API.PyObject :=
                           Get_Attribute (CLF, "feature_log_prob_");
      Curs             : Cursor := Sentence.First;
      Acc              : Float := 1.0;
      Log_Prior_0      : constant Float :=
                           Call (Classifier, "array_item", Class_Log_Prior, 0);
      Log_Prior_1      : constant Float :=
                           Call (Classifier, "array_item", Class_Log_Prior, 1);
      Log_Prob_0       : Float;
      Log_Prob_1       : Float;
      Factor           : Float := Exp (Log_Prior_0 - Log_Prior_1);
   begin
      Labels.Append ("PRIOR");
      Facs.Append (Factor);
      Acc := Acc * Factor;

      while Has_Element (Curs) loop
         declare
            Word  : constant String := Sentence (Curs);
            Item  : Dictionary_Record;
            Index : Natural;
         begin
            Assert (Find_Item (Word_Dict, To_Unbounded_String (Word), Item),
                    Routine_Name & Word & " is not in the dictionary");
            Labels.Append (Word);
            Index := Item.Value;
            Log_Prob_0 :=
              Call (Classifier, "matrix_item", Feature_Log_Prob,
                    0, Index);
            Log_Prob_1 :=
              Call (Classifier, "matrix_item", Feature_Log_Prob,
                    1, Index);
            Factor := Exp (Log_Prob_0 - Log_Prob_1);
            Facs.Append (Factor);
            Acc := Acc * Factor;
         end;

         Next (Curs);
      end loop;

      Labels.Append ("POST");
      Facs.Append (Acc);

   end Plot_Sentence;

   --  -------------------------------------------------------------------------

   procedure Print_Bayes_Data
     (Classifier : Python.Module; CLF : Python_API.PyObject;
      Word_Dict  : Dictionary_List; Sentence : ML_Types.Indef_String_List) is
      use ML_Types.Indefinite_String_Package;
--        Routine_Name : constant String := "Support_6A.Print_Bayes_Data ";
      Label_Cursor : Cursor;
      Facs         : Real_Float_List;
      Labels       : ML_Types.Indef_String_List;
      Index        : Natural := 0;
   begin
      Plot_Sentence (Classifier, CLF, Word_Dict, Sentence, Facs, Labels);
      for fac in Facs.First_Index .. Facs.Last_Index loop
         if Facs (fac) < 1.0 then
            Facs.Replace_Element (fac, -1.0 / Facs (fac));
         end if;
      end loop;

      New_Line;
      Put_Line ("Naive Bayes factors:");
      Label_Cursor := Labels.First;
      Index := 0;
      while Has_Element (Label_Cursor) loop
         Index := Index + 1;
         Put_Line (Element (Label_Cursor) & ", " &
                     Float'Image (abs (Facs (Index))));
         Next (Label_Cursor);
      end loop;
      New_Line;

   end Print_Bayes_Data;

   --  -------------------------------------------------------------------------

   function Read_Vocabulary (File_Name : String) return Dictionary_List is
      --        Routine_Name    : constant String := "Support_6A.Read_Vocabulary ";
      File_ID         : File_Type;
      Lexicon_Size    : Natural := 0;  --  Token
      Word_Dictionary : Dictionary_List;
      Item            : Dictionary_Record;
   begin
      Item := (Unknown, Lexicon_Size);
      Word_Dictionary.Append (Item);
      Lexicon_Size := Lexicon_Size + 1;

      Open (File_ID, In_File, File_Name);

      while not End_Of_File (File_ID) loop
         declare
            aLine : constant Unbounded_String :=
                      To_Unbounded_String (Get_Line (File_ID));
            Count : constant Positive := Integer'Value (Slice (aLine, 1, 4));
            Token : constant Unbounded_String :=
                      To_Unbounded_String
                        (Slice (aLine, 6, Length (aLine) - 1));
         begin
            if Count > 1 then
               Item :=  (Token, Lexicon_Size);
               Word_Dictionary.Append (Item);
               Lexicon_Size := Lexicon_Size + 1;
            end if;
         end;
      end loop;

      Close (File_ID);

      Item :=  (Lex_Size, Lexicon_Size);
      Word_Dictionary.Append (Item);

      return Word_Dictionary;

   end Read_Vocabulary;

   --  -------------------------------------------------------------------------

   function Tokenize (Data : String; Dictionary : Dictionary_List)
                      return Integer_Array is
      use Neural_Utilities;
      use ML_Types;
      use String_Package;
      --        Routine_Name : constant String := "Support_6A.Tokenize ";
      Words        : ML_Types.String_List;
      Word_Cursor  : String_Package.Cursor;
      Index        : Natural;
      Item         : Dictionary_Record;
      Vec          : Integer_Array (0 .. Positive (Dictionary.Length) - 1) :=
                       (others => 0);
      Word         : Unbounded_String;
      Dummy        : Boolean;
   begin
      Words := Split_String_On_Spaces (Data);
      Word_Cursor := Words.First;
      while Has_Element (Word_Cursor) loop
         Word := Element (Word_Cursor);
         if Find_Item (Dictionary, Word, Item) then
            Num_Known := Num_Known + 1;
            Index := Item.Value;
         else
            Num_Unknown := Num_Unknown + 1;
            Dummy := Find_Item (Dictionary, Unknown, Item);
            Index := Item.Value;
         end if;

         Vec (Index) := Vec (Index) + 1;
         Next  (Word_Cursor);
      end loop;

      return Vec;

   end Tokenize;

   --  -------------------------------------------------------------------------

   function Word_List  (Dictionary : Dictionary_List)
                        return ML_Types.Indef_String_List is
      use ML_Types;
      use Dictionary_Package;
      --        Routine_Name : constant String := "Support_6A.Word_List ";
      Curs  : Cursor := Dictionary.First;
      Words : Indef_String_List;
      Item  : Dictionary_Record;
   begin
      while Has_Element (Curs) loop
         if Curs /= Dictionary.Last then
            Item := Element (Curs);
            Words.Append (To_String (Item.Key));
         end if;
         Next (Curs);
      end loop;

      return Words;

   end Word_List;

   --  -------------------------------------------------------------------------

end Support_6A;
